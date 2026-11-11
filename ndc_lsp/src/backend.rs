use ahash::AHashMap;

use lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, InlayHint, InlayHintParams, Location, MessageType, OneOf,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    WorkDoneProgressOptions,
};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result as JsonRPCResult;
use tower_lsp::{Client, LanguageServer};

use crate::document::{self, Configure};
use crate::features::completion::FunctionInfo;
use crate::features::{completion, definition, hover, inlay_hints, symbols};
use crate::state::DocumentState;

/// An open document: its state, and the client's version of the text in it.
struct OpenDocument {
    /// Monotonic document version from the client (LSP `didChange`). Used to
    /// discard the result of a `validate` that an overlapping later edit has
    /// already superseded, so a slow analysis can't roll the buffer back.
    version: i32,
    state: DocumentState,
}

pub struct Backend {
    pub client: Client,
    documents: RwLock<AHashMap<Url, OpenDocument>>,
    configure: Configure,
    /// Native-function metadata, snapshotted once at startup. The set of native
    /// functions never changes, so completion and hover read this instead of
    /// rebuilding an interpreter per request.
    functions: Vec<FunctionInfo>,
}

impl Backend {
    pub fn new(client: Client, configure: Configure) -> Self {
        Self {
            client,
            documents: RwLock::new(AHashMap::new()),
            configure,
            functions: FunctionInfo::collect(&document::interpreter(configure)),
        }
    }

    // Staleness & concurrency model
    // ------------------------------
    // `didChange` notifications can interleave at await points, so any cached
    // state is tagged with the client's monotonic document `version`. Two rules
    // keep async edits sound:
    //   1. `update_source` runs synchronously under the lock and bumps `version`,
    //      so completion (triggered by `.`) always sees the latest text.
    //   2. `validate` runs analysis off the lock, then commits/publishes only if
    //      the stored `version` still equals the one it analysed — a slow run
    //      can't roll the buffer back to older text.
    // AST-backed features (hover, go-to-def, symbols, inlay hints) additionally
    // gate on `analysis_matches_source`, so they never map stale spans onto
    // edited text. The completion caches (`variable_types` by name,
    // `expression_types` by end-offset) are intentionally resilient: dot
    // completion fires on a buffer that doesn't parse (`x.`), and the data it
    // reads sits at the cursor where offsets are stable for the appended `.`.

    /// Update the cached source text immediately and bump the document version.
    async fn update_source(&self, uri: &Url, text: &str, version: i32) {
        let mut docs = self.documents.write().await;
        if let Some(open) = docs.get_mut(uri) {
            // The AST now predates this edit; `validate` commits a new one if
            // the new source parses.
            open.state.set_source(text);
            open.version = version;
        } else {
            let state = DocumentState::from_source(text.to_string());
            docs.insert(uri.clone(), OpenDocument { version, state });
        }
    }

    /// Run diagnostics and semantic analysis for `version` of the document, then
    /// commit the analysis and publish diagnostics only if no later edit has
    /// superseded it. `update_source` must have run for this `version` first.
    async fn validate(&self, uri: &Url, text: &str, version: i32) {
        // The interpreter uses Rc internally (non-Send); `analyse` drops it
        // before returning, so nothing non-Send lives across the next await.
        let (diagnostics, analysed) = document::analyse(self.configure, text);

        // Hold the write lock across the commit AND the publish. `did_close`
        // also takes this lock to remove state + clear diagnostics, so holding
        // it here serializes the two: either close runs first (and we then see
        // the document gone and skip), or we publish first and close clears it
        // afterwards. Without this, a close could slip between an unlocked
        // version check and the publish, leaving stale diagnostics on a closed
        // file. The publish is a fire-and-forget notification, so the lock is
        // held only briefly.
        let mut docs = self.documents.write().await;
        let Some(open) = docs.get_mut(uri) else {
            return;
        };
        // A later edit already moved the buffer on — drop this stale run
        // rather than committing old AST or publishing old diagnostics.
        if open.version != version {
            return;
        }
        // On success, commit in place (keeps the current source/line_index).
        // On parse failure, keep the last good AST; `analysis_matches_source`
        // is already false, so AST-backed features stay disabled.
        if let Some((ast, analysis)) = analysed {
            open.state.set_analysis(ast, analysis);
        }

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> JsonRPCResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    completion_item: None,
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized")
            .await;
    }

    async fn shutdown(&self) -> JsonRPCResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;
        self.update_source(&uri, &text, version).await;
        self.validate(&uri, &text, version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        // Full-document sync: the last change carries the whole buffer, so only
        // the final one matters.
        if let Some(change) = params.content_changes.into_iter().next_back() {
            self.update_source(&uri, &change.text, version).await;
            self.validate(&uri, &change.text, version).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        // Drop cached state so the map doesn't grow unbounded, and clear the
        // document's diagnostics. Hold the lock across the publish so it can't
        // interleave with a `validate` publish (see the note in `validate`).
        let mut docs = self.documents.write().await;
        docs.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> JsonRPCResult<Option<Vec<InlayHint>>> {
        let docs = self.documents.read().await;
        Ok(docs
            .get(&params.text_document.uri)
            .map(|open| &open.state)
            .map(|state| {
                if !state.analysis_matches_source {
                    return Vec::new();
                }
                inlay_hints::collect(
                    &state.ast,
                    &state.analysis,
                    &state.source,
                    &state.line_index,
                )
            }))
    }

    async fn hover(&self, params: HoverParams) -> JsonRPCResult<Option<Hover>> {
        let docs = self.documents.read().await;
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(docs
            .get(uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)
            .and_then(|state| hover::hover(state, position, &self.functions)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> JsonRPCResult<Option<GotoDefinitionResponse>> {
        let docs = self.documents.read().await;
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(docs
            .get(uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)
            .and_then(|state| {
                definition::goto_definition(state, position).map(|range| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range,
                    })
                })
            }))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> JsonRPCResult<Option<DocumentSymbolResponse>> {
        let docs = self.documents.read().await;
        Ok(docs
            .get(&params.text_document.uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)
            .map(|state| {
                DocumentSymbolResponse::Nested(symbols::document_symbols(
                    &state.ast,
                    &state.analysis,
                    &state.source,
                    &state.line_index,
                ))
            }))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> JsonRPCResult<Option<CompletionResponse>> {
        let docs = self.documents.read().await;
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        // Completion is synchronous and never awaits, so we can hold the read
        // lock and borrow the state directly (no cloning).
        let response = completion::complete(
            docs.get(uri).map(|open| &open.state),
            position,
            &self.functions,
        );
        Ok(Some(response))
    }

    async fn completion_resolve(&self, item: CompletionItem) -> JsonRPCResult<CompletionItem> {
        Ok(item)
    }
}
