use ahash::AHashMap;

use ndc_core::FunctionRegistry;
use ndc_interpreter::{Interpreter, NativeFunction};
use std::rc::Rc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result as JsonRPCResult;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, InlayHint, InlayHintParams, MessageType, OneOf, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use tower_lsp::{Client, LanguageServer};

use crate::diagnostics;
use crate::features::completion::FunctionInfo;
use crate::features::{completion, definition, hover, inlay_hints, symbols};
use crate::state::DocumentState;

pub struct Backend {
    pub client: Client,
    documents: RwLock<AHashMap<Url, DocumentState>>,
    configure: fn(&mut FunctionRegistry<Rc<NativeFunction>>),
    /// Native-function metadata, snapshotted once at startup. The set of native
    /// functions never changes, so completion and hover read this instead of
    /// rebuilding an interpreter per request.
    functions: Vec<FunctionInfo>,
}

impl Backend {
    pub fn new(client: Client, configure: fn(&mut FunctionRegistry<Rc<NativeFunction>>)) -> Self {
        let functions = {
            let mut interpreter = Interpreter::capturing();
            interpreter.configure(configure);
            FunctionInfo::collect(&interpreter)
        };
        Self {
            client,
            documents: RwLock::new(AHashMap::new()),
            configure,
            functions,
        }
    }

    fn make_interpreter(&self) -> Interpreter {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(self.configure);
        interpreter
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
        if let Some(state) = docs.get_mut(uri) {
            state.source = text.to_string();
            state.line_index = crate::util::LineIndex::new(text);
            state.version = version;
            // The AST now predates this edit; `validate` re-sets this to true
            // if the new source parses.
            state.analysis_matches_source = false;
        } else {
            let mut state = DocumentState::from_source(text.to_string());
            state.version = version;
            docs.insert(uri.clone(), state);
        }
    }

    /// Run diagnostics and semantic analysis for `version` of the document, then
    /// commit the analysis and publish diagnostics only if no later edit has
    /// superseded it. `update_source` must have run for this `version` first.
    async fn validate(&self, uri: &Url, text: &str, version: i32) {
        let (mut diagnostics, _ast) = diagnostics::lex_and_parse(text);

        // Run full semantic analysis. The interpreter uses Rc internally
        // (non-Send), so it must be fully dropped before the next await point.
        let analysed = {
            let mut interpreter = self.make_interpreter();
            interpreter
                .analyse_str(text)
                .ok()
                .map(|(expressions, analysis_result)| {
                    for err in &analysis_result.errors {
                        diagnostics.push(diagnostics::analysis_error_to_diagnostic(text, err));
                    }
                    (expressions, analysis_result)
                })
        };

        {
            let mut docs = self.documents.write().await;
            let Some(state) = docs.get_mut(uri) else {
                return;
            };
            // A later edit already moved the buffer on — drop this stale run
            // rather than committing old AST or publishing old diagnostics.
            if state.version != version {
                return;
            }
            // On success, commit in place (keeps the current source/line_index).
            // On parse failure, keep the last good AST; `analysis_matches_source`
            // is already false, so AST-backed features stay disabled.
            if let Some((ast, analysis)) = analysed {
                state.set_analysis(ast, analysis);
            }
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
        // document's diagnostics.
        self.documents.write().await.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> JsonRPCResult<Option<Vec<InlayHint>>> {
        let docs = self.documents.read().await;
        Ok(docs.get(&params.text_document.uri).map(|state| {
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
            .filter(|state| state.analysis_matches_source)
            .and_then(|state| {
                definition::goto_definition(state, position, uri.clone())
                    .map(GotoDefinitionResponse::Scalar)
            }))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> JsonRPCResult<Option<DocumentSymbolResponse>> {
        let docs = self.documents.read().await;
        Ok(docs
            .get(&params.text_document.uri)
            .filter(|state| state.analysis_matches_source)
            .map(|state| {
                DocumentSymbolResponse::Nested(symbols::document_symbols(
                    &state.ast,
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
        let response = completion::complete(docs.get(uri), position, &self.functions);
        Ok(Some(response))
    }

    async fn completion_resolve(&self, item: CompletionItem) -> JsonRPCResult<CompletionItem> {
        Ok(item)
    }
}
