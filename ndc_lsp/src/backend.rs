use std::collections::HashMap;

use ndc_core::FunctionRegistry;
use ndc_interpreter::{Interpreter, NativeFunction};
use std::rc::Rc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result as JsonRPCResult;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, InlayHint,
    InlayHintParams, MessageType, OneOf, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use tower_lsp::{Client, LanguageServer};

use crate::diagnostics;
use crate::features::completion::FunctionInfo;
use crate::features::{completion, definition, hover, inlay_hints, symbols};
use crate::state::DocumentState;

pub struct Backend {
    pub client: Client,
    documents: RwLock<HashMap<Url, DocumentState>>,
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
            documents: RwLock::new(HashMap::new()),
            configure,
            functions,
        }
    }

    fn make_interpreter(&self) -> Interpreter {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(self.configure);
        interpreter
    }

    /// Update the source text immediately so concurrent requests (e.g. completion
    /// triggered by `.`) always see the latest document content.
    async fn update_source(&self, uri: &Url, text: &str) {
        let mut docs = self.documents.write().await;
        match docs.get_mut(uri) {
            Some(state) => {
                state.source = text.to_string();
                state.line_index = crate::util::LineIndex::new(text);
            }
            None => {
                docs.insert(uri.clone(), DocumentState::from_source(text.to_string()));
            }
        }
    }

    /// Run diagnostics and semantic analysis, replacing the cached AST and side
    /// tables. The source text must already be updated via `update_source`.
    async fn validate(&self, uri: &Url, text: &str) {
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

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;

        // Only replace document state when analysis succeeds. On failure (e.g.
        // incomplete syntax while typing `x.`), keep the last good AST and maps
        // so inlay hints stay visible and dot-completion keeps working.
        if let Some((ast, analysis)) = analysed {
            let mut docs = self.documents.write().await;
            docs.insert(
                uri.clone(),
                DocumentState::from_analysis(text.to_string(), ast, analysis),
            );
        }
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
        self.update_source(&uri, &text).await;
        self.validate(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        for change in params.content_changes {
            self.update_source(&uri, &change.text).await;
            self.validate(&uri, &change.text).await;
        }
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> JsonRPCResult<Option<Vec<InlayHint>>> {
        let docs = self.documents.read().await;
        Ok(docs.get(&params.text_document.uri).map(|state| {
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
            .and_then(|state| hover::hover(state, position, &self.functions)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> JsonRPCResult<Option<GotoDefinitionResponse>> {
        let docs = self.documents.read().await;
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(docs.get(uri).and_then(|state| {
            definition::goto_definition(state, position, uri.clone())
                .map(GotoDefinitionResponse::Scalar)
        }))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> JsonRPCResult<Option<DocumentSymbolResponse>> {
        let docs = self.documents.read().await;
        Ok(docs.get(&params.text_document.uri).map(|state| {
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
