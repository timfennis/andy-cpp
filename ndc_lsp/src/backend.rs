use std::collections::HashMap;

use ndc_core::FunctionRegistry;
use ndc_interpreter::{Interpreter, NativeFunction};
use std::rc::Rc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result as JsonRPCResult;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
    InitializedParams, InlayHint, InlayHintParams, MessageType, OneOf, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use tower_lsp::{Client, LanguageServer};

use crate::diagnostics;
use crate::features::{completion, inlay_hints};
use crate::state::DocumentState;

pub struct Backend {
    pub client: Client,
    documents: Mutex<HashMap<Url, DocumentState>>,
    configure: fn(&mut FunctionRegistry<Rc<NativeFunction>>),
}

impl Backend {
    pub fn new(client: Client, configure: fn(&mut FunctionRegistry<Rc<NativeFunction>>)) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
            configure,
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
        let mut docs = self.documents.lock().await;
        match docs.get_mut(uri) {
            Some(state) => state.source = text.to_string(),
            None => {
                docs.insert(
                    uri.clone(),
                    DocumentState {
                        hints: Vec::new(),
                        source: text.to_string(),
                        variable_types: HashMap::new(),
                        expression_types: HashMap::new(),
                    },
                );
            }
        }
    }

    /// Run diagnostics and semantic analysis, updating cached hints and types.
    /// The source text must already be updated via `update_source` before calling this.
    async fn validate(&self, uri: &Url, text: &str) {
        let (diagnostics, _ast) = diagnostics::lex_and_parse(text);

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;

        // Run full semantic analysis and collect inlay hints + variable types.
        // The interpreter uses Rc internally (non-Send), so it must be fully
        // dropped before the next await point.
        let analysis = {
            let mut interpreter = self.make_interpreter();
            interpreter
                .analyse_str(text)
                .ok()
                .map(|(expressions, analysis_result)| {
                    inlay_hints::collect(&expressions, &analysis_result, text)
                })
        };

        // Only update document state when analysis succeeds. On failure (e.g.
        // incomplete syntax while typing `x.`), keep the last good hints and
        // variable types so inlay hints stay visible and dot-completion works.
        if let Some(info) = analysis {
            let mut docs = self.documents.lock().await;
            if let Some(state) = docs.get_mut(uri) {
                state.hints = info.hints;
                state.variable_types = info.variable_types;
                state.expression_types = info.expression_types;
            }
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
        let docs = self.documents.lock().await;
        Ok(docs
            .get(&params.text_document.uri)
            .map(|state| state.hints.clone()))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> JsonRPCResult<Option<CompletionResponse>> {
        let state = {
            let docs = self.documents.lock().await;
            let uri = &params.text_document_position.text_document.uri;
            docs.get(uri).map(|s| {
                // Clone what completion needs so we can drop the lock.
                DocumentState {
                    hints: Vec::new(), // not needed for completion
                    source: s.source.clone(),
                    variable_types: s.variable_types.clone(),
                    expression_types: s.expression_types.clone(),
                }
            })
        };

        let interpreter = self.make_interpreter();
        let position = params.text_document_position.position;
        let response = completion::complete(state.as_ref(), position, &interpreter);
        Ok(Some(response))
    }

    async fn completion_resolve(&self, item: CompletionItem) -> JsonRPCResult<CompletionItem> {
        Ok(item)
    }
}
