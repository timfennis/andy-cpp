use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
    InitializedParams, MessageType, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};

use ndc_lib::interpreter::Interpreter;
use ndc_lib::lexer::{Lexer, Span, TokenLocation};
use tower_lsp::jsonrpc::Result as JsonRPCResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

pub struct Backend {
    pub client: Client,
}
impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }

    async fn validate(&self, uri: &Url, text: &str) {
        let scanner = Lexer::new(text);
        let tokens = scanner
            .collect::<Result<Vec<TokenLocation>, ndc_lib::lexer::Error>>()
            .map_err(|err| {
                let span: Span = err.location();
                let diag = Diagnostic {
                    range: span_into_range(text, span),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: None,
                    message: format!("{err}"),
                    related_information: None,
                    tags: None,
                    data: None,
                };

                diag
            });

        let mut diagnostics = vec![];
        match tokens {
            Ok(tokens) => {
                let mut parser = ndc_lib::ast::Parser::from_tokens(tokens);

                let parse_result = parser.parse().map_err(|err| Diagnostic {
                    range: span_into_range(text, err.location()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: None,
                    message: format!("{err}"),
                    related_information: None,
                    tags: None,
                    data: None,
                });

                if let Err(diag) = parse_result {
                    diagnostics.push(diag);
                }
            }
            Err(diag) => diagnostics.push(diag),
        }

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

fn span_into_range(text: &str, span: Span) -> Range {
    Range {
        start: position_from_offset(text, span.offset()),
        end: position_from_offset(text, span.end()),
    }
}
fn position_from_offset(text: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut col = 0;
    let mut byte_count = 0;

    for c in text.chars() {
        let char_len = c.len_utf8();
        if byte_count >= offset {
            break;
        }

        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }

        byte_count += char_len;
    }

    Position {
        line,
        character: col,
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
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("andy-cpp-diag-provider".to_string()),
                        inter_file_dependencies: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                        workspace_diagnostics: false,
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false), // or true if you want to support `completionItem/resolve`
                    trigger_characters: Some(vec![".".to_string()]), // characters that trigger completion
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    completion_item: None,
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn completion(
        &self,
        _params: CompletionParams,
    ) -> Result<Option<CompletionResponse>, tower_lsp::jsonrpc::Error> {
        let interpreter = Interpreter::new(Vec::new());
        let env = interpreter.environment();
        let functions = env.borrow().get_all_functions();

        let items = functions.iter().flat_map(|f| {
            f.implementations().filter_map(|(sig, fun)| {
                // Ignore operators
                if fun.name().chars().all(|c| c.is_alphanumeric() || c == '?') {
                    Some(CompletionItem {
                        label: fun.name().to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        documentation: Some(Documentation::MarkupContent(MarkupContent { kind: MarkupKind::Markdown, value: fun.documentation().to_string() })),
                        ..Default::default()
                    })
                } else {
                    None
                }
            })
        });

        Ok(Some(CompletionResponse::Array(items.collect())))
    }

    // Optional: resolve more details on selected item (THIS IS DISABLED FOR NOW??!)
    async fn completion_resolve(
        &self,
        item: CompletionItem,
    ) -> Result<CompletionItem, tower_lsp::jsonrpc::Error> {
        Ok(item) // add documentation, detail, etc. if needed
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
        self.validate(&params.text_document.uri, &params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        for change in params.content_changes {
            self.validate(&params.text_document.uri, &change.text).await;
        }
    }
}
