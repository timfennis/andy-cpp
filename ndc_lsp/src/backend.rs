use std::collections::HashMap;

use ndc_lib::ast::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue};
use ndc_lib::interpreter::Interpreter;
use ndc_lexer::{Lexer, Span, TokenLocation};
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result as JsonRPCResult;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionOptions,
    CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity, Documentation,
    InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintKind,
    InlayHintLabel, InlayHintParams, MarkupContent, MarkupKind, MessageType, OneOf, Position,
    Range, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    WorkDoneProgressOptions,
};
use tower_lsp::{Client, LanguageServer};

pub struct Backend {
    pub client: Client,
    documents: Mutex<HashMap<Url, Vec<InlayHint>>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
        }
    }

    async fn validate(&self, uri: &Url, text: &str) {
        let scanner = Lexer::new(text);
        let tokens = scanner
            .collect::<Result<Vec<TokenLocation>, ndc_lexer::Error>>()
            .map_err(|err| {
                let span: Span = err.location();

                Diagnostic {
                    range: span_into_range(text, span),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: None,
                    message: format!("{err}"),
                    related_information: None,
                    tags: None,
                    data: None,
                }
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

        // Run full semantic analysis and collect inlay hints from the annotated AST.
        // The interpreter uses Rc internally (non-Send), so it must be fully dropped
        // before the next await point.
        let hints = {
            let mut interpreter = Interpreter::new(Vec::new());
            match interpreter.analyse_str(text) {
                Ok(expressions) => {
                    let mut hints = Vec::new();
                    for expr in &expressions {
                        collect_hints(expr, text, &mut hints);
                    }
                    hints
                }
                Err(_) => Vec::new(),
            }
        };

        self.documents.lock().await.insert(uri.clone(), hints);
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

    async fn did_open(&self, params: tower_lsp::lsp_types::DidOpenTextDocumentParams) {
        self.validate(&params.text_document.uri, &params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        for change in params.content_changes {
            self.validate(&params.text_document.uri, &change.text).await;
        }
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> JsonRPCResult<Option<Vec<InlayHint>>> {
        let hints = self.documents.lock().await;
        Ok(hints.get(&params.text_document.uri).cloned())
    }

    async fn completion(
        &self,
        _params: CompletionParams,
    ) -> Result<Option<CompletionResponse>, tower_lsp::jsonrpc::Error> {
        let interpreter = Interpreter::new(Vec::new());
        let env = interpreter.environment();
        let functions = env.borrow().get_all_functions();

        let items = functions.iter().filter_map(|fun| {
            if !is_normal_ident(fun.name()) {
                return None;
            }

            Some(CompletionItem {
                label: fun.name().to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    detail: Some(format!("({})", fun.type_signature())),
                    description: Some(fun.return_type().to_string()),
                }),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: fun.documentation().to_string(),
                })),
                ..Default::default()
            })
        });

        let items = items.chain(vec![
            CompletionItem {
                label: String::from("true"),
                kind: Some(CompletionItemKind::VALUE),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("false"),
                kind: Some(CompletionItemKind::VALUE),
                ..Default::default()
            },
        ]);

        Ok(Some(CompletionResponse::Array(items.collect())))
    }

    async fn completion_resolve(
        &self,
        item: CompletionItem,
    ) -> Result<CompletionItem, tower_lsp::jsonrpc::Error> {
        Ok(item)
    }
}

/// Recursively walk an analysed AST node and collect inlay hints from places where
/// the analyser stored type information: `Lvalue::Identifier.inferred_type` (variable
/// and for-loop declarations) and `FunctionDeclaration.return_type`.
fn collect_hints(expr: &ExpressionLocation, text: &str, hints: &mut Vec<InlayHint>) {
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value } => {
            collect_hints_from_lvalue(l_value, text, hints);
            collect_hints(value, text, hints);
        }
        Expression::FunctionDeclaration {
            return_type,
            parameters,
            body,
            ..
        } => {
            if let Some(rt) = return_type {
                hints.push(InlayHint {
                    position: position_from_offset(text, parameters.span.end()),
                    label: InlayHintLabel::String(format!(" -> {rt}")),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                });
            }
            collect_hints(body, text, hints);
        }
        Expression::Statement(inner) => collect_hints(inner, text, hints),
        Expression::Grouping(inner) => collect_hints(inner, text, hints),
        Expression::Block { statements } => {
            for s in statements {
                collect_hints(s, text, hints);
            }
        }
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            collect_hints(condition, text, hints);
            collect_hints(on_true, text, hints);
            if let Some(f) = on_false {
                collect_hints(f, text, hints);
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            collect_hints(expression, text, hints);
            collect_hints(loop_body, text, hints);
        }
        Expression::For { iterations, body } => {
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { l_value, sequence } => {
                        collect_hints_from_lvalue(l_value, text, hints);
                        collect_hints(sequence, text, hints);
                    }
                    ForIteration::Guard(expr) => collect_hints(expr, text, hints),
                }
            }
            match body.as_ref() {
                ForBody::Block(e) | ForBody::List(e) => collect_hints(e, text, hints),
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    collect_hints(key, text, hints);
                    if let Some(v) = value {
                        collect_hints(v, text, hints);
                    }
                    if let Some(d) = default {
                        collect_hints(d, text, hints);
                    }
                }
            }
        }
        Expression::Return { value } => collect_hints(value, text, hints),
        // Literals, identifiers, ranges, calls etc. contain no declaration sites
        _ => {}
    }
}

fn collect_hints_from_lvalue(lvalue: &Lvalue, text: &str, hints: &mut Vec<InlayHint>) {
    match lvalue {
        Lvalue::Identifier {
            inferred_type: Some(typ),
            span,
            ..
        } => {
            hints.push(InlayHint {
                position: position_from_offset(text, span.end()),
                label: InlayHintLabel::String(format!(": {typ}")),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: Some(true),
                data: None,
            });
        }
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                collect_hints_from_lvalue(lv, text, hints);
            }
        }
        _ => {}
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

fn is_normal_ident(input: &str) -> bool {
    input
        .chars()
        .all(|c| c.is_alphanumeric() || c == '?' || c == '_')
}
