use std::collections::HashMap;

use ndc_core::StaticType;
use ndc_interpreter::Interpreter;
use ndc_lexer::Span;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionResponse,
    Documentation, MarkupContent, MarkupKind, Position,
};

use crate::state::DocumentState;
use crate::visitor::{AstVisitor, walk_ast};

/// A `Send` snapshot of a registered native function, built once at startup so
/// completion (and hover) never have to rebuild the interpreter per request.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub static_type: StaticType,
    pub documentation: Option<String>,
}

impl FunctionInfo {
    /// Snapshot every function registered in an interpreter.
    pub fn collect(interpreter: &Interpreter) -> Vec<Self> {
        interpreter
            .functions()
            .map(|fun| Self {
                name: fun.name.clone(),
                static_type: fun.static_type.clone(),
                documentation: fun.documentation.clone(),
            })
            .collect()
    }
}

/// Build completion response for the given cursor position and document state.
pub fn complete(
    state: Option<&DocumentState>,
    position: Position,
    functions: &[FunctionInfo],
) -> CompletionResponse {
    let receiver_type = state.and_then(|s| {
        let offset = s.line_index.offset(&s.source, position)?;
        let dot_offset = find_dot_before(s.source.as_bytes(), offset)?;
        // Try expression type map first (handles `func(args).` and any expression),
        // then fall back to variable name lookup for simple `ident.` cases.
        s.expression_types.get(&dot_offset).cloned().or_else(|| {
            let ident = identifier_before_dot(&s.source, offset)?;
            s.variable_types.get(ident).cloned()
        })
    });

    let is_dot = receiver_type.is_some();

    let function_items = functions.iter().filter_map(|fun| {
        if !is_normal_ident(&fun.name) {
            return None;
        }

        if let Some(recv) = &receiver_type {
            match &fun.static_type {
                StaticType::Function {
                    parameters: Some(params),
                    ..
                } => {
                    if params.is_empty() || !recv.is_subtype(&params[0]) {
                        return None;
                    }
                }
                StaticType::Function {
                    parameters: None, ..
                } => {}
                _ => return None,
            }
        }

        let (param_detail, return_detail) = format_function_signature(&fun.static_type, is_dot);

        Some(CompletionItem {
            label: fun.name.clone(),
            label_details: Some(CompletionItemLabelDetails {
                detail: Some(param_detail),
                description: Some(return_detail),
            }),
            kind: Some(CompletionItemKind::FUNCTION),
            documentation: fun.documentation.as_ref().map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d.clone(),
                })
            }),
            ..Default::default()
        })
    });

    if is_dot {
        return CompletionResponse::Array(function_items.collect());
    }

    // General (non-dot) completion: functions + in-scope locals + keywords.
    let mut items: Vec<CompletionItem> = function_items.collect();
    if let Some(state) = state {
        items.extend(local_completions(state, position));
    }
    items.extend(keyword_completions());
    CompletionResponse::Array(items)
}

fn format_function_signature(typ: &StaticType, is_dot: bool) -> (String, String) {
    match typ {
        StaticType::Function {
            parameters: Some(params),
            return_type,
        } => {
            let skip = if is_dot { 1 } else { 0 };
            let ps = params
                .iter()
                .skip(skip)
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            (format!("({ps})"), return_type.to_string())
        }
        StaticType::Function {
            parameters: None,
            return_type,
        } => ("(...)".to_string(), return_type.to_string()),
        other => (String::new(), other.to_string()),
    }
}

/// Language keywords offered in general (non-dot) completion.
const KEYWORDS: &[&str] = &[
    "let", "fn", "if", "else", "while", "for", "in", "return", "break", "continue", "true", "false",
];

fn keyword_completions() -> impl Iterator<Item = CompletionItem> {
    KEYWORDS.iter().map(|kw| CompletionItem {
        label: String::from(*kw),
        kind: Some(CompletionItemKind::KEYWORD),
        ..Default::default()
    })
}

/// Collect in-scope local variables (declarations whose span ends at or before
/// the cursor) from the last successfully analysed AST.
fn local_completions(state: &DocumentState, position: Position) -> Vec<CompletionItem> {
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return Vec::new();
    };
    let mut collector = LocalsCollector {
        cursor: offset,
        locals: HashMap::new(),
    };
    walk_ast(&mut collector, &state.ast);

    collector
        .locals
        .into_iter()
        .map(|(name, typ)| CompletionItem {
            label: name,
            label_details: typ.as_ref().map(|t| CompletionItemLabelDetails {
                detail: None,
                description: Some(t.to_string()),
            }),
            kind: Some(CompletionItemKind::VARIABLE),
            ..Default::default()
        })
        .collect()
}

/// Gathers declared variable names visible at the cursor. Approximate: it does
/// not model block scoping, only "declared earlier in the file", which is enough
/// for a useful completion list.
struct LocalsCollector {
    cursor: usize,
    locals: HashMap<String, Option<StaticType>>,
}

impl AstVisitor for LocalsCollector {
    fn on_declaration(
        &mut self,
        identifier: &str,
        inferred_type: Option<&StaticType>,
        _has_annotation: bool,
        span: Span,
    ) {
        if span.end() <= self.cursor {
            self.locals
                .insert(identifier.to_string(), inferred_type.cloned());
        }
    }
}

fn is_normal_ident(input: &str) -> bool {
    input
        .chars()
        .all(|c| c.is_alphanumeric() || c == '?' || c == '_')
}

/// Scan backward from `offset` to find the byte position of the `.` trigger.
/// Returns the offset of the dot itself (i.e. the byte offset where the expression
/// before the dot ends, which is the key in `expression_types`).
fn find_dot_before(text: &[u8], offset: usize) -> Option<usize> {
    // Skip any whitespace between cursor and the dot
    let mut i = offset;
    while i > 0 && text[i - 1].is_ascii_whitespace() {
        i -= 1;
    }
    if i > 0 && text[i - 1] == b'.' {
        Some(i - 1)
    } else {
        None
    }
}

/// Given a byte offset (pointing at or just after the `.`), scan backward to find
/// the identifier immediately before the dot.
fn identifier_before_dot(text: &str, offset: usize) -> Option<&str> {
    let before = &text[..offset];
    let before = before.trim_end();
    let before = before.strip_suffix('.')?;
    let before = before.trim_end();

    let end = before.len();
    let start = before
        .char_indices()
        .rev()
        .take_while(|(_, c)| c.is_alphanumeric() || *c == '_' || *c == '?')
        .last()
        .map_or(end, |(i, _)| i);

    if start == end {
        return None;
    }

    Some(&before[start..end])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn functions() -> Vec<FunctionInfo> {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        FunctionInfo::collect(&interpreter)
    }

    /// Build a document state whose `variable_types` / `expression_types` are set
    /// directly, simulating the cached-after-analysis state used by completion.
    fn state_with(
        source: &str,
        variable_types: HashMap<String, StaticType>,
        expression_types: HashMap<usize, StaticType>,
    ) -> DocumentState {
        let mut state = DocumentState::from_source(source.to_string());
        state.variable_types = variable_types;
        state.expression_types = expression_types;
        state
    }

    #[test]
    fn identifier_before_dot_simple() {
        assert_eq!(identifier_before_dot("x.", 2), Some("x"));
    }

    #[test]
    fn identifier_before_dot_with_spaces() {
        assert_eq!(identifier_before_dot("x . ", 4), Some("x"));
    }

    #[test]
    fn identifier_before_dot_multiline() {
        // "let x = 5\nx." is 12 bytes; offset 12 is end of text (after the dot)
        assert_eq!(identifier_before_dot("let x = 5\nx.", 12), Some("x"));
    }

    #[test]
    fn identifier_before_dot_no_dot() {
        assert_eq!(identifier_before_dot("x", 1), None);
    }

    #[test]
    fn identifier_before_dot_nothing_before() {
        assert_eq!(identifier_before_dot(".", 1), None);
    }

    #[test]
    fn identifier_before_dot_underscore_and_question_mark() {
        assert_eq!(identifier_before_dot("is_empty?.", 10), Some("is_empty?"));
    }

    #[test]
    fn is_normal_ident_accepts_alphanumeric() {
        assert!(is_normal_ident("foo"));
        assert!(is_normal_ident("foo_bar"));
        assert!(is_normal_ident("empty?"));
    }

    #[test]
    fn is_normal_ident_rejects_operators() {
        assert!(!is_normal_ident("+"));
        assert!(!is_normal_ident("=="));
    }

    #[test]
    fn dot_completion_filters_by_receiver_type() {
        // Simulate: user typed `let x = [1,2,3]` then `x.`
        let state = state_with(
            "let x = [1,2,3]\nx.",
            HashMap::from([("x".to_string(), StaticType::List(Box::new(StaticType::Int)))]),
            HashMap::new(),
        );

        // Cursor is after the dot: line 1, character 2
        let response = complete(Some(&state), Position::new(1, 2), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        // Should not contain keyword completions (true/false) in dot mode
        assert!(
            !items.iter().any(|i| i.label == "true"),
            "dot-completion should not include keywords"
        );

        // Should contain list-compatible functions like `len`
        assert!(
            items.iter().any(|i| i.label == "len"),
            "dot-completion on list should include `len`"
        );
    }

    #[test]
    fn dot_completion_works_with_preserved_types_after_parse_failure() {
        // Source has been updated to contain the dot, but variable_types are
        // preserved from a previous successful analysis.
        let state = state_with(
            "let x = 42\nx.",
            HashMap::from([("x".to_string(), StaticType::Int)]),
            HashMap::new(),
        );

        let response = complete(Some(&state), Position::new(1, 2), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !items.iter().any(|i| i.label == "true"),
            "should be dot-completion, not general"
        );
        assert!(
            items.iter().any(|i| i.label == "abs"),
            "dot-completion on Int should include `abs`"
        );
    }

    #[test]
    fn dot_completion_on_call_expression_via_expression_types() {
        // Simulates `read_file("foo").` where the expression type map knows
        // that the call expression `read_file("foo")` returns String.
        let source = r#"read_file("foo")."#;
        // The call expression spans bytes 0..16, so its end offset is 16.
        let state = state_with(
            source,
            HashMap::new(),
            HashMap::from([(16, StaticType::String)]),
        );

        // Cursor is at end: line 0, character 17 (after the dot)
        let response = complete(Some(&state), Position::new(0, 17), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            !items.iter().any(|i| i.label == "true"),
            "should be dot-completion, not general"
        );
        assert!(
            items.iter().any(|i| i.label == "len"),
            "dot-completion on String should include `len`"
        );
    }

    #[test]
    fn general_completion_includes_keywords() {
        let state = state_with(
            "let x = 42\n",
            HashMap::from([("x".to_string(), StaticType::Int)]),
            HashMap::new(),
        );

        // No dot — general completion
        let response = complete(Some(&state), Position::new(1, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            items.iter().any(|i| i.label == "true"),
            "general completion should include keywords"
        );
        assert!(
            items.iter().any(|i| i.label == "fn"),
            "general completion should include the `fn` keyword"
        );
    }

    #[test]
    fn general_completion_includes_in_scope_locals() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let source = "let greeting = \"hi\";\n";
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let state = DocumentState::from_analysis(source.to_string(), ast, analysis);

        // Cursor on the (empty) second line — `greeting` is in scope.
        let response = complete(Some(&state), Position::new(1, 0), &functions());
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            items
                .iter()
                .any(|i| i.label == "greeting" && i.kind == Some(CompletionItemKind::VARIABLE)),
            "general completion should include the in-scope local `greeting`"
        );
    }
}
