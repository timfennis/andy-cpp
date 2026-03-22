use ndc_core::StaticType;
use ndc_interpreter::Interpreter;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionResponse,
    Documentation, MarkupContent, MarkupKind, Position,
};

use crate::state::DocumentState;
use crate::util::offset_from_position;

/// Build completion response for the given cursor position and document state.
pub fn complete(
    state: Option<&DocumentState>,
    position: Position,
    interpreter: &Interpreter,
) -> CompletionResponse {
    let receiver_type = state.and_then(|s| {
        let offset = offset_from_position(&s.source, position)?;
        let ident = identifier_before_dot(&s.source, offset)?;
        s.variable_types.get(ident).cloned()
    });

    let is_dot = receiver_type.is_some();

    let items = interpreter.functions().filter_map(|fun| {
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

    let items: Vec<_> = if is_dot {
        items.collect()
    } else {
        items.chain(keyword_completions()).collect()
    };

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

fn keyword_completions() -> impl Iterator<Item = CompletionItem> {
    ["true", "false"].into_iter().map(|kw| CompletionItem {
        label: String::from(kw),
        kind: Some(CompletionItemKind::VALUE),
        ..Default::default()
    })
}

fn is_normal_ident(input: &str) -> bool {
    input
        .chars()
        .all(|c| c.is_alphanumeric() || c == '?' || c == '_')
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
    use crate::state::DocumentState;
    use std::collections::HashMap;

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
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);

        // Simulate: user typed `let x = [1,2,3]` then `x.`
        // variable_types has x as List(Int), source has the dot
        let state = DocumentState {
            hints: Vec::new(),
            source: "let x = [1,2,3]\nx.".to_string(),
            variable_types: HashMap::from([(
                "x".to_string(),
                StaticType::List(Box::new(StaticType::Int)),
            )]),
        };

        // Cursor is after the dot: line 1, character 2
        let response = complete(Some(&state), Position::new(1, 2), &interpreter);
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
        // This tests the key scenario: source has been updated to contain the dot,
        // but variable_types are preserved from a previous successful analysis.
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);

        let state = DocumentState {
            hints: Vec::new(),
            // Current source is invalid (has trailing dot)
            source: "let x = 42\nx.".to_string(),
            // Types from the last successful analysis
            variable_types: HashMap::from([("x".to_string(), StaticType::Int)]),
        };

        let response = complete(Some(&state), Position::new(1, 2), &interpreter);
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        // Should be dot-completion (no keywords)
        assert!(
            !items.iter().any(|i| i.label == "true"),
            "should be dot-completion, not general"
        );

        // Should include functions that accept Int
        assert!(
            items.iter().any(|i| i.label == "abs"),
            "dot-completion on Int should include `abs`"
        );
    }

    #[test]
    fn general_completion_includes_keywords() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);

        let state = DocumentState {
            hints: Vec::new(),
            source: "let x = 42\n".to_string(),
            variable_types: HashMap::from([("x".to_string(), StaticType::Int)]),
        };

        // No dot — general completion
        let response = complete(Some(&state), Position::new(1, 0), &interpreter);
        let CompletionResponse::Array(items) = response else {
            panic!("expected Array response");
        };

        assert!(
            items.iter().any(|i| i.label == "true"),
            "general completion should include keywords"
        );
    }
}
