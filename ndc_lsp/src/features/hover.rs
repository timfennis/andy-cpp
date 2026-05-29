use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::Expression;
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use crate::features::completion::FunctionInfo;
use crate::state::DocumentState;
use crate::visitor::{AstVisitor, node_at_offset, walk_ast};

/// Build hover information for the cursor position.
///
/// When the cursor is on a declaration's name, shows the declared variable's
/// type. Otherwise resolves the innermost expression under the cursor and shows
/// its inferred type — or, for an identifier naming a built-in function, the
/// function's signature and documentation.
pub fn hover(
    state: &DocumentState,
    position: Position,
    functions: &[FunctionInfo],
) -> Option<Hover> {
    let offset = state.line_index.offset(&state.source, position)?;

    // A declaration's name is an lvalue, not an expression node, so check those
    // first — they are more specific than the enclosing declaration expression.
    if let Some((span, typ)) = declaration_at(state, offset) {
        return Some(markup(state, span, format!("```ndc\n{typ}\n```")));
    }

    let node = node_at_offset(&state.ast, offset)?;
    let markdown = if let Expression::Identifier { name, .. } = &node.expression {
        function_hover(name, functions).or_else(|| type_hover(state, node.id))
    } else {
        type_hover(state, node.id)
    }?;

    Some(markup(state, node.span, markdown))
}

fn markup(state: &DocumentState, span: Span, value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(state.line_index.range(&state.source, span)),
    }
}

/// Find the declaration identifier whose span contains `offset`, returning its
/// span and inferred type.
fn declaration_at(state: &DocumentState, offset: usize) -> Option<(Span, StaticType)> {
    let mut finder = DeclFinder {
        offset,
        found: None,
    };
    walk_ast(&mut finder, &state.ast);
    finder.found
}

struct DeclFinder {
    offset: usize,
    found: Option<(Span, StaticType)>,
}

impl AstVisitor for DeclFinder {
    fn on_declaration(
        &mut self,
        _identifier: &str,
        inferred_type: Option<&StaticType>,
        _has_annotation: bool,
        span: Span,
    ) {
        if self.offset >= span.offset()
            && self.offset < span.end()
            && let Some(typ) = inferred_type
        {
            self.found = Some((span, typ.clone()));
        }
    }
}

/// Markdown for an identifier that names a built-in function: a fenced
/// signature followed by its documentation.
fn function_hover(name: &str, functions: &[FunctionInfo]) -> Option<String> {
    let fun = functions.iter().find(|f| f.name == name)?;
    let mut out = format!(
        "```ndc\n{}\n```",
        format_signature(&fun.name, &fun.static_type)
    );
    if let Some(doc) = &fun.documentation {
        out.push_str("\n\n");
        out.push_str(doc);
    }
    Some(out)
}

/// Markdown showing the inferred type of an expression.
fn type_hover(state: &DocumentState, id: ndc_parser::NodeId) -> Option<String> {
    let typ = state.analysis.expr_types.get(&id)?;
    Some(format!("```ndc\n{typ}\n```"))
}

/// Render a function name plus its [`StaticType`] as `name(p1, p2) -> ret`.
fn format_signature(name: &str, typ: &StaticType) -> String {
    match typ {
        StaticType::Function {
            parameters: Some(params),
            return_type,
        } => {
            let ps = params
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{name}({ps}) -> {return_type}")
        }
        StaticType::Function {
            parameters: None,
            return_type,
        } => format!("{name}(...) -> {return_type}"),
        other => format!("{name}: {other}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;

    fn analyse(source: &str) -> (DocumentState, Vec<FunctionInfo>) {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let functions = FunctionInfo::collect(&interpreter);
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let state = DocumentState::from_analysis(source.to_string(), ast, analysis);
        (state, functions)
    }

    #[test]
    fn hover_on_variable_shows_type() {
        let (state, functions) = analyse("let count = 41 + 1;");
        // Cursor on `count` (character 4..9 on line 0).
        let hover = hover(&state, Position::new(0, 6), &functions).expect("hover present");
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        assert!(
            content.value.contains("Int"),
            "expected Int in hover, got: {}",
            content.value
        );
    }

    #[test]
    fn hover_on_builtin_function_shows_signature_and_docs() {
        let (state, functions) = analyse("let n = abs(-3);");
        // `abs` starts at byte 8 in "let n = abs(-3);".
        let hover = hover(&state, Position::new(0, 9), &functions).expect("hover present");
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        assert!(
            content.value.contains("abs("),
            "expected a signature for abs, got: {}",
            content.value
        );
    }

    #[test]
    fn hover_outside_any_node_is_none() {
        let (state, functions) = analyse("let n = 1;");
        // Way past the end of the document.
        assert!(hover(&state, Position::new(5, 0), &functions).is_none());
    }
}
