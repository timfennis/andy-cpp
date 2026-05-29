use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation, FunctionParameter, Lvalue};
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use crate::util::LineIndex;

/// Build the document outline (functions and variable declarations) from the
/// analysed AST. Declarations nested inside a function body become children of
/// that function's symbol.
pub fn document_symbols(
    ast: &[ExpressionLocation],
    text: &str,
    line_index: &LineIndex,
) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for expr in ast {
        collect_symbol(expr, text, line_index, &mut symbols);
    }
    symbols
}

fn collect_symbol(
    expr: &ExpressionLocation,
    text: &str,
    line_index: &LineIndex,
    out: &mut Vec<DocumentSymbol>,
) {
    match &expr.expression {
        Expression::Statement(inner) | Expression::Grouping(inner) => {
            collect_symbol(inner, text, line_index, out);
        }
        Expression::FunctionDeclaration {
            name: Some(name),
            parameters,
            return_type,
            body,
            ..
        } => {
            let mut children = Vec::new();
            collect_children(body, text, line_index, &mut children);
            out.push(make_symbol(
                name.clone(),
                Some(signature(parameters, return_type.as_ref())),
                SymbolKind::FUNCTION,
                expr.span,
                expr.span,
                text,
                line_index,
                children,
            ));
        }
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_lvalue_symbols(l_value, expr.span, text, line_index, out);
            // A lambda bound to a variable should still appear in the outline.
            collect_symbol(value, text, line_index, out);
        }
        _ => {}
    }
}

/// Walk a function body collecting nested function/variable declarations.
fn collect_children(
    body: &ExpressionLocation,
    text: &str,
    line_index: &LineIndex,
    out: &mut Vec<DocumentSymbol>,
) {
    match &body.expression {
        Expression::Block { statements } => {
            for s in statements {
                collect_symbol(s, text, line_index, out);
            }
        }
        Expression::Statement(inner) | Expression::Grouping(inner) => {
            collect_children(inner, text, line_index, out);
        }
        _ => collect_symbol(body, text, line_index, out),
    }
}

fn push_lvalue_symbols(
    lvalue: &Lvalue,
    decl_span: Span,
    text: &str,
    line_index: &LineIndex,
    out: &mut Vec<DocumentSymbol>,
) {
    match lvalue {
        Lvalue::Identifier {
            identifier,
            span,
            inferred_type,
            ..
        } => {
            out.push(make_symbol(
                identifier.clone(),
                inferred_type.as_ref().map(ToString::to_string),
                SymbolKind::VARIABLE,
                decl_span,
                *span,
                text,
                line_index,
                Vec::new(),
            ));
        }
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                push_lvalue_symbols(lv, decl_span, text, line_index, out);
            }
        }
        Lvalue::Index { .. } => {}
    }
}

/// Render a function signature like `(a, b) -> Int` for the symbol detail.
fn signature(parameters: &[FunctionParameter], return_type: Option<&StaticType>) -> String {
    let params = parameters
        .iter()
        .map(|p| match &p.lvalue {
            Lvalue::Identifier { identifier, .. } => identifier.clone(),
            _ => "_".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    match return_type {
        Some(rt) => format!("({params}) -> {rt}"),
        None => format!("({params})"),
    }
}

#[allow(clippy::too_many_arguments)]
fn make_symbol(
    name: String,
    detail: Option<String>,
    kind: SymbolKind,
    range_span: Span,
    selection_span: Span,
    text: &str,
    line_index: &LineIndex,
    children: Vec<DocumentSymbol>,
) -> DocumentSymbol {
    #[allow(deprecated)]
    DocumentSymbol {
        name,
        detail,
        kind,
        tags: None,
        deprecated: None,
        range: line_index.range(text, range_span),
        selection_range: line_index.range(text, selection_span),
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::DocumentState;
    use ndc_interpreter::Interpreter;

    fn symbols(source: &str) -> Vec<DocumentSymbol> {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let state = DocumentState::from_analysis(source.to_string(), ast, analysis);
        document_symbols(&state.ast, &state.source, &state.line_index)
    }

    #[test]
    fn top_level_function_and_variable_are_listed() {
        let syms = symbols("let answer = 42;\nfn greet(name) { name }\n");
        assert!(
            syms.iter()
                .any(|s| s.name == "answer" && s.kind == SymbolKind::VARIABLE),
            "expected variable `answer` in {syms:?}"
        );
        assert!(
            syms.iter()
                .any(|s| s.name == "greet" && s.kind == SymbolKind::FUNCTION),
            "expected function `greet` in {syms:?}"
        );
    }

    #[test]
    fn function_signature_in_detail() {
        let syms = symbols("fn add(a, b) -> Int { a + b }");
        let func = syms
            .iter()
            .find(|s| s.name == "add")
            .expect("add symbol present");
        let detail = func.detail.as_deref().unwrap_or_default();
        assert!(detail.contains("a, b"), "got detail: {detail}");
    }
}
