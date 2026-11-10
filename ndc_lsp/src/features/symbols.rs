use ndc_analysis::Analysis;
use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, Expression, ExpressionLocation, FunctionParameter,
};
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use crate::util::LineIndex;

/// Build the document outline (functions and variable declarations) from the
/// analysed AST. Declarations nested inside a function body become children of
/// that function's symbol.
pub fn document_symbols(
    ast: &[ExpressionLocation],
    analysis: &Analysis,
    text: &str,
    line_index: &LineIndex,
) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for expr in ast {
        collect_symbol(expr, analysis, text, line_index, &mut symbols);
    }
    symbols
}

fn collect_symbol(
    expr: &ExpressionLocation,
    analysis: &Analysis,
    text: &str,
    line_index: &LineIndex,
    out: &mut Vec<DocumentSymbol>,
) {
    match &expr.expression {
        Expression::Statement(inner)
        | Expression::Grouping(inner)
        | Expression::Cast { value: inner, .. } => {
            collect_symbol(inner, analysis, text, line_index, out);
        }
        Expression::FunctionDeclaration {
            name: Some(name),
            parameters,
            return_annotation,
            body,
            ..
        } => {
            let mut children = Vec::new();
            collect_children(body, analysis, text, line_index, &mut children);
            // Only an annotated return type is shown; the declared function
            // type holds it as the analyser lowered it.
            let return_type = match analysis.function_types.get(&expr.id) {
                Some(StaticType::Function { return_type, .. }) if return_annotation.is_some() => {
                    Some(return_type.as_ref())
                }
                _ => None,
            };
            out.push(make_symbol(
                name.clone(),
                Some(signature(parameters, return_type)),
                SymbolKind::FUNCTION,
                expr.span,
                expr.span,
                text,
                line_index,
                children,
            ));
        }
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_pattern_symbols(l_value, expr.span, analysis, text, line_index, out);
            // A lambda bound to a variable should still appear in the outline.
            collect_symbol(value, analysis, text, line_index, out);
        }
        Expression::StructDeclaration { name, fields, .. } => {
            let children = fields
                .iter()
                .map(|field| {
                    make_symbol(
                        field.identifier.clone(),
                        Some(field.annotation.to_string()),
                        SymbolKind::FIELD,
                        field.span,
                        field.span,
                        text,
                        line_index,
                        Vec::new(),
                    )
                })
                .collect();
            out.push(make_symbol(
                name.clone(),
                None,
                SymbolKind::STRUCT,
                expr.span,
                expr.span,
                text,
                line_index,
                children,
            ));
        }
        _ => {}
    }
}

/// Walk a function body collecting nested function/variable declarations.
fn collect_children(
    body: &ExpressionLocation,
    analysis: &Analysis,
    text: &str,
    line_index: &LineIndex,
    out: &mut Vec<DocumentSymbol>,
) {
    match &body.expression {
        Expression::Block { statements } => {
            for s in statements {
                collect_symbol(s, analysis, text, line_index, out);
            }
        }
        Expression::Statement(inner)
        | Expression::Grouping(inner)
        | Expression::Cast { value: inner, .. } => {
            collect_children(inner, analysis, text, line_index, out);
        }
        _ => collect_symbol(body, analysis, text, line_index, out),
    }
}

fn push_pattern_symbols(
    pattern: &BindingPatternLocation,
    decl_span: Span,
    analysis: &Analysis,
    text: &str,
    line_index: &LineIndex,
    out: &mut Vec<DocumentSymbol>,
) {
    match &pattern.pattern {
        BindingPattern::Identifier { identifier, span } => {
            out.push(make_symbol(
                identifier.clone(),
                analysis
                    .variable_types
                    .get(&pattern.id)
                    .map(ToString::to_string),
                SymbolKind::VARIABLE,
                decl_span,
                *span,
                text,
                line_index,
                Vec::new(),
            ));
        }
        BindingPattern::Sequence(patterns) => {
            for pattern in patterns {
                push_pattern_symbols(pattern, decl_span, analysis, text, line_index, out);
            }
        }
    }
}

/// Render a function signature like `(a, b) -> Int` for the symbol detail.
fn signature(parameters: &[FunctionParameter], return_type: Option<&StaticType>) -> String {
    let params = parameters
        .iter()
        .map(|p| match &p.lvalue.pattern {
            BindingPattern::Identifier { identifier, .. } => identifier.clone(),
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
        document_symbols(
            &state.ast,
            &state.analysis,
            &state.source,
            &state.line_index,
        )
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

    #[test]
    fn struct_with_fields_in_outline() {
        let syms = symbols("struct Point {\n    x: Int,\n    y: Int,\n}\n");
        let point = syms
            .iter()
            .find(|s| s.name == "Point" && s.kind == SymbolKind::STRUCT)
            .expect("Point struct symbol present");
        let children = point.children.as_deref().unwrap_or_default();
        assert!(
            children
                .iter()
                .any(|c| c.name == "x" && c.kind == SymbolKind::FIELD),
            "expected field `x` in {children:?}"
        );
        assert_eq!(children.len(), 2);
        assert_eq!(children[1].detail.as_deref(), Some("Int"));
    }

    #[test]
    fn cast_does_not_hide_a_declaration() {
        let syms = symbols("(fn identity(value) => value) as Any");
        assert!(
            syms.iter()
                .any(|s| s.name == "identity" && s.kind == SymbolKind::FUNCTION),
            "expected function `identity` in {syms:?}"
        );
    }
}
