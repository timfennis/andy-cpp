use lsp_types::{DocumentSymbol, SymbolKind};
use ndc_analysis::Analysis;
use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, Expression, ExpressionLocation, FunctionParameter,
};

use crate::util::LineIndex;

/// Build the document outline (functions, structs and variable declarations)
/// from the analysed AST, wherever they are declared. Declarations inside a
/// named function's body become children of that function's symbol.
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
        Expression::FunctionDeclaration {
            name: Some(name),
            parameters,
            return_annotation,
            body,
            ..
        } => {
            let mut children = Vec::new();
            collect_symbol(body, analysis, text, line_index, &mut children);
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
        // A function without a name has no symbol to list what it declares under.
        Expression::FunctionDeclaration { name: None, .. } => {}
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_pattern_symbols(l_value, expr.span, analysis, text, line_index, out);
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
        _ => {
            for child in expr.children() {
                collect_symbol(child, analysis, text, line_index, out);
            }
        }
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
            BindingPattern::Sequence(_) => "_".to_string(),
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
    fn a_function_symbol_shows_its_parameters_and_annotated_return_type() {
        let syms = symbols("fn add(a, b) -> Int { a + b }");
        let func = syms
            .iter()
            .find(|s| s.name == "add")
            .expect("add symbol present");
        assert_eq!(func.detail.as_deref(), Some("(a, b) -> Int"));
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
    fn declarations_nested_in_any_expression_are_listed() {
        let syms = symbols(concat!(
            "if true { fn helper() { 1 } }\n",
            "for x in [1] { let doubled = x * 2; }\n",
            "(fn identity(value) => value) as Any",
        ));
        let names = syms.iter().map(|s| s.name.as_str()).collect::<Vec<_>>();
        assert_eq!(names, ["helper", "doubled", "identity"]);
    }

    #[test]
    fn declarations_nested_in_a_function_body_are_its_children() {
        let syms = symbols("fn outer() { while true { fn inner() { 1 }; break; } }");
        let [outer] = syms.as_slice() else {
            panic!("expected only `outer` at the top level, got {syms:?}");
        };
        let children = outer.children.as_deref().unwrap_or_default();
        let names = children.iter().map(|s| s.name.as_str()).collect::<Vec<_>>();
        assert_eq!(names, ["inner"]);
    }
}
