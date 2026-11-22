use ls_types::{DocumentSymbol, SymbolKind};
use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, Expression, ExpressionLocation, FunctionParameter,
    TypeParameter,
};

use crate::resolved::Definition;
use crate::state::DocumentState;

/// Build the document outline (functions, structs and variable declarations)
/// from the analysed AST, wherever they are declared. Declarations inside a
/// named function's body become children of that function's symbol.
///
/// While the text has changed since the analysis, a symbol whose name
/// changed, or whose range ends where the text changed, is left out.
pub fn document_symbols(state: &DocumentState) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for expr in &state.ast {
        collect_symbol(expr, state, &mut symbols);
    }
    symbols
}

fn collect_symbol(expr: &ExpressionLocation, state: &DocumentState, out: &mut Vec<DocumentSymbol>) {
    match &expr.expression {
        Expression::FunctionDeclaration {
            name: Some(name),
            type_parameters,
            parameters,
            return_annotation,
            body,
            ..
        } => {
            let mut children = Vec::new();
            collect_symbol(body, state, &mut children);
            // Only an annotated return type is shown; the declared function
            // type holds it as the analyser lowered it.
            let return_type = match state.analysis.function_types.get(&expr.id) {
                Some(StaticType::Function { return_type, .. }) if return_annotation.is_some() => {
                    Some(return_type.as_ref())
                }
                _ => None,
            };
            out.extend(make_symbol(
                name.clone(),
                Some(signature(type_parameters, parameters, return_type)),
                SymbolKind::FUNCTION,
                expr.span,
                declared_name(state, Definition::Node(expr.id), expr.span),
                state,
                children,
            ));
        }
        // A function without a name has no symbol to list what it declares under.
        Expression::FunctionDeclaration { name: None, .. } => {}
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_pattern_symbols(l_value, expr.span, state, out);
            collect_symbol(value, state, out);
        }
        Expression::StructDeclaration { name, fields, .. } => {
            let children = fields
                .iter()
                .enumerate()
                .filter_map(|(index, field)| {
                    let definition = Definition::Field {
                        declaration: expr.id,
                        index,
                    };
                    make_symbol(
                        field.identifier.clone(),
                        Some(field.annotation.to_string()),
                        SymbolKind::FIELD,
                        field.span,
                        declared_name(state, definition, field.span),
                        state,
                        Vec::new(),
                    )
                })
                .collect();
            out.extend(make_symbol(
                name.clone(),
                None,
                SymbolKind::STRUCT,
                expr.span,
                declared_name(state, Definition::Node(expr.id), expr.span),
                state,
                children,
            ));
        }
        _ => {
            for child in expr.children() {
                collect_symbol(child, state, out);
            }
        }
    }
}

fn push_pattern_symbols(
    pattern: &BindingPatternLocation,
    decl_span: Span,
    state: &DocumentState,
    out: &mut Vec<DocumentSymbol>,
) {
    match &pattern.pattern {
        BindingPattern::Identifier { identifier, span } => {
            out.extend(make_symbol(
                identifier.clone(),
                state
                    .analysis
                    .variable_types
                    .get(&pattern.id)
                    .map(ToString::to_string),
                SymbolKind::VARIABLE,
                decl_span,
                *span,
                state,
                Vec::new(),
            ));
        }
        BindingPattern::Sequence(patterns) => {
            for pattern in patterns {
                push_pattern_symbols(pattern, decl_span, state, out);
            }
        }
    }
}

/// Render a function signature like `(a, b) -> Int` for the symbol detail,
/// with the type parameters it declares in front, as `<T>(x) -> T`.
fn signature(
    type_parameters: &[TypeParameter],
    parameters: &[FunctionParameter],
    return_type: Option<&StaticType>,
) -> String {
    let declared = if type_parameters.is_empty() {
        String::new()
    } else {
        let names = type_parameters
            .iter()
            .map(|parameter| parameter.name.as_str());
        format!("<{}>", names.collect::<Vec<_>>().join(", "))
    };
    let params = parameters
        .iter()
        .map(|p| match &p.lvalue.pattern {
            BindingPattern::Identifier { identifier, .. } => identifier.clone(),
            BindingPattern::Sequence(_) => "_".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    match return_type {
        Some(rt) => format!("{declared}({params}) -> {rt}"),
        None => format!("{declared}({params})"),
    }
}

/// The name `definition` declares, or else all of `whole`.
fn declared_name(state: &DocumentState, definition: Definition, whole: Span) -> Span {
    state.names.declaration(definition).unwrap_or(whole)
}

#[allow(clippy::too_many_arguments)]
fn make_symbol(
    name: String,
    detail: Option<String>,
    kind: SymbolKind,
    range_span: Span,
    selection_span: Span,
    state: &DocumentState,
    children: Vec<DocumentSymbol>,
) -> Option<DocumentSymbol> {
    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        detail,
        kind,
        tags: None,
        deprecated: None,
        range: state.range_of(range_span)?,
        selection_range: state.name_range(selection_span)?,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::DocumentState;

    fn symbols(source: &str) -> Vec<DocumentSymbol> {
        let state = DocumentState::analysed(source);
        document_symbols(&state)
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
    fn a_function_symbol_shows_the_type_parameters_it_declares() {
        let syms = symbols("fn swap<B, A>(a: A, b: B) -> (B, A) { (b, a) }");
        let func = syms
            .iter()
            .find(|s| s.name == "swap")
            .expect("swap symbol present");
        assert_eq!(func.detail.as_deref(), Some("<B, A>(a, b) -> (B, A)"));
    }

    #[test]
    fn a_type_parameter_keeps_the_name_it_is_declared_with() {
        let syms = symbols("fn id<T>(x: T) -> T { x }");
        let func = syms
            .iter()
            .find(|s| s.name == "id")
            .expect("id symbol present");
        assert_eq!(func.detail.as_deref(), Some("<T>(x) -> T"));
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
            "print({ fn identity(value) => value; identity(1) })",
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
