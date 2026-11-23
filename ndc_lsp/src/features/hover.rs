use ls_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use ndc_analysis::{Analysis, Binding, Callee, ResolvedVar};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::Expression;

use crate::features::completion::FunctionInfo;
use crate::features::type_arguments;
use crate::state::DocumentState;
use crate::visitor::{AstVisitor, node_at_offset, walk_ast};

/// Build hover information for the cursor position.
///
/// When the cursor is on a declaration's name, shows the declared variable's
/// type. Otherwise resolves the innermost expression under the cursor and shows
/// its inferred type. An identifier is only shown as a built-in function when the
/// analyser resolved it to a global — so a local that shadows a built-in (e.g.
/// `let len = 1; len`) shows the local's type, not the built-in's docs.
pub fn hover(
    state: &DocumentState,
    position: Position,
    functions: &[FunctionInfo],
) -> Option<Hover> {
    let offset = state.offset_at(position)?;

    // A declaration's name is an lvalue, not an expression node, so check those
    // first — they are more specific than the enclosing declaration expression.
    // A type with an error in it is left out: the error is reported where it
    // happened, and the type would only show its failed parts as `Any`.
    if let Some((span, typ)) = declaration_at(state, offset) {
        if typ.has_errors() {
            return None;
        }
        return markup(state, span, with_type_arguments(state, &typ, offset));
    }

    let node = node_at_offset(&state.ast, offset)?;
    let markdown = match &node.expression {
        Expression::Identifier { .. } => {
            native_hover(&overloads_at(&state.analysis, node.id), functions)
                .or_else(|| type_hover(state, node.id, offset))
        }
        _ => type_hover(state, node.id, offset),
    }?;

    markup(state, node.span, markdown)
}

/// The functions the identifier `id` refers to: the overload it picked, or
/// one for each position of tuple arguments. In the body of a generic
/// function whose instances pick for their own types, as `abs` in
/// `fn f(x) { abs(x) } f(1); f(2.5);` does, that's every overload they pick:
/// `abs(Int)` and `abs(Float)`.
fn overloads_at(analysis: &Analysis, id: NodeId) -> Vec<Callee> {
    let binding = analysis.binding(id);
    let bindings: Vec<&Binding> = if *binding == Binding::None {
        analysis
            .instances
            .values()
            .flatten()
            .filter_map(|instance| instance.bindings.get(&id))
            .collect()
    } else {
        vec![binding]
    };
    let mut picked = Vec::new();
    for binding in bindings {
        let overloads = match binding {
            Binding::None => Vec::new(),
            Binding::Resolved(candidate) => vec![candidate.var()],
            Binding::PerPosition(overloads) => overloads.clone(),
        };
        for overload in overloads {
            if !picked.contains(&overload) {
                picked.push(overload);
            }
        }
    }
    picked
}

/// Markdown for an identifier the analyser resolved to built-in functions: the
/// signature and documentation of each overload it picked. `None` unless every
/// one is a built-in, so a local that shadows one (e.g. `let len = 1; len`)
/// shows the local's type.
fn native_hover(overloads: &[Callee], functions: &[FunctionInfo]) -> Option<String> {
    // A built-in's global slot is its position in the registry, which is the
    // order of `functions`.
    let markdown = overloads
        .iter()
        .map(|overload| match overload {
            Callee::Var(ResolvedVar::Global { slot }) => {
                functions.get(*slot).map(function_markdown)
            }
            Callee::Intrinsic(intrinsic) => functions
                .iter()
                .find(|function| function.intrinsic == Some(*intrinsic))
                .map(function_markdown),
            Callee::Var(_) => None,
        })
        .collect::<Option<Vec<_>>>()?;
    (!markdown.is_empty()).then(|| markdown.join("\n\n---\n\n"))
}

/// The hover of what's at `span`, unless it changed since the analysis, as
/// what it says may then not be so any more.
fn markup(state: &DocumentState, span: Span, value: String) -> Option<Hover> {
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(state.name_range(span)?),
    })
}

/// Find the declaration identifier whose span contains `offset`, returning its
/// span and inferred type.
fn declaration_at(state: &DocumentState, offset: usize) -> Option<(Span, StaticType)> {
    let mut finder = DeclFinder {
        analysis: &state.analysis,
        offset,
        found: None,
    };
    walk_ast(&mut finder, &state.ast);
    finder.found
}

struct DeclFinder<'a> {
    analysis: &'a Analysis,
    offset: usize,
    found: Option<(Span, StaticType)>,
}

impl AstVisitor for DeclFinder<'_> {
    fn on_declaration(
        &mut self,
        _identifier: &str,
        node: NodeId,
        _has_annotation: bool,
        span: Span,
    ) {
        if self.offset >= span.offset()
            && self.offset < span.end()
            && let Some(typ) = self.analysis.variable_types.get(&node)
        {
            self.found = Some((span, typ.clone()));
        }
    }
}

/// A built-in function's fenced signature followed by its documentation.
fn function_markdown(fun: &FunctionInfo) -> String {
    let mut out = format!(
        "```ndc\n{}\n```",
        format_signature(&fun.name, &fun.static_type)
    );
    if let Some(doc) = &fun.documentation {
        out.push_str("\n\n");
        out.push_str(doc);
    }
    out
}

/// Markdown showing the inferred type of an expression, unless its check failed.
fn type_hover(state: &DocumentState, id: NodeId, offset: usize) -> Option<String> {
    let typ = state
        .analysis
        .expr_types
        .get(&id)
        .filter(|typ| !typ.has_errors())?;
    Some(with_type_arguments(state, typ, offset))
}

/// `typ` as a code block, and if it has type parameters of the generic
/// function it's in, what they are at the calls the program makes of it.
fn with_type_arguments(state: &DocumentState, typ: &StaticType, offset: usize) -> String {
    let code = format!("```ndc\n{typ}\n```");
    match type_arguments::describe(state, typ, offset) {
        Some(calls) => format!("{code}\n\n{calls}"),
        None => code,
    }
}

/// Render a function name plus its [`StaticType`] as `name(p1, p2) -> ret`,
/// with its type parameters after the name, in the order type arguments
/// bind them: `fold<T, A>(Iterable<T>, A, fn(A, T) -> A) -> A`.
fn format_signature(name: &str, typ: &StaticType) -> String {
    let type_parameters = typ.type_parameters();
    let name = if type_parameters.is_empty() {
        name.to_string()
    } else {
        format!("{name}<{}>", type_parameters.join(", "))
    };
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
        (DocumentState::analysed(source), functions)
    }

    fn markdown(hover: Hover) -> String {
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        content.value
    }

    #[test]
    fn a_type_with_a_failed_check_in_it_has_no_hover() {
        // `f`'s two returns share no type, so `y`'s type is an error.
        let source = "fn f(c) { if c { return 1; } }\nlet y = f(true);";
        let (state, functions) = analyse(source);
        // On the declared `y`, and on `f`, which returns it.
        assert_eq!(hover(&state, Position::new(1, 4), &functions), None);
        assert_eq!(hover(&state, Position::new(1, 8), &functions), None);
    }

    #[test]
    fn hovering_a_declared_name_shows_its_type() {
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
    fn hover_on_a_builtin_shows_the_overload_the_call_picks_and_its_docs() {
        // `abs` starts at byte 8 in "let n = abs(-3);".
        let (state, functions) = analyse("let n = abs(-3);");
        let hover = hover(&state, Position::new(0, 9), &functions).expect("hover present");
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        assert!(
            content.value.contains("abs(Int) -> Int"),
            "expected the Int overload, got: {}",
            content.value
        );
        assert!(
            !content.value.contains("abs(Float)"),
            "expected only the picked overload, got: {}",
            content.value
        );
        assert!(
            content
                .value
                .contains("Returns the absolute value of a number."),
            "expected the documentation of abs, got: {}",
            content.value
        );
    }

    #[test]
    fn hover_on_a_builtin_in_a_generic_body_lists_what_its_instances_pick() {
        // `x` is untyped, so which `abs` the call reaches depends on the
        // instance.
        let src = "fn f(x) { abs(x) } f(1); f(2.5);";
        let (state, functions) = analyse(src);
        let pos = state
            .line_index
            .position(&state.source, src.find("abs").unwrap());
        let value = markdown(hover(&state, pos, &functions).expect("hover present"));
        for signature in ["abs(Int) -> Int", "abs(Float) -> Float"] {
            assert!(
                value.contains(signature),
                "expected {signature} among the candidates, got: {value}"
            );
        }
    }

    #[test]
    fn hover_on_a_generic_builtin_shows_the_order_of_its_type_parameters() {
        let src = "let n = fold::<Int, Int>([1], 0, fn(a, x) => a + x);";
        let (state, functions) = analyse(src);
        let pos = state
            .line_index
            .position(&state.source, src.find("fold").unwrap());
        let value = markdown(hover(&state, pos, &functions).expect("hover present"));
        assert!(
            value.contains("fold<T, A>(Iterable<T>, A, fn(A, T) -> A) -> A"),
            "expected the type parameters in order, got: {value}"
        );
    }

    #[test]
    fn hover_on_a_type_argument_is_not_hover_on_the_function() {
        let src = "let n = fold::<Int, Int>([1], 0, fn(a, x) => a + x);";
        let (state, functions) = analyse(src);
        let pos = state
            .line_index
            .position(&state.source, src.find("Int").unwrap());
        let value = hover(&state, pos, &functions).map(markdown);
        assert!(
            value.as_deref().is_none_or(|value| !value.contains("fold")),
            "expected no docs of fold on its type argument, got: {value:?}"
        );
    }

    #[test]
    fn hover_between_expressions_is_none() {
        let (state, functions) = analyse("let n = 1;\n\nlet m = 2;");
        // The blank line between the two declarations.
        assert!(hover(&state, Position::new(1, 0), &functions).is_none());
    }

    #[test]
    fn local_shadowing_builtin_shows_local_type_not_signature() {
        // `len` is a built-in, but here it's shadowed by a local Int. Hover on the
        // *use* of `len` must show the local's type, not the built-in's signature.
        let src = "let len = 1;\nlen;";
        let (state, functions) = analyse(src);
        let use_offset = src.rfind("len").unwrap();
        let pos = state.line_index.position(&state.source, use_offset);
        let hover = hover(&state, pos, &functions).expect("hover present");
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        assert!(
            content.value.contains("Int"),
            "expected the local Int type, got: {}",
            content.value
        );
        assert!(
            !content.value.contains("len("),
            "must not show the built-in signature for a shadowing local, got: {}",
            content.value
        );
    }

    #[test]
    fn hover_in_a_generic_function_says_what_its_calls_make_its_types() {
        let src = "fn merge_ranges(ranges) {\n    let rc = deepcopy(ranges);\n    rc\n}\nlet rr = [(1, 2), (3, 4)];\nlet merged = merge_ranges(rr);\n";
        let (state, functions) = analyse(src);
        let hover = hover(&state, Position::new(1, 9), &functions).expect("hover on `rc`");
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        assert_eq!(
            content.value,
            "```ndc\nA\n```\n\n`A` is `[(Int, Int)]` in `merge_ranges(rr)`, line 6"
        );
    }
}
