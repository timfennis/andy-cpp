use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use ndc_analysis::{Analysis, Binding, ResolvedVar};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::Expression;

use crate::features::completion::FunctionInfo;
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
    let offset = state.line_index.offset(&state.source, position)?;

    // A declaration's name is an lvalue, not an expression node, so check those
    // first — they are more specific than the enclosing declaration expression.
    if let Some((span, typ)) = declaration_at(state, offset) {
        return Some(markup(state, span, format!("```ndc\n{typ}\n```")));
    }

    let node = node_at_offset(&state.ast, offset)?;
    let markdown = match &node.expression {
        Expression::Identifier { .. } => {
            native_hover(&binding_at(&state.analysis, node.id), functions)
                .or_else(|| type_hover(state, node.id))
        }
        _ => type_hover(state, node.id),
    }?;

    Some(markup(state, node.span, markdown))
}

/// What the identifier `id` refers to. In the body of a generic function
/// whose instances pick for their own types, as `abs` in
/// `fn f(x) { abs(x) } f(1); f(2.5);` does, that's every overload they pick:
/// `abs(Int)` and `abs(Float)`.
fn binding_at(analysis: &Analysis, id: NodeId) -> Binding {
    let binding = analysis.binding(id);
    if *binding != Binding::None {
        return binding.clone();
    }
    let mut picked = Vec::new();
    for binding in analysis
        .instances
        .values()
        .flatten()
        .filter_map(|instance| instance.bindings.get(&id))
    {
        let candidates = match binding {
            Binding::Resolved(candidate) => std::slice::from_ref(candidate),
            Binding::Dynamic(candidates) => candidates.as_slice(),
            Binding::None => &[],
        };
        for candidate in candidates {
            if !picked.contains(candidate) {
                picked.push(*candidate);
            }
        }
    }
    if picked.is_empty() {
        Binding::None
    } else {
        Binding::Dynamic(picked)
    }
}

/// Markdown for an identifier the analyser resolved to built-in functions: the
/// signature and documentation of the overload it picked, or of each one it
/// leaves to runtime dispatch. `None` unless every candidate is a built-in, so a
/// local that shadows one (e.g. `let len = 1; len`) shows the local's type.
fn native_hover(binding: &Binding, functions: &[FunctionInfo]) -> Option<String> {
    let candidates = match binding {
        Binding::Resolved(candidate) => std::slice::from_ref(candidate),
        Binding::Dynamic(candidates) => candidates.as_slice(),
        Binding::None => return None,
    };
    // A built-in's global slot is its position in the registry, which is the
    // order of `functions`.
    let mut slots = candidates
        .iter()
        .map(|candidate| match candidate.var() {
            ResolvedVar::Global { slot } => Some(slot),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    // A broadcast candidate names the same overload as the scalar one.
    slots.dedup();
    let overloads = slots
        .into_iter()
        .map(|slot| functions.get(slot).map(function_markdown))
        .collect::<Option<Vec<_>>>()?;
    (!overloads.is_empty()).then(|| overloads.join("\n\n---\n\n"))
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

/// Markdown showing the inferred type of an expression.
fn type_hover(state: &DocumentState, id: NodeId) -> Option<String> {
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

    fn markdown(hover: Hover) -> String {
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        content.value
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
}
