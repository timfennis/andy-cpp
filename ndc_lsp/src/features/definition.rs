use ndc_core::StaticType;
use ndc_lexer::{SourceId, Span};
use ndc_parser::Expression;
use tower_lsp::lsp_types::{Location, Position, Url};

use crate::scope_resolve::{Decl, collect_declarations, file_scope, is_visible, scope_len};
use crate::state::DocumentState;
use crate::visitor::{AstVisitor, node_at_offset, walk_ast};

/// Resolve the declaration of the identifier under the cursor.
///
/// Gathers every declaration with the scope region in which it is visible, keeps
/// only those visible at the cursor (enclosing scope + declared-before-use, with
/// functions hoisted), then picks the innermost-scoped one.
///
/// NOTE: this is name-only — it does not disambiguate function overloads, so a
/// call to one of several same-named functions resolves to whichever visible
/// declaration is innermost/latest rather than the overload the analyser actually
/// selected. The follow-up that exposes the analyser's resolution fixes this.
pub fn goto_definition(state: &DocumentState, position: Position, uri: Url) -> Option<Location> {
    let offset = state.line_index.offset(&state.source, position)?;
    let (name, source_id) = identifier_at(&state.ast, offset)?;

    let scope = file_scope(source_id, state.source.len());
    let decls = collect_declarations(&state.ast, scope);

    let best = decls
        .iter()
        .filter(|d| d.name == name && is_visible(d, offset))
        .reduce(|a, b| if better(b, a) { b } else { a })?;

    Some(Location {
        uri,
        range: state.line_index.range(&state.source, best.name_span),
    })
}

/// The identifier name the cursor is on, plus the document's `SourceId`.
///
/// Handles both expression uses (`x`) and assignment targets (`x = 2`). A
/// reassignment target is an [`ndc_parser::Lvalue`], not an expression node, so
/// `node_at_offset` returns the enclosing assignment for it; we then look up the
/// lvalue identifier directly. (Declaration/parameter/loop-variable lvalues are
/// found too, but they aren't visible to themselves, so they resolve to nothing.)
fn identifier_at(
    ast: &[ndc_parser::ExpressionLocation],
    offset: usize,
) -> Option<(String, SourceId)> {
    if let Some(node) = node_at_offset(ast, offset)
        && let Expression::Identifier { name, .. } = &node.expression
    {
        return Some((name.clone(), node.span.source_id()));
    }
    let mut finder = LvalueIdentFinder {
        offset,
        found: None,
    };
    walk_ast(&mut finder, ast);
    finder.found
}

/// Finds the lvalue identifier whose span contains the cursor. Lvalue
/// identifiers don't overlap, so the one containing the offset is unambiguous.
struct LvalueIdentFinder {
    offset: usize,
    found: Option<(String, SourceId)>,
}

impl AstVisitor for LvalueIdentFinder {
    fn on_declaration(
        &mut self,
        identifier: &str,
        _inferred_type: Option<&StaticType>,
        _has_annotation: bool,
        span: Span,
    ) {
        if self.offset >= span.offset() && self.offset < span.end() {
            self.found = Some((identifier.to_string(), span.source_id()));
        }
    }
}

/// Among declarations already known to be visible at the use, is `candidate` a
/// better match than `current`? Innermost scope wins; ties go to the most recent
/// declaration (handles shadowing within one scope).
fn better(candidate: &Decl, current: &Decl) -> bool {
    let (cand_len, cur_len) = (
        scope_len(candidate.scope_span),
        scope_len(current.scope_span),
    );
    if cand_len != cur_len {
        return cand_len < cur_len;
    }
    candidate.name_span.offset() > current.name_span.offset()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;

    fn analyse(source: &str) -> DocumentState {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        DocumentState::from_analysis(source.to_string(), ast, analysis)
    }

    fn uri() -> Url {
        Url::parse("file:///test.ndc").unwrap()
    }

    fn start_offset(state: &DocumentState, location: &Location) -> usize {
        state
            .line_index
            .offset(&state.source, location.range.start)
            .unwrap()
    }

    /// Resolve at the byte offset of the `needle`th occurrence of `name`.
    fn def_at(state: &DocumentState, byte: usize) -> Option<Location> {
        let pos = state.line_index.position(&state.source, byte);
        goto_definition(state, pos, uri())
    }

    #[test]
    fn jump_from_usage_to_let_declaration() {
        let src = "let total = 5;\ntotal + 1;";
        let state = analyse(src);
        let loc = def_at(&state, src.rfind("total").unwrap()).expect("definition found");
        assert_eq!(start_offset(&state, &loc), 4); // `total` decl at byte 4
    }

    #[test]
    fn jump_to_function_parameter() {
        let src = "fn square(n) { n * n }";
        let state = analyse(src);
        let loc = def_at(&state, src.rfind("n *").unwrap()).expect("definition found");
        assert_eq!(start_offset(&state, &loc), 10); // `n` parameter at byte 10
    }

    #[test]
    fn innermost_scope_shadows_outer() {
        let src = "let x = 1;\nfn f() { let x = 2; x }";
        let state = analyse(src);
        let inner_decl = src.rfind("x = 2").unwrap();
        let loc = def_at(&state, src.rfind('x').unwrap()).expect("definition found");
        assert_eq!(start_offset(&state, &loc), inner_decl);
    }

    #[test]
    fn use_before_inner_shadow_resolves_to_outer() {
        // Codex regression: the `x` use precedes the inner `let x = 2`, so it must
        // resolve to the outer `x = 1`, not the not-yet-visible inner declaration.
        let src = "let x = 1; { x; let x = 2; }";
        let state = analyse(src);
        let outer_decl = src.find("x = 1").unwrap();
        // The first `x` after the `{` is the use.
        let use_offset = src.find("{ x").unwrap() + 2;
        let loc = def_at(&state, use_offset).expect("definition found");
        assert_eq!(start_offset(&state, &loc), outer_decl);
    }

    #[test]
    fn let_binding_invisible_in_its_own_initializer() {
        // The RHS `x` must resolve to the outer (first) `x`, not the binding
        // currently being declared — the analyser binds after the initializer.
        let src = "let x = 1;\nlet x = x;";
        let state = analyse(src);
        let outer_decl = src.find("x").unwrap(); // first `x` at byte 4
        let rhs_use = src.rfind('x').unwrap(); // the `x` on the RHS of line 2
        let loc = def_at(&state, rhs_use).expect("definition found");
        assert_eq!(start_offset(&state, &loc), outer_decl);
    }

    #[test]
    fn jump_from_reassignment_target_to_declaration() {
        // The `x` in `x = 2` is a use in write position (an lvalue, not an
        // expression node), but should still jump to its declaration.
        let src = "let x = 1;\nx = 2;";
        let state = analyse(src);
        let target = src.rfind("x").unwrap(); // `x` in `x = 2` on line 2
        let loc = def_at(&state, target).expect("definition found");
        assert_eq!(start_offset(&state, &loc), 4); // `let x` at byte 4
    }
}
