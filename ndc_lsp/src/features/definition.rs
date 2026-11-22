use ls_types::{Position, Range};
use ndc_lexer::Span;

use crate::names::{Name, Refers};
use crate::resolved::Definition;
use std::cell::OnceCell;

use crate::scope_resolve::{Decl, DeclKind, collect_declarations, file_scope, is_visible};
use crate::state::DocumentState;

/// The name the cursor at `offset` is on, and what it refers to: what the
/// analyser resolved it to, so a call means the overload it picks, `p.x` the
/// field `x`, and a type name its struct or type parameter. A name in the
/// body of a generic function can mean one for each instance; a built-in
/// means none. A declaration's own name means what it declares.
pub fn definitions_at(state: &DocumentState, offset: usize) -> Option<(Name, Vec<Definition>)> {
    let name = *state.names.at(offset)?;
    // A name edited since the analysis may mean something else now.
    state.name_range(name.span)?;
    Some((name, Meanings::new(state).of(&name)))
}

/// What the names of one document refer to; see [`definitions_at`]. Keeps
/// what it works out about scopes, so asking about every name stays cheap.
pub struct Meanings<'a> {
    state: &'a DocumentState,
    /// Every declaration with where it's visible, made the first time a name
    /// the resolver didn't get to needs it.
    declarations: OnceCell<Vec<Decl>>,
}

impl<'a> Meanings<'a> {
    pub fn new(state: &'a DocumentState) -> Self {
        Self {
            state,
            declarations: OnceCell::new(),
        }
    }

    /// What `name` refers to.
    pub fn of(&self, name: &Name) -> Vec<Definition> {
        match name.refers {
            Refers::Declaration(definition) => vec![definition],
            Refers::Node(node) => match self.state.resolved.definitions_of(node) {
                Some(definitions) => definitions.to_vec(),
                // The resolver stops a statement at an unknown name, so a name
                // after one, as `total` in `print(missing, total)`, is found
                // by its scope.
                None => self.in_scope(name).into_iter().collect(),
            },
        }
    }

    /// The declaration of `name` visible where it's written, in the
    /// innermost scope, the latest there.
    fn in_scope(&self, name: &Name) -> Option<Definition> {
        let state = self.state;
        let text = state
            .analysed_text()
            .get(name.span.offset()..name.span.end())?;
        let declarations = self.declarations.get_or_init(|| match state.ast.first() {
            Some(first) => collect_declarations(
                &state.ast,
                file_scope(first.span.source_id(), state.analysed_len()),
            ),
            None => Vec::new(),
        });
        let scope_len = |span: Span| span.end() - span.offset();
        declarations
            .iter()
            .filter(|decl| decl.name == text && is_visible(decl, name.span.offset()))
            .map(|decl| {
                let (DeclKind::Variable(id) | DeclKind::Function(id) | DeclKind::Struct(id)) =
                    decl.kind;
                (scope_len(decl.scope_span), decl.visible_from, id)
            })
            // Innermost scope first, then the latest declaration in it.
            .min_by_key(|&(len, from, _)| (len, std::cmp::Reverse(from)))
            .map(|(_, _, id)| Definition::Node(id))
    }
}

/// Where the name under the cursor is declared, in the order they appear.
pub fn goto_definition(state: &DocumentState, position: Position) -> Vec<Range> {
    let Some(offset) = state.offset_at(position) else {
        return Vec::new();
    };
    let Some((_, definitions)) = definitions_at(state, offset) else {
        return Vec::new();
    };
    let mut spans: Vec<Span> = definitions
        .into_iter()
        .filter_map(|definition| state.names.declaration(definition))
        .collect();
    spans.sort_by_key(|span| span.offset());
    spans
        .into_iter()
        .filter_map(|span| state.name_range(span))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn analyse(source: &str) -> DocumentState {
        DocumentState::analysed(source)
    }

    fn start_offset(state: &DocumentState, location: &Range) -> usize {
        state
            .line_index
            .offset(&state.source, location.start)
            .unwrap()
    }

    /// Does `location` cover the `len` bytes at `offset`?
    fn covers(state: &DocumentState, location: &Range, offset: usize, len: usize) -> bool {
        let start = start_offset(state, location);
        let end = state
            .line_index
            .offset(&state.source, location.end)
            .unwrap();
        start <= offset && offset + len <= end
    }

    /// The one definition at `byte`, if there's exactly one.
    fn def_at(state: &DocumentState, byte: usize) -> Option<Range> {
        let pos = state.line_index.position(&state.source, byte);
        let mut definitions = goto_definition(state, pos);
        assert!(definitions.len() <= 1, "{definitions:?}");
        definitions.pop()
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
    fn a_constructor_call_jumps_to_its_struct_declaration() {
        let src = "struct Point {\n    x: Int,\n    y: Int,\n}\nlet p = Point(1, 2);";
        let state = analyse(src);
        let loc = def_at(&state, src.rfind("Point").unwrap()).expect("definition found");
        assert!(covers(
            &state,
            &loc,
            src.find("Point").unwrap(),
            "Point".len()
        ));
    }

    #[test]
    fn struct_not_visible_before_declaration() {
        let src = "let p = Point(1, 2);\nstruct Point { x: Int }";
        let state = analyse(src);
        // Analysis reports an error, but go-to-def is structural: the struct
        // is not hoisted, so the use before the declaration finds nothing.
        assert!(def_at(&state, src.find("Point").unwrap()).is_none());
    }

    #[test]
    fn jump_from_inside_an_if_body() {
        let src = "let total = 5;\nif true { total + 1 }";
        let state = analyse(src);
        let loc = def_at(&state, src.rfind("total").unwrap()).expect("definition found");
        assert_eq!(start_offset(&state, &loc), 4);
    }

    #[test]
    fn function_not_visible_before_declaration() {
        // Functions aren't hoisted, so the call before the declaration (an
        // analysis error) finds nothing.
        let src = "f();\nfn f() { 1 }";
        let state = analyse(src);
        assert!(def_at(&state, src.find('f').unwrap()).is_none());
    }

    #[test]
    fn a_recursive_call_jumps_to_its_own_function() {
        let src = "fn count(n) { if n > 0 { count(n - 1) } }";
        let state = analyse(src);
        let loc = def_at(&state, src.rfind("count").unwrap()).expect("definition found");
        assert!(covers(
            &state,
            &loc,
            src.find("count").unwrap(),
            "count".len()
        ));
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
        // The `x` in `x = 2` is a use in write position (an assignment target, not an
        // expression node), but should still jump to its declaration.
        let src = "let x = 1;\nx = 2;";
        let state = analyse(src);
        let target = src.rfind("x").unwrap(); // `x` in `x = 2` on line 2
        let loc = def_at(&state, target).expect("definition found");
        assert_eq!(start_offset(&state, &loc), 4); // `let x` at byte 4
    }

    #[test]
    fn grouped_destructuring_target_resolves_to_precise_binding_span() {
        let src = "let ((left), [right]) = (1, [2]);\n((left), [right]) = (3, [4]);";
        let state = analyse(src);
        for name in ["left", "right"] {
            let loc = def_at(&state, src.rfind(name).unwrap()).expect("definition found");
            assert_eq!(start_offset(&state, &loc), src.find(name).unwrap());
            assert_eq!(
                state.line_index.offset(&state.source, loc.end),
                Some(src.find(name).unwrap() + name.len()),
            );
        }
    }

    #[test]
    fn names_in_an_indexed_assignment_target_jump_to_their_declarations() {
        let src = "let values = [1]; let index = 0; values[index] = 2;";
        let state = analyse(src);
        for name in ["values", "index"] {
            let loc = def_at(&state, src.rfind(name).unwrap()).expect("definition found");
            assert_eq!(start_offset(&state, &loc), src.find(name).unwrap());
        }
    }

    #[test]
    fn a_for_binding_is_visible_in_the_body_but_not_in_its_own_sequence() {
        let src = "let x = [1];\nfor x in x { x }";
        let state = analyse(src);
        let outer = src.find("x =").unwrap();
        let binder = src.find("for x").unwrap() + "for ".len();

        let sequence = src.find("in x").unwrap() + "in ".len();
        let loc = def_at(&state, sequence).expect("definition found");
        assert_eq!(start_offset(&state, &loc), outer);

        let body = src.rfind('x').unwrap();
        let loc = def_at(&state, body).expect("definition found");
        assert_eq!(start_offset(&state, &loc), binder);
    }

    mod resolved {
        use super::*;

        /// Every definition of the name at the `n`th `needle`, as the line
        /// it's on. Each covers just the declared name.
        fn definitions(src: &str, needle: &str, n: usize) -> Vec<String> {
            let state = analyse(src);
            let byte = src.match_indices(needle).nth(n).expect("needle").0;
            let position = state.line_index.position(&state.source, byte);
            goto_definition(&state, position)
                .iter()
                .map(|range| {
                    let start = state.line_index.offset(&state.source, range.start).unwrap();
                    let end = state.line_index.offset(&state.source, range.end).unwrap();
                    let name = &src[start..end];
                    assert!(
                        name.chars().all(|c| c.is_alphanumeric() || c == '_'),
                        "not just a name: {name:?}"
                    );
                    src.lines()
                        .nth(range.start.line as usize)
                        .expect("a line")
                        .to_string()
                })
                .collect()
        }

        #[test]
        fn a_call_jumps_to_the_overload_it_picks() {
            // Matching by name would find the newest `f`, the String one.
            let src = "fn f(x: Int) { 1 }\nfn f(x: String) { 2 }\nf(1);";
            assert_eq!(definitions(src, "f(1", 0), ["fn f(x: Int) { 1 }"]);
        }

        #[test]
        fn a_call_with_a_dot_jumps_to_its_function() {
            let src = "fn double(x: Int) { x * 2 }\n5.double();";
            assert_eq!(
                definitions(src, "double", 1),
                ["fn double(x: Int) { x * 2 }"]
            );
        }

        #[test]
        fn a_field_read_or_written_jumps_to_the_field() {
            let src = "struct Point { x: Int, y: Int }\nlet p = Point(1, 2);\np.y;\np.y = 3;";
            assert_eq!(
                definitions(src, "y", 1),
                ["struct Point { x: Int, y: Int }"]
            );
            assert_eq!(
                definitions(src, "y", 2),
                ["struct Point { x: Int, y: Int }"]
            );
        }

        #[test]
        fn a_type_name_in_an_annotation_jumps_to_its_declaration() {
            let src = "struct Point { x: Int }\nlet p: Point = Point(1);";
            assert_eq!(definitions(src, "Point", 1), ["struct Point { x: Int }"]);
            let src = "fn same<T>(x: T) -> T { x }";
            assert_eq!(definitions(src, "T", 1), ["fn same<T>(x: T) -> T { x }"]);
            assert_eq!(definitions(src, "T", 2), ["fn same<T>(x: T) -> T { x }"]);
        }

        #[test]
        fn a_call_in_a_generic_function_jumps_to_what_each_instance_picks() {
            let src =
                "fn g(x: Int) { 1 }\nfn g(x: String) { 2 }\nfn h(v) { g(v) }\nh(1);\nh(\"a\");";
            assert_eq!(
                definitions(src, "g(v)", 0),
                ["fn g(x: Int) { 1 }", "fn g(x: String) { 2 }"]
            );
        }

        #[test]
        fn an_assignment_target_jumps_to_its_declaration_despite_an_error_in_its_value() {
            let src = "let total = 1;\ntotal = missing;";
            assert_eq!(definitions(src, "total", 1), ["let total = 1;"]);
        }

        #[test]
        fn a_name_after_an_unknown_name_in_the_same_statement_jumps_by_scope() {
            let src = "let total = 1;\nprint(missing, total);";
            assert_eq!(definitions(src, "total", 1), ["let total = 1;"]);
            let src = "let total = 1;\nif missing { total }";
            assert_eq!(definitions(src, "total", 1), ["let total = 1;"]);
            let src = "let total = 1;\nfn f() {\n  let total = 2;\n  if missing { total }\n}";
            assert_eq!(definitions(src, "total", 2), ["  let total = 2;"]);
            assert_eq!(
                start_offset(
                    &analyse(src),
                    &def_at(&analyse(src), src.rfind("total").unwrap()).unwrap()
                ),
                src.match_indices("total").nth(1).unwrap().0
            );
        }

        #[test]
        fn several_definitions_come_in_the_order_they_are_written() {
            let src =
                "fn g(x: Int) { 1 }\nfn g(x: String) { 2 }\nfn h(v) { g(v) }\nh(\"a\");\nh(1);";
            assert_eq!(
                definitions(src, "g(v)", 0),
                ["fn g(x: Int) { 1 }", "fn g(x: String) { 2 }"]
            );
        }

        #[test]
        fn a_built_in_has_no_definition_even_beside_a_function_of_the_same_name() {
            let src = "struct P { x: Int }\nfn len(p: P) { 1 }\nlen([1]);";
            assert!(definitions(src, "len", 1).is_empty());
        }
    }
}
