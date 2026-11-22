use ls_types::{Position, Range};
use ndc_core::NodeId;
use ndc_core::collections::HashMap;
use ndc_lexer::Span;
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, Expression, ExpressionLocation, TypeExpr,
};

use crate::resolved::Definition;
use crate::scope_resolve::{DeclKind, collect_declarations, file_scope, is_visible};
use crate::state::DocumentState;
use crate::visitor::{AstVisitor, node_at_offset, walk_ast};

/// Where the declarations of the analysed AST are, so a [`Definition`] can be
/// shown: by node, and each struct's fields by its declaration.
#[derive(Default)]
pub struct DeclarationSpans {
    nodes: HashMap<NodeId, Span>,
    fields: HashMap<NodeId, Vec<Span>>,
}

impl DeclarationSpans {
    pub fn collect(ast: &[ExpressionLocation]) -> Self {
        let mut spans = Self::default();
        walk_ast(&mut spans, ast);
        spans
    }

    fn span(&self, definition: Definition) -> Option<Span> {
        match definition {
            Definition::Node(node) => self.nodes.get(&node).copied(),
            Definition::Field { declaration, index } => {
                self.fields.get(&declaration)?.get(index).copied()
            }
        }
    }
}

impl AstVisitor for DeclarationSpans {
    fn on_declaration(&mut self, _identifier: &str, node: NodeId, _: bool, span: Span) {
        self.nodes.insert(node, span);
    }

    fn on_expression(&mut self, expr: &ExpressionLocation) {
        match &expr.expression {
            // Neither has a span of its own for its name yet.
            Expression::FunctionDeclaration {
                type_parameters, ..
            } => {
                self.nodes.insert(expr.id, expr.span);
                for parameter in type_parameters {
                    self.nodes.insert(parameter.id, parameter.span);
                }
            }
            Expression::StructDeclaration { fields, .. } => {
                self.nodes.insert(expr.id, expr.span);
                let spans = fields.iter().map(|field| field.span).collect();
                self.fields.insert(expr.id, spans);
            }
            _ => {}
        }
    }
}

/// Where the name under the cursor is declared, in the order they appear:
/// what the analyser resolved it to, so a call jumps to the overload it
/// picks, `p.x` to the field `x`, and a type name in an annotation to its
/// struct or type parameter. A name in the body of a generic function can
/// have one for each instance; a built-in has none.
pub fn goto_definition(state: &DocumentState, position: Position) -> Vec<Range> {
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return Vec::new();
    };
    let Some(node) = name_at(&state.ast, offset) else {
        return Vec::new();
    };
    let mut spans: Vec<Span> = match state.resolved.definitions_of(node) {
        Some(definitions) => definitions
            .iter()
            .filter_map(|&definition| state.declarations.span(definition))
            .collect(),
        // The resolver stops a statement at an unknown name, so a name after
        // one, as `total` in `print(missing, total)`, is found by its scope.
        None => in_scope(state, node, offset).into_iter().collect(),
    };
    spans.sort_by_key(|span| span.offset());
    spans
        .into_iter()
        .map(|span| state.line_index.range(&state.source, span))
        .collect()
}

/// The declaration of the identifier at `node` that is visible at `offset`
/// in the innermost scope, the latest there.
fn in_scope(state: &DocumentState, node: NodeId, offset: usize) -> Option<Span> {
    let name = identifier_name(&state.ast, node)?;
    let first = state.ast.first()?;
    let scope = file_scope(first.span.source_id(), state.source.len());
    let scope_len = |span: Span| span.end() - span.offset();
    collect_declarations(&state.ast, scope)
        .into_iter()
        .filter(|decl| decl.name == name && is_visible(decl, offset))
        .filter_map(|decl| {
            let (DeclKind::Variable(id) | DeclKind::Function(id) | DeclKind::Struct(id)) =
                decl.kind;
            let span = state.declarations.span(Definition::Node(id))?;
            Some((scope_len(decl.scope_span), span))
        })
        // Innermost scope first, then the latest declaration in it.
        .min_by_key(|&(len, span)| (len, std::cmp::Reverse(span.offset())))
        .map(|(_, span)| span)
}

/// The name of the identifier expression at `node`.
fn identifier_name(ast: &[ExpressionLocation], node: NodeId) -> Option<String> {
    let mut stack: Vec<&ExpressionLocation> = ast.iter().collect();
    while let Some(expr) = stack.pop() {
        if expr.id == node {
            return match &expr.expression {
                Expression::Identifier { name, .. } => Some(name.clone()),
                _ => None,
            };
        }
        stack.extend(expr.children());
    }
    None
}

/// The node of the name the cursor is on, as the analysis keys its
/// definitions: an identifier, the member of a member access, a binding
/// pattern or assignment target, or a name in a type annotation.
fn name_at(ast: &[ExpressionLocation], offset: usize) -> Option<NodeId> {
    let mut finder = NameFinder {
        offset,
        found: None,
    };
    walk_ast(&mut finder, ast);
    if finder.found.is_some() {
        return finder.found;
    }
    let node = node_at_offset(ast, offset)?;
    match &node.expression {
        Expression::Identifier {
            identifier_span, ..
        } if contains(*identifier_span, offset) => Some(node.id),
        Expression::MemberAccess { member_span, .. } if contains(*member_span, offset) => {
            Some(node.id)
        }
        _ => None,
    }
}

fn contains(span: Span, offset: usize) -> bool {
    span.offset() <= offset && offset < span.end()
}

/// Finds the binding pattern, assignment target or type name the cursor is
/// on. Their spans don't overlap each other or an expression's name, so the
/// first one found is the one.
struct NameFinder {
    offset: usize,
    found: Option<NodeId>,
}

impl NameFinder {
    /// The member of a target that writes through a field, as `x` in
    /// `p.x = 1`. A plain name is reported as a declaration.
    fn in_target(&mut self, target: &AssignmentTargetLocation) {
        match &target.target {
            AssignmentTarget::Member { member_span, .. } if contains(*member_span, self.offset) => {
                self.found = Some(target.id);
            }
            AssignmentTarget::Sequence(targets) => {
                for target in targets {
                    self.in_target(target);
                }
            }
            _ => {}
        }
    }

    fn in_type(&mut self, annotation: &TypeExpr) {
        match annotation {
            TypeExpr::Name {
                id,
                name,
                args,
                span,
            } => {
                let name_span = Span::new(span.source_id(), span.offset(), name.len());
                if contains(name_span, self.offset) {
                    self.found = Some(*id);
                }
                for arg in args {
                    self.in_type(arg);
                }
            }
            TypeExpr::List { element, .. } => self.in_type(element),
            TypeExpr::Map { key, value, .. } => {
                self.in_type(key);
                if let Some(value) = value {
                    self.in_type(value);
                }
            }
            TypeExpr::Tuple { elements, .. } => {
                for element in elements {
                    self.in_type(element);
                }
            }
            TypeExpr::Function {
                parameters,
                return_type,
                ..
            } => {
                for p in parameters {
                    self.in_type(p);
                }
                if let Some(return_type) = return_type {
                    self.in_type(return_type);
                }
            }
        }
    }
}

impl AstVisitor for NameFinder {
    fn on_declaration(&mut self, _identifier: &str, node: NodeId, _: bool, span: Span) {
        if contains(span, self.offset) {
            self.found = Some(node);
        }
    }

    fn on_expression(&mut self, expr: &ExpressionLocation) {
        if !contains(expr.span, self.offset) {
            return;
        }
        match &expr.expression {
            Expression::Identifier { type_arguments, .. } => {
                for t in type_arguments {
                    self.in_type(t);
                }
            }
            Expression::VariableDeclaration {
                annotated_type: Some(annotation),
                ..
            } => self.in_type(annotation),
            Expression::FunctionDeclaration {
                type_parameters,
                parameters,
                return_annotation,
                ..
            } => {
                for parameter in type_parameters {
                    if contains(parameter.span, self.offset) {
                        self.found = Some(parameter.id);
                    }
                }
                for annotation in parameters.iter().filter_map(|p| p.annotation.as_ref()) {
                    self.in_type(annotation);
                }
                if let Some(annotation) = return_annotation {
                    self.in_type(annotation);
                }
            }
            Expression::StructDeclaration { fields, .. } => {
                for field in fields {
                    self.in_type(&field.annotation);
                }
            }
            Expression::Assignment { l_value, .. } | Expression::OpAssignment { l_value, .. } => {
                self.in_target(l_value);
            }
            _ => {}
        }
    }
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

        /// Every definition of the name at the `n`th `needle`, as the text
        /// each range covers.
        fn definitions(src: &str, needle: &str, n: usize) -> Vec<String> {
            let state = analyse(src);
            let byte = src.match_indices(needle).nth(n).expect("needle").0;
            let position = state.line_index.position(&state.source, byte);
            goto_definition(&state, position)
                .iter()
                .map(|range| {
                    let start = state.line_index.offset(&state.source, range.start).unwrap();
                    let end = state.line_index.offset(&state.source, range.end).unwrap();
                    src[start..end].to_string()
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
            assert_eq!(definitions(src, "y", 1), ["y: Int"]);
            assert_eq!(definitions(src, "y", 2), ["y: Int"]);
        }

        #[test]
        fn a_type_name_in_an_annotation_jumps_to_its_declaration() {
            let src = "struct Point { x: Int }\nlet p: Point = Point(1);";
            assert_eq!(definitions(src, "Point", 1), ["struct Point { x: Int }"]);
            let src = "fn same<T>(x: T) -> T { x }";
            assert_eq!(definitions(src, "T", 1), ["T"]);
            assert_eq!(definitions(src, "T", 2), ["T"]);
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
            assert_eq!(definitions(src, "total", 1), ["total"]);
        }

        #[test]
        fn a_name_after_an_unknown_name_in_the_same_statement_jumps_by_scope() {
            let src = "let total = 1;\nprint(missing, total);";
            assert_eq!(definitions(src, "total", 1), ["total"]);
            let src = "let total = 1;\nif missing { total }";
            assert_eq!(definitions(src, "total", 1), ["total"]);
            let src = "let total = 1;\nfn f() {\n  let total = 2;\n  if missing { total }\n}";
            assert_eq!(definitions(src, "total", 2), ["total"]);
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
