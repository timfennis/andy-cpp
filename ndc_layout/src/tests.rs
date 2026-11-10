use crate::{Layout, Picks};
use Slot::{Global, Local};
use ndc_analysis::{Analysis, Binding, Candidate, CaptureSource, ResolvedVar};
use ndc_core::StaticType;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, Parser};
use ndc_resolver::{NameResolver, ScopeKind};

fn resolve_with_natives(source: &str, natives: &[&str]) -> NameResolver {
    let mut resolver = NameResolver::new(
        natives.iter().copied(),
        StaticType::BUILTIN_TYPE_NAMES.iter().copied(),
    );
    resolve_batch(&mut resolver, source);
    resolver
}

/// Resolves `source` as one more batch for `resolver` and returns its statements.
fn resolve_batch(resolver: &mut NameResolver, source: &str) -> Vec<ExpressionLocation> {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let statements = Parser::from_tokens(tokens).parse().expect("parse failed");
    for statement in &statements {
        resolver.resolve_statement(statement);
    }
    assert!(resolver.take_errors().is_empty(), "resolution failed");
    statements
}

fn resolve(source: &str) -> NameResolver {
    resolve_with_natives(source, &[])
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Slot {
    Global(usize),
    Local(usize),
}

/// Where each declared value lives, in declaration order.
fn slots_with_natives(source: &str, natives: &[&str]) -> Vec<(String, Slot)> {
    let resolver = resolve_with_natives(source, natives);
    let layout = Layout::new(&resolver);
    let mut symbols = resolver
        .scopes()
        .filter(|(_, scope)| scope.kind() != ScopeKind::Builtins)
        .flat_map(|(_, scope)| scope.bindings().iter().copied())
        .collect::<Vec<_>>();
    symbols.sort();
    symbols
        .into_iter()
        .map(|symbol| {
            let slot = match layout.global_slot(symbol) {
                Some(slot) => Global(slot),
                None => Local(layout.local_slot(symbol)),
            };
            (resolver.symbol_name(symbol).to_string(), slot)
        })
        .collect()
}

fn slots(source: &str) -> Vec<(String, Slot)> {
    slots_with_natives(source, &[])
}

/// Each function's captures, in the order the functions appear.
fn captures(source: &str) -> Vec<Vec<CaptureSource>> {
    let resolver = resolve(source);
    let layout = Layout::new(&resolver);
    resolver
        .scopes()
        .filter_map(|(_, scope)| match scope.kind() {
            ScopeKind::Function(function) => Some(layout.captures(function)),
            _ => None,
        })
        .collect()
}

fn named(slots: &[(&str, Slot)]) -> Vec<(String, Slot)> {
    slots
        .iter()
        .map(|&(name, slot)| (name.to_string(), slot))
        .collect()
}

#[test]
fn top_level_declarations_are_globals_in_declaration_order() {
    assert_eq!(
        slots("let a = 1; let b = 2; let c = 3;"),
        named(&[("a", Global(0)), ("b", Global(1)), ("c", Global(2))])
    );
}

#[test]
fn top_level_globals_come_after_the_natives() {
    assert_eq!(
        slots_with_natives("let a = 1;", &["len", "print"]),
        named(&[("a", Global(2))])
    );
}

#[test]
fn redeclaring_a_top_level_name_takes_a_new_global() {
    assert_eq!(
        slots("let x = 1; let x = 2;"),
        named(&[("x", Global(0)), ("x", Global(1))])
    );
}

#[test]
fn top_level_blocks_number_their_locals_from_zero() {
    assert_eq!(
        slots("let a = 1; { let b = 2; } let c = 3;"),
        named(&[("a", Global(0)), ("b", Local(0)), ("c", Global(1))])
    );
}

#[test]
fn block_slots_are_free_again_after_the_block() {
    assert_eq!(
        slots("fn f() { let a = 1; { let b = 2; } let c = 3; }"),
        named(&[
            ("f", Global(0)),
            ("a", Local(0)),
            ("b", Local(1)),
            ("c", Local(1))
        ])
    );
}

#[test]
fn nested_blocks_number_on_from_their_parent() {
    assert_eq!(
        slots("fn f() { let a = 1; { let b = 2; { let c = 3; } let d = 4; } let e = 5; }"),
        named(&[
            ("f", Global(0)),
            ("a", Local(0)),
            ("b", Local(1)),
            ("c", Local(2)),
            ("d", Local(2)),
            ("e", Local(1)),
        ])
    );
}

#[test]
fn a_function_frame_starts_at_zero_with_its_parameters() {
    assert_eq!(
        slots("let a = 1; fn f(x, y) { let z = x; }"),
        named(&[
            ("a", Global(0)),
            ("f", Global(1)),
            ("x", Local(0)),
            ("y", Local(1)),
            ("z", Local(2)),
        ])
    );
}

#[test]
fn for_binders_number_on_like_a_block() {
    assert_eq!(
        slots("fn f(xs) { for x in xs { let y = x; } let z = 1; }"),
        named(&[
            ("f", Global(0)),
            ("xs", Local(0)),
            ("x", Local(1)),
            ("y", Local(2)),
            ("z", Local(1)),
        ])
    );
}

#[test]
fn top_level_for_binders_are_locals() {
    assert_eq!(
        slots("let xs = 1; for x in xs { let y = x; }"),
        named(&[("xs", Global(0)), ("x", Local(0)), ("y", Local(1))])
    );
}

#[test]
fn a_struct_binds_its_constructor_then_each_getter_and_setter() {
    assert_eq!(
        slots("struct P { x: Int } let p = 1;"),
        named(&[
            ("P", Global(0)),
            ("x", Global(1)),
            ("x=", Global(2)),
            ("p", Global(3)),
        ])
    );
}

#[test]
fn top_level_values_are_not_captured() {
    assert_eq!(captures("let x = 1; fn f() { x }"), vec![vec![]]);
}

#[test]
fn captures_from_the_enclosing_frame_are_its_locals() {
    assert_eq!(
        captures("fn outer() { let a = 1; let b = 2; fn f() { b; a } }"),
        vec![
            vec![],
            vec![CaptureSource::Local(1), CaptureSource::Local(0)]
        ]
    );
}

#[test]
fn captures_skip_globals_without_shifting_the_others() {
    assert_eq!(
        captures("let g = 1; fn outer() { let x = 1; fn f() { g; x } }"),
        vec![vec![], vec![CaptureSource::Local(0)]]
    );
}

#[test]
fn captures_from_further_out_go_through_the_enclosing_upvalue() {
    assert_eq!(
        captures("fn outer() { let x = 1; fn f() { fn g() { x } } }"),
        vec![
            vec![],
            vec![CaptureSource::Local(0)],
            vec![CaptureSource::Upvalue(0)]
        ]
    );
}

#[test]
fn captures_from_a_block_of_the_enclosing_function_use_its_slot() {
    assert_eq!(
        captures("fn f() { let a = 1; { let b = 2; fn g() { b } } }"),
        vec![vec![], vec![CaptureSource::Local(1)]]
    );
}

/// Lays out a batch with no picks, which still sizes its frames.
fn lay_out_batch(resolver: &NameResolver, first_scope: usize) -> Analysis {
    let mut analysis = Analysis::default();
    Layout::new(resolver).lay_out(Picks::default(), first_scope, &mut analysis);
    analysis
}

/// Every node in `statements` that is an identifier named `name`.
fn identifiers<'a>(
    statements: &'a [ExpressionLocation],
    name: &str,
) -> Vec<&'a ExpressionLocation> {
    let mut found = Vec::new();
    let mut pending: Vec<&ExpressionLocation> = statements.iter().collect();
    while let Some(expression) = pending.pop() {
        if matches!(&expression.expression, Expression::Identifier { name: n, .. } if n == name) {
            found.push(expression);
        }
        pending.extend(expression.children());
    }
    found
}

#[test]
fn a_frame_is_as_large_as_its_deepest_block() {
    let mut resolver = resolve("");
    let statements = resolve_batch(&mut resolver, "fn f(a) { let b = 1; { let c = 2; } }");
    let analysis = lay_out_batch(&resolver, 0);
    assert_eq!(analysis.frame_size(statements[0].id), 3);
    assert_eq!(analysis.top_level_frame_size, 0);
}

#[test]
fn the_top_level_frame_holds_only_blocks_and_loops() {
    let mut resolver = resolve("");
    resolve_batch(
        &mut resolver,
        "let a = 1; { let b = 2; { let c = 3; } } for x in [] {}",
    );
    assert_eq!(lay_out_batch(&resolver, 0).top_level_frame_size, 2);
}

#[test]
fn a_batch_lays_out_only_the_scopes_it_created() {
    let mut resolver = resolve("");
    resolve_batch(
        &mut resolver,
        "fn f() { let a = 1; } { let b = 1; let c = 2; }",
    );
    let first_scope = resolver.scope_count();
    resolve_batch(&mut resolver, "{ let d = 1; }");

    let analysis = lay_out_batch(&resolver, first_scope);
    assert!(analysis.frame_sizes.is_empty());
    assert!(analysis.captures.is_empty());
    assert_eq!(analysis.top_level_frame_size, 1);
}

#[test]
fn a_pick_is_located_from_the_function_its_use_sits_in() {
    let mut resolver = resolve("");
    let statements = resolve_batch(&mut resolver, "fn f() { let x = 1; x; fn g() { x } }");
    let mut picks = Picks::default();
    for use_of_x in identifiers(&statements, "x") {
        let symbol = resolver.innermost_reference(use_of_x.id);
        picks
            .bindings
            .insert(use_of_x.id, Binding::Resolved(Candidate::Scalar(symbol)));
    }
    let mut analysis = Analysis::default();
    Layout::new(&resolver).lay_out(picks, 0, &mut analysis);

    let mut locations = analysis
        .bindings
        .values()
        .map(|binding| match binding {
            Binding::Resolved(candidate) => candidate.var(),
            other => panic!("expected one candidate, got {other:?}"),
        })
        .collect::<Vec<_>>();
    locations.sort_by_key(|location| matches!(location, ResolvedVar::Upvalue { .. }));
    assert_eq!(
        locations,
        [
            ResolvedVar::Cell { slot: 0 },
            ResolvedVar::Upvalue { slot: 0 }
        ]
    );
}
