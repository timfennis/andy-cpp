use crate::Layout;
use Slot::{Global, Local};
use ndc_analysis::CaptureSource;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_resolver::{NameResolver, ScopeKind};

fn resolve_with_natives(source: &str, natives: &[&str]) -> NameResolver {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let statements = Parser::from_tokens(tokens).parse().expect("parse failed");
    let mut resolver = NameResolver::new(natives.iter().copied());
    for statement in &statements {
        resolver.resolve_statement(statement);
    }
    assert!(resolver.take_errors().is_empty(), "resolution failed");
    resolver
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
