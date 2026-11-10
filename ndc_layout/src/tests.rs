use crate::Layout;
use ndc_analysis::CaptureSource;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_resolver::{NameResolver, ScopeKind};

fn resolve(source: &str) -> NameResolver {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("lex failed");
    let statements = Parser::from_tokens(tokens).parse().expect("parse failed");
    let mut resolver = NameResolver::new([]);
    for statement in &statements {
        resolver.resolve_statement(statement);
    }
    assert!(resolver.take_errors().is_empty(), "resolution failed");
    resolver
}

/// Each declared value's name and slot, in declaration order.
fn slots(source: &str) -> Vec<(String, usize)> {
    let resolver = resolve(source);
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
            (
                resolver.symbol_name(symbol).to_string(),
                layout.local_slot(symbol),
            )
        })
        .collect()
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

fn named(slots: &[(&str, usize)]) -> Vec<(String, usize)> {
    slots
        .iter()
        .map(|&(name, slot)| (name.to_string(), slot))
        .collect()
}

#[test]
fn slots_follow_declaration_order() {
    assert_eq!(
        slots("let a = 1; let b = 2; let c = 3;"),
        named(&[("a", 0), ("b", 1), ("c", 2)])
    );
}

#[test]
fn redeclaring_a_name_takes_a_new_slot() {
    assert_eq!(slots("let x = 1; let x = 2;"), named(&[("x", 0), ("x", 1)]));
}

#[test]
fn block_slots_are_free_again_after_the_block() {
    assert_eq!(
        slots("let a = 1; { let b = 2; } let c = 3;"),
        named(&[("a", 0), ("b", 1), ("c", 1)])
    );
}

#[test]
fn nested_blocks_number_on_from_their_parent() {
    assert_eq!(
        slots("let a = 1; { let b = 2; { let c = 3; } let d = 4; } let e = 5;"),
        named(&[("a", 0), ("b", 1), ("c", 2), ("d", 2), ("e", 1)])
    );
}

#[test]
fn a_function_frame_starts_at_zero_with_its_parameters() {
    assert_eq!(
        slots("let a = 1; fn f(x, y) { let z = x; }"),
        named(&[("a", 0), ("f", 1), ("x", 0), ("y", 1), ("z", 2)])
    );
}

#[test]
fn for_binders_number_on_like_a_block() {
    assert_eq!(
        slots("let xs = 1; for x in xs { let y = x; } let z = 1;"),
        named(&[("xs", 0), ("x", 1), ("y", 2), ("z", 1)])
    );
}

#[test]
fn a_struct_binds_its_constructor_then_each_getter_and_setter() {
    assert_eq!(
        slots("struct P { x: Int } let p = 1;"),
        named(&[("P", 0), ("x", 1), ("x=", 2), ("p", 3)])
    );
}

#[test]
fn captures_from_the_enclosing_frame_are_its_locals() {
    assert_eq!(
        captures("let a = 1; let b = 2; fn f() { b; a }"),
        vec![vec![CaptureSource::Local(1), CaptureSource::Local(0)]]
    );
}

#[test]
fn captures_from_further_out_go_through_the_enclosing_upvalue() {
    assert_eq!(
        captures("let x = 1; fn f() { fn g() { x } }"),
        vec![
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
