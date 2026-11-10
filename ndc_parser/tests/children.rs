#![allow(unused_crate_dependencies)]

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;

/// The source text of each direct child of the one expression in `source`.
fn children(source: &str) -> Vec<&str> {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("source must lex");
    let expressions = Parser::from_tokens(tokens)
        .parse()
        .expect("source must parse");
    let [expression] = expressions.as_slice() else {
        panic!("expected one expression in {source:?}");
    };
    expression
        .children()
        .into_iter()
        .map(|child| &source[child.span.range()])
        .collect()
}

#[test]
fn an_assignment_target_contributes_its_expressions() {
    assert_eq!(children("xs[i] = f(x)"), ["xs", "i", "f(x)"]);
    assert_eq!(children("p.x += 1"), ["p", "1"]);
    assert_eq!(children("a, b = pair"), ["pair"]);
}

#[test]
fn for_children_are_its_sequences_then_its_body() {
    assert_eq!(children("for x in xs { x }"), ["xs", "{ x }"]);
}

#[test]
fn a_function_declaration_has_only_its_body_as_a_child() {
    assert_eq!(children("fn f(a: Int) -> Int { a }"), ["{ a }"]);
}

#[test]
fn declarations_without_expressions_have_no_children() {
    assert!(children("struct P { x: Int }").is_empty());
    assert!(children("x").is_empty());
}
