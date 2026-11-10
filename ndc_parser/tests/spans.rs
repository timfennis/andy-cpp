#![allow(unused_crate_dependencies)]

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, Parser};

fn parse_one(source: &str) -> ExpressionLocation {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let mut expressions = Parser::from_tokens(tokens).parse().unwrap();
    assert_eq!(expressions.len(), 1);
    expressions.pop().unwrap()
}

fn text(source: &str, expression: &ExpressionLocation) -> String {
    source[expression.span.range()].to_string()
}

#[test]
fn if_spans_from_its_keyword_to_its_last_branch() {
    let source = "if a { 1 }";
    assert_eq!(text(source, &parse_one(source)), source);

    let source = "if a { 1 } else if b { 2 } else { 3 }";
    let expression = parse_one(source);
    assert_eq!(text(source, &expression), source);
    let Expression::If { on_false, .. } = expression.expression else {
        panic!("expected an if");
    };
    let nested = on_false.expect("else branch");
    assert_eq!(text(source, &nested), "if b { 2 } else { 3 }");
}

#[test]
fn while_spans_from_its_keyword_to_its_body() {
    let source = "while a { 1 }";
    assert_eq!(text(source, &parse_one(source)), source);
}
