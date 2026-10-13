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

#[test]
fn none_is_a_literal() {
    assert_eq!(parse_one("None").expression, Expression::NoneLiteral);
}

#[test]
fn none_cannot_be_a_binding_name() {
    for source in ["let None = 5", "fn f(None) { 1 }"] {
        let tokens = Lexer::new(source, SourceId::SYNTHETIC)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert!(Parser::from_tokens(tokens).parse().is_err(), "{source}");
    }
}
