#![allow(unused_crate_dependencies)]

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, Parser};

fn parse_one(source: &str) -> ExpressionLocation {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("source must lex");
    let mut expressions = Parser::from_tokens(tokens)
        .parse()
        .expect("source must parse");
    assert_eq!(expressions.len(), 1);
    expressions.pop().unwrap()
}

#[test]
fn bare_dot_is_member_access() {
    let expression = parse_one("foo.bar");

    let Expression::MemberAccess {
        receiver, member, ..
    } = expression.expression
    else {
        panic!("expected member access, found {:?}", expression.expression);
    };

    assert_eq!(member, "bar");
    assert!(matches!(
        receiver.expression,
        Expression::Identifier { ref name, .. } if name == "foo"
    ));
}

#[test]
fn invoked_dot_is_call_with_receiver_as_first_argument() {
    let expression = parse_one("foo.bar()");

    let Expression::Call {
        function,
        arguments,
    } = expression.expression
    else {
        panic!("expected call, found {:?}", expression.expression);
    };

    assert!(matches!(
        function.expression,
        Expression::Identifier { ref name, .. } if name == "bar"
    ));
    assert!(matches!(
        arguments.as_slice(),
        [ExpressionLocation {
            expression: Expression::Identifier { name, .. },
            ..
        }] if name == "foo"
    ));
}
