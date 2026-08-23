#![allow(unused_crate_dependencies)]

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, Lvalue, Parser};

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

#[test]
fn member_assignment_has_a_member_lvalue() {
    let expression = parse_one("foo.bar = value");

    let Expression::Assignment { l_value, .. } = expression.expression else {
        panic!("expected assignment, found {:?}", expression.expression);
    };

    assert!(matches!(
        l_value,
        Lvalue::Member {
            member,
            receiver,
            ..
        } if member == "bar"
            && matches!(receiver.expression, Expression::Identifier { ref name, .. } if name == "foo")
    ));
}

#[test]
fn invoked_member_is_not_assignable() {
    let tokens = Lexer::new("foo.bar() = value", SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("source must lex");
    let error = Parser::from_tokens(tokens)
        .parse()
        .expect_err("invoked member must not be assignable");

    assert!(error.to_string().contains("Invalid assignment target"));
}

#[test]
fn member_destructuring_is_not_assignable() {
    let tokens = Lexer::new("[foo.bar] = values", SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("source must lex");
    let error = Parser::from_tokens(tokens)
        .parse()
        .expect_err("member destructuring must not be assignable");

    assert!(error.to_string().contains("Invalid assignment target"));
}

#[test]
fn member_augmented_assignment_has_a_member_lvalue() {
    let expression = parse_one("foo.bar += value");

    let Expression::OpAssignment {
        l_value, operation, ..
    } = expression.expression
    else {
        panic!(
            "expected augmented assignment, found {:?}",
            expression.expression
        );
    };

    assert_eq!(operation, "+");
    assert!(matches!(
        l_value,
        Lvalue::Member { member, .. } if member == "bar"
    ));
}

#[test]
fn member_is_not_a_declaration_target() {
    let tokens = Lexer::new("let foo.bar = value", SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("source must lex");
    let error = Parser::from_tokens(tokens)
        .parse()
        .expect_err("`let` must not accept a member target");

    assert!(error.to_string().contains("Invalid declaration target"));
}

#[test]
fn index_is_not_a_declaration_target() {
    let tokens = Lexer::new("let foo[0] = value", SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .expect("source must lex");
    let error = Parser::from_tokens(tokens)
        .parse()
        .expect_err("`let` must not accept an index target");

    assert!(error.to_string().contains("Invalid declaration target"));
}
