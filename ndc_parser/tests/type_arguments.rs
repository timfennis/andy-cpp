#![allow(unused_crate_dependencies)]

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, Parser};

fn parse(source: &str) -> Result<Vec<ExpressionLocation>, ndc_parser::Error> {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    Parser::from_tokens(tokens).parse()
}

fn parse_one(source: &str) -> ExpressionLocation {
    let mut expressions = parse(source).unwrap();
    assert_eq!(expressions.len(), 1);
    expressions.pop().unwrap()
}

/// The type arguments of the identifier `expression` is, as written.
fn type_arguments(expression: &ExpressionLocation) -> Vec<String> {
    let Expression::Identifier { type_arguments, .. } = &expression.expression else {
        panic!("expected an identifier, got {expression:?}");
    };
    type_arguments.iter().map(ToString::to_string).collect()
}

#[test]
fn a_call_takes_type_arguments_after_its_name() {
    let Expression::Call { function, .. } = parse_one("f::<Int, [String]>(x)").expression else {
        panic!("expected a call");
    };
    assert_eq!(type_arguments(&function), ["Int", "[String]"]);
}

#[test]
fn a_name_with_type_arguments_is_a_value() {
    let name = parse_one("f::<Option<%{String: Int}>>");
    assert_eq!(type_arguments(&name), ["Option<%{String: Int}>"]);
}

#[test]
fn a_method_call_takes_type_arguments_after_its_name() {
    let Expression::MethodCall {
        function,
        arguments,
    } = parse_one("x.f::<Int>(1)").expression
    else {
        panic!("expected a method call");
    };
    assert_eq!(type_arguments(&function), ["Int"]);
    assert_eq!(arguments.len(), 2);
}

#[test]
fn a_less_than_after_a_call_with_type_arguments_compares() {
    for (source, values) in [
        ("f(id::<Int>(1) < y, z > 1)", 2),
        ("(id::<Int>(1) < y, z > 1)", 2),
        ("[id::<Int>(1) < y, z > 1]", 2),
        ("[id::<Int>(1) < y] > [false]", 1),
    ] {
        let expression = parse_one(source);
        let found = match &expression.expression {
            Expression::Call { arguments, .. } => arguments.len(),
            Expression::Tuple { values } | Expression::List { values } => values.len(),
            Expression::OperatorCall { arguments, .. } => match &arguments[0].expression {
                Expression::List { values } => values.len(),
                other => panic!("expected a list, got {other:?}"),
            },
            other => panic!("unexpected {other:?}"),
        };
        assert_eq!(found, values, "{source}");
    }
}

#[test]
fn a_function_declares_type_parameters_after_its_name() {
    let Expression::FunctionDeclaration {
        type_parameters, ..
    } = parse_one("fn f<T, U>(x: T, y: U) -> T { x }").expression
    else {
        panic!("expected a function declaration");
    };
    let names = type_parameters
        .iter()
        .map(|parameter| parameter.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(names, ["T", "U"]);
}

#[test]
fn type_arguments_after_a_member_go_with_a_call() {
    let error = parse("x.f::<Int>;").unwrap_err();
    assert!(error.to_string().contains("expected '('"), "{error}");
}

#[test]
fn two_colons_start_type_arguments() {
    let error = parse("f::(x);").unwrap_err();
    assert!(
        error.to_string().contains("expected '<' after '::'"),
        "{error}"
    );
}

#[test]
fn type_arguments_are_not_empty() {
    assert!(parse("f::<>(x);").is_err());
    assert!(parse("fn f<>(x) => x;").is_err());
}
