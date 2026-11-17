#![allow(unused_crate_dependencies)]

use ndc_core::NodeId;

use std::collections::HashSet;

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, Parser, TypeExpr};

fn parse_one(source: &str) -> ExpressionLocation {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let mut expressions = Parser::from_tokens(tokens).parse().unwrap();
    assert_eq!(expressions.len(), 1);
    expressions.pop().unwrap()
}

fn names(annotation: &TypeExpr, found: &mut Vec<(String, NodeId)>) {
    match annotation {
        TypeExpr::Name { id, name, args, .. } => {
            found.push((name.clone(), *id));
            for arg in args {
                names(arg, found);
            }
        }
        TypeExpr::List { element, .. } => names(element, found),
        TypeExpr::Map { key, value, .. } => {
            names(key, found);
            if let Some(value) = value {
                names(value, found);
            }
        }
        TypeExpr::Tuple { elements, .. } => {
            for element in elements {
                names(element, found);
            }
        }
        TypeExpr::Function {
            parameters,
            return_type,
            ..
        } => {
            for parameter in parameters {
                names(parameter, found);
            }
            if let Some(return_type) = return_type {
                names(return_type, found);
            }
        }
    }
}

fn assert_distinct(ids: impl IntoIterator<Item = NodeId>) {
    let ids = ids.into_iter().collect::<Vec<_>>();
    let unique = ids.iter().collect::<HashSet<_>>();
    assert_eq!(unique.len(), ids.len(), "{ids:?}");
}

#[test]
fn every_type_name_has_its_own_id() {
    let expression = parse_one("let x: %{P: (Q, Option<P>)} = input");
    let Expression::VariableDeclaration {
        annotated_type: Some(annotation),
        ..
    } = expression.expression
    else {
        panic!("expected an annotated declaration");
    };

    let mut found = vec![];
    names(&annotation, &mut found);
    assert_eq!(
        found
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>(),
        ["P", "Q", "Option", "P"]
    );
    // Both `P`s are separate uses, so they get separate ids.
    assert_distinct(found.into_iter().map(|(_, id)| id));
}

#[test]
fn a_cast_target_before_a_comparison_gets_its_own_id() {
    // `Int < 5` is not a type argument list, so the parser reads `Int` on its own.
    let expression = parse_one("[x as Int < 5, y as Int < 6]");
    let Expression::List { values } = expression.expression else {
        panic!("expected a list");
    };

    let ids = values.iter().map(|value| {
        let (Expression::Call { arguments, .. }
        | Expression::OperatorCall { arguments, .. }
        | Expression::MethodCall { arguments, .. }) = &value.expression
        else {
            panic!("expected a comparison, got {value:?}");
        };
        let Expression::Cast {
            annotation: TypeExpr::Name { id, name, .. },
            ..
        } = &arguments[0].expression
        else {
            panic!("expected a cast to a named type");
        };
        assert_eq!(name, "Int");
        *id
    });
    assert_distinct(ids);
}

#[test]
fn a_function_type_has_parameters_and_an_optional_return_type() {
    for (source, shown) in [
        (
            "let f: fn(Int, String) -> Bool = g",
            "fn(Int, String) -> Bool",
        ),
        ("let f: fn() = g", "fn()"),
        (
            "let f: fn(fn(Int) -> Int) -> [Int] = g",
            "fn(fn(Int) -> Int) -> [Int]",
        ),
        (
            "fn h(f: fn(Int) -> Int, x: Int) -> fn(Int) -> Int { f }",
            "fn(Int) -> Int",
        ),
    ] {
        let expression = parse_one(source);
        let annotation = match expression.expression {
            Expression::VariableDeclaration {
                annotated_type: Some(annotation),
                ..
            } => annotation,
            Expression::FunctionDeclaration { parameters, .. } => parameters[0]
                .annotation
                .clone()
                .expect("the first parameter is annotated"),
            other => panic!("unexpected expression {other:?}"),
        };
        assert_eq!(annotation.to_string(), shown, "{source}");
    }
}

#[test]
fn list_and_tuple_types_are_written_like_their_values() {
    for (source, shown) in [
        ("let x: [Int] = y", "[Int]"),
        ("let x: [[Int]] = y", "[[Int]]"),
        ("let x: (Int, [String]) = y", "(Int, [String])"),
        ("let x: %{String: [Int]} = y", "%{String: [Int]}"),
        // A comma makes a 1-tuple; parentheses alone only group, as in `(1)`.
        ("let x: (Int,) = y", "(Int,)"),
        ("let x: (Int) = y", "Int"),
        ("let x: () = y", "()"),
        ("let x: %{String: [Int]} = y", "%{String: [Int]}"),
        ("let x: %{Int} = y", "%{Int}"),
        ("let x: fn(Int) -> %{Int} = y", "fn(Int) -> %{Int}"),
    ] {
        let Expression::VariableDeclaration {
            annotated_type: Some(annotation),
            ..
        } = parse_one(source).expression
        else {
            panic!("expected an annotated declaration");
        };
        assert_eq!(annotation.to_string(), shown, "{source}");
    }
}

#[test]
fn list_map_and_function_types_inside_type_arguments_are_still_a_cast() {
    for source in [
        "x as Option<[Int]>",
        "x as Option<%{String: Int}>",
        "x as Option<fn(Int)>",
    ] {
        let Expression::Cast { annotation, .. } = parse_one(source).expression else {
            panic!("expected a cast: {source}");
        };
        assert_eq!(format!("x as {annotation}"), source);
    }
}

#[test]
fn a_less_than_after_a_cast_in_a_map_literal_is_a_comparison() {
    let Expression::Map { values, .. } = parse_one("%{k: x as Int < y, j: z > 1}").expression
    else {
        panic!("expected a map literal");
    };
    assert_eq!(values.len(), 2);
}

#[test]
fn the_old_function_type_spelling_says_how_to_write_it() {
    let tokens = Lexer::new("let f: Fn(Int) -> Int = abs;", SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let error = Parser::from_tokens(tokens).parse().unwrap_err();
    assert!(error.to_string().contains("unknown type `Fn`"), "{error}");
}
