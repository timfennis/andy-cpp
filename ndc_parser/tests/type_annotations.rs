#![allow(unused_crate_dependencies)]

use std::collections::HashSet;

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::{Expression, ExpressionLocation, NodeId, Parser, TypeExpr};

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
        TypeExpr::Tuple { elements, .. } => {
            for element in elements {
                names(element, found);
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
    let expression = parse_one("let x: Map<P, (Q, List<P>)> = input");
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
        ["Map", "P", "Q", "List", "P"]
    );
    // Both `P`s are separate uses, so they get separate ids.
    assert_distinct(found.into_iter().map(|(_, id)| id));
}

#[test]
fn cast_targets_before_a_comparison_have_ids() {
    // `Int < 5` is not a type argument list, so the parser reads `Int` on its own.
    let expression = parse_one("[x as Int < 5, y as Int < 6]");
    let Expression::List { values } = expression.expression else {
        panic!("expected a list");
    };

    let ids = values.iter().map(|value| {
        let (Expression::Call { arguments, .. } | Expression::OperatorCall { arguments, .. }) =
            &value.expression
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
