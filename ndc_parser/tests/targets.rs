#![allow(unused_crate_dependencies)]

use std::collections::HashSet;

use ndc_lexer::{Lexer, SourceId, Span};
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForIteration, NodeId, Parser,
};

fn parse_one(source: &str) -> ExpressionLocation {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let mut expressions = Parser::from_tokens(tokens).parse().unwrap();
    assert_eq!(expressions.len(), 1);
    expressions.pop().unwrap()
}

fn text(source: &str, span: Span) -> &str {
    assert_eq!(span.source_id(), SourceId::SYNTHETIC);
    &source[span.range()]
}

fn pattern_nodes(pattern: &BindingPatternLocation, nodes: &mut Vec<(NodeId, Span)>) {
    nodes.push((pattern.id, pattern.span));
    if let BindingPattern::Sequence(items) = &pattern.pattern {
        for item in items {
            pattern_nodes(item, nodes);
        }
    }
}

#[test]
fn nested_binding_patterns_have_distinct_ids_and_complete_spans() {
    let source = "let [(a), (b, [c])] = input";
    let expression = parse_one(source);
    let Expression::VariableDeclaration { l_value, value, .. } = expression.expression else {
        panic!("expected declaration");
    };
    let mut nodes = vec![];
    pattern_nodes(&l_value, &mut nodes);
    assert_eq!(
        nodes
            .iter()
            .map(|(_, span)| text(source, *span))
            .collect::<Vec<_>>(),
        ["[(a), (b, [c])]", "(a)", "(b, [c])", "b", "[c]", "c"]
    );
    let ids: HashSet<_> = nodes
        .iter()
        .map(|(id, _)| *id)
        .chain([expression.id, value.id])
        .collect();
    assert_eq!(ids.len(), nodes.len() + 2);
    let BindingPattern::Sequence(items) = l_value.pattern else {
        unreachable!()
    };
    let BindingPattern::Identifier { span, .. } = items[0].pattern else {
        unreachable!()
    };
    assert_eq!(text(source, span), "a");
}

#[test]
fn assignment_conversion_preserves_ids_and_index_expression_children() {
    let source = "[(a), values[next()]]";
    let expression = parse_one(source);
    let id = expression.id;
    let Expression::List { values } = &expression.expression else {
        panic!("expected list")
    };
    let a_id = values[0].id;
    let index_id = values[1].id;
    let Expression::Call { arguments, .. } = &values[1].expression else {
        panic!("expected index call")
    };
    let receiver_id = arguments[0].id;
    let subscript_id = arguments[1].id;

    let target = AssignmentTargetLocation::try_from(expression).unwrap();
    assert_eq!(target.id, id);
    assert_eq!(text(source, target.span), source);
    let AssignmentTarget::Sequence(items) = target.target else {
        unreachable!()
    };
    assert_eq!(items[0].id, a_id);
    assert_eq!(text(source, items[0].span), "(a)");
    let AssignmentTarget::Identifier { span, .. } = items[0].target else {
        unreachable!()
    };
    assert_eq!(text(source, span), "a");
    assert_eq!(items[1].id, index_id);
    assert_eq!(text(source, items[1].span), "values[next()]");
    let AssignmentTarget::Index { value, index, .. } = &items[1].target else {
        unreachable!()
    };
    assert_eq!(value.id, receiver_id);
    assert_eq!(index.id, subscript_id);
    assert_eq!(text(source, value.span), "values");
    assert_eq!(text(source, index.span), "next()");
    assert_eq!(
        HashSet::from([id, a_id, index_id, receiver_id, subscript_id]).len(),
        5
    );
}

#[test]
fn grouped_member_target_keeps_outer_identity_and_precise_member_span() {
    let source = "((make().field))";
    let expression = parse_one(source);
    let id = expression.id;
    let target = AssignmentTargetLocation::try_from(expression).unwrap();
    assert_eq!(target.id, id);
    assert_eq!(text(source, target.span), source);
    let AssignmentTarget::Member {
        receiver,
        member_span,
        ..
    } = target.target
    else {
        panic!("expected member target")
    };
    assert_eq!(text(source, member_span), "field");
    assert_eq!(text(source, receiver.span), "make()");
    assert_ne!(receiver.id, id);
}

#[test]
fn declaration_conversion_retains_each_pattern_identity() {
    let expression = parse_one("[x, [y]]");
    let target = AssignmentTargetLocation::try_from(expression).unwrap();
    let root_id = target.id;
    let AssignmentTarget::Sequence(items) = &target.target else {
        unreachable!()
    };
    let x_id = items[0].id;
    let nested_id = items[1].id;
    let AssignmentTarget::Sequence(nested) = &items[1].target else {
        unreachable!()
    };
    let y_id = nested[0].id;
    let pattern = BindingPatternLocation::try_from(target).unwrap();
    let mut nodes = vec![];
    pattern_nodes(&pattern, &mut nodes);
    assert_eq!(
        nodes.iter().map(|(id, _)| *id).collect::<Vec<_>>(),
        [root_id, x_id, nested_id, y_id]
    );
}

#[test]
fn function_parameters_and_loop_binders_carry_pattern_metadata() {
    let source = "fn identity((x): Int) => x";
    let expression = parse_one(source);
    let Expression::FunctionDeclaration { parameters, .. } = expression.expression else {
        panic!("expected function")
    };
    let parameter = &parameters[0];
    assert_eq!(text(source, parameter.span), "(x): Int");
    assert_eq!(text(source, parameter.lvalue.span), "(x)");
    let BindingPattern::Identifier { span, .. } = parameter.lvalue.pattern else {
        unreachable!()
    };
    assert_eq!(text(source, span), "x");

    let source = "for [(x), y] in input { x }";
    let expression = parse_one(source);
    let Expression::For { iterations, .. } = expression.expression else {
        panic!("expected loop")
    };
    let ForIteration::Iteration { l_value, sequence } = &iterations[0] else {
        unreachable!()
    };
    let mut nodes = vec![];
    pattern_nodes(l_value, &mut nodes);
    assert_eq!(
        nodes
            .iter()
            .map(|(_, span)| text(source, *span))
            .collect::<Vec<_>>(),
        ["[(x), y]", "(x)", "y"]
    );
    assert!(nodes.iter().all(|(id, _)| *id != sequence.id));
}

#[test]
fn split_targets_preserve_rejected_syntax() {
    for source in [
        "let [xs[0]] = input",
        "let [object.field] = input",
        "for [xs[0]] in input { 0 }",
        "for [object.field] in input { 0 }",
        "fn destructure((x, y)) => x",
        "[(object.field)] = input",
    ] {
        let tokens = Lexer::new(source, SourceId::SYNTHETIC)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert!(
            Parser::from_tokens(tokens).parse().is_err(),
            "must reject {source}"
        );
    }
}
