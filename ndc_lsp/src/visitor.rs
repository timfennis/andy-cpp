use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForBody, ForIteration, NodeId,
};

/// Trait for visiting interesting nodes during an AST walk.
///
/// All methods have default no-op implementations, so visitors only need to
/// override the hooks they care about. Add new hooks here (with defaults) as
/// new LSP features require them — existing visitors won't break.
pub trait AstVisitor {
    /// An identifier in a declaration position (variable or for-loop binding).
    fn on_declaration(
        &mut self,
        _identifier: &str,
        _inferred_type: Option<&StaticType>,
        _has_annotation: bool,
        _span: Span,
    ) {
    }

    /// Called for every expression node, before its children are walked.
    fn on_expression(&mut self, _expr: &ExpressionLocation) {}

    /// A function declaration, called before the body is walked.
    fn on_function_declaration(
        &mut self,
        _return_type: Option<&StaticType>,
        _parameters_span: Span,
        _node_id: NodeId,
    ) {
    }
}

/// Walk a slice of top-level expressions, invoking visitor hooks along the way.
pub fn walk_ast(visitor: &mut impl AstVisitor, expressions: &[ExpressionLocation]) {
    for expr in expressions {
        walk_expression(visitor, expr);
    }
}

/// Find the innermost expression whose span contains `offset`.
///
/// Used for position-based features (hover, go-to-definition): given a cursor
/// byte offset, return the most specific expression node under it.
pub fn node_at_offset(
    expressions: &[ExpressionLocation],
    offset: usize,
) -> Option<&ExpressionLocation> {
    let mut best: Option<&ExpressionLocation> = None;
    for expr in expressions {
        find_node_at(expr, offset, &mut best);
    }
    best
}

fn span_len(span: Span) -> usize {
    span.end().saturating_sub(span.offset())
}

fn find_node_at<'a>(
    expr: &'a ExpressionLocation,
    offset: usize,
    best: &mut Option<&'a ExpressionLocation>,
) {
    let span = expr.span;
    if offset < span.offset() || offset >= span.end() {
        return;
    }
    // This node contains the offset; keep it if it's at least as specific
    // (smaller span) as the best candidate so far.
    if best.is_none_or(|b| span_len(span) <= span_len(b.span)) {
        *best = Some(expr);
    }
    for child in child_expressions(expr) {
        find_node_at(child, offset, best);
    }
}

/// The expression-typed children of a node. Mirrors the structure walked by
/// [`walk_expression`], but returns references so callers can search for a node
/// rather than visiting via the [`AstVisitor`] trait. Binding patterns are not
/// expression children. Assignment targets contribute their receiver and index
/// expressions.
fn child_expressions(expr: &ExpressionLocation) -> Vec<&ExpressionLocation> {
    let mut out: Vec<&ExpressionLocation> = Vec::new();
    match &expr.expression {
        Expression::VariableDeclaration { value, .. } => {
            out.push(value);
        }
        Expression::FunctionDeclaration { body, .. } => out.push(body),
        Expression::Statement(inner)
        | Expression::Grouping(inner)
        | Expression::Cast { value: inner, .. } => out.push(inner),
        Expression::Block { statements } => out.extend(statements.iter()),
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            out.push(condition);
            out.push(on_true);
            if let Some(f) = on_false {
                out.push(f);
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            out.push(expression);
            out.push(loop_body);
        }
        Expression::For { iterations, body } => {
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { sequence, .. } => {
                        out.push(sequence);
                    }
                    ForIteration::Guard(e) => out.push(e),
                }
            }
            match body.as_ref() {
                ForBody::Block(e) | ForBody::List { expr: e, .. } => out.push(e),
                ForBody::Map {
                    key,
                    value,
                    default,
                    ..
                } => {
                    out.push(key);
                    if let Some(v) = value {
                        out.push(v);
                    }
                    if let Some(d) = default {
                        out.push(d);
                    }
                }
            }
        }
        Expression::Return { value } => out.push(value),
        Expression::Logical { left, right, .. } => {
            out.push(left);
            out.push(right);
        }
        Expression::Assignment { l_value, r_value }
        | Expression::OpAssignment {
            l_value, r_value, ..
        } => {
            push_target_expressions(l_value, &mut out);
            out.push(r_value);
        }
        Expression::Call {
            function,
            arguments,
        }
        | Expression::OperatorCall {
            function,
            arguments,
        } => {
            out.push(function);
            out.extend(arguments.iter());
        }
        Expression::MemberAccess { receiver, .. } => out.push(receiver),
        Expression::Tuple { values } | Expression::List { values } => out.extend(values.iter()),
        Expression::Map { values, default } => {
            for (key, value) in values {
                out.push(key);
                if let Some(v) = value {
                    out.push(v);
                }
            }
            if let Some(d) = default {
                out.push(d);
            }
        }
        Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
            if let Some(s) = start {
                out.push(s);
            }
            if let Some(e) = end {
                out.push(e);
            }
        }
        // Leaves with no sub-expressions. Struct declarations belong here:
        // fields are names + type annotations, not expressions.
        Expression::Identifier { .. }
        | Expression::BoolLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::Break
        | Expression::Continue
        | Expression::StructDeclaration { .. } => {}
    }
    out
}

/// Push receiver and index expressions so they participate in position lookup.
fn push_target_expressions<'a>(
    target: &'a AssignmentTargetLocation,
    out: &mut Vec<&'a ExpressionLocation>,
) {
    match &target.target {
        AssignmentTarget::Index { value, index, .. } => {
            out.push(value);
            out.push(index);
        }
        AssignmentTarget::Member { receiver, .. } => out.push(receiver),
        AssignmentTarget::Sequence(targets) => {
            for target in targets {
                push_target_expressions(target, out);
            }
        }
        AssignmentTarget::Identifier { .. } => {}
    }
}

fn walk_expression(visitor: &mut impl AstVisitor, expr: &ExpressionLocation) {
    visitor.on_expression(expr);
    match &expr.expression {
        Expression::VariableDeclaration {
            l_value,
            annotated_type,
            value,
        } => {
            walk_pattern(visitor, l_value, annotated_type.is_some());
            walk_expression(visitor, value);
        }
        Expression::FunctionDeclaration {
            resolved_return_type,
            parameters,
            parameters_span,
            body,
            ..
        } => {
            for p in parameters {
                walk_pattern(visitor, &p.lvalue, p.annotation.is_some());
            }
            visitor.on_function_declaration(
                resolved_return_type.as_ref(),
                *parameters_span,
                expr.id,
            );
            walk_expression(visitor, body);
        }
        Expression::Statement(inner)
        | Expression::Grouping(inner)
        | Expression::Cast { value: inner, .. } => {
            walk_expression(visitor, inner);
        }
        Expression::Block { statements } => {
            for s in statements {
                walk_expression(visitor, s);
            }
        }
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            walk_expression(visitor, condition);
            walk_expression(visitor, on_true);
            if let Some(f) = on_false {
                walk_expression(visitor, f);
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            walk_expression(visitor, expression);
            walk_expression(visitor, loop_body);
        }
        Expression::For { iterations, body } => {
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { l_value, sequence } => {
                        walk_pattern(visitor, l_value, false);
                        walk_expression(visitor, sequence);
                    }
                    ForIteration::Guard(expr) => walk_expression(visitor, expr),
                }
            }
            match body.as_ref() {
                ForBody::Block(e) | ForBody::List { expr: e, .. } => {
                    walk_expression(visitor, e);
                }
                ForBody::Map {
                    key,
                    value,
                    default,
                    ..
                } => {
                    walk_expression(visitor, key);
                    if let Some(v) = value {
                        walk_expression(visitor, v);
                    }
                    if let Some(d) = default {
                        walk_expression(visitor, d);
                    }
                }
            }
        }
        Expression::Return { value } => walk_expression(visitor, value),
        Expression::Logical { left, right, .. } => {
            walk_expression(visitor, left);
            walk_expression(visitor, right);
        }
        Expression::Assignment { l_value, r_value }
        | Expression::OpAssignment {
            l_value, r_value, ..
        } => {
            walk_target(visitor, l_value);
            walk_expression(visitor, r_value);
        }
        Expression::Call {
            function,
            arguments,
        }
        | Expression::OperatorCall {
            function,
            arguments,
        } => {
            walk_expression(visitor, function);
            for arg in arguments {
                walk_expression(visitor, arg);
            }
        }
        Expression::MemberAccess { receiver, .. } => walk_expression(visitor, receiver),
        Expression::Tuple { values } | Expression::List { values } => {
            for v in values {
                walk_expression(visitor, v);
            }
        }
        Expression::Map { values, default } => {
            for (key, value) in values {
                walk_expression(visitor, key);
                if let Some(v) = value {
                    walk_expression(visitor, v);
                }
            }
            if let Some(d) = default {
                walk_expression(visitor, d);
            }
        }
        Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
            if let Some(s) = start {
                walk_expression(visitor, s);
            }
            if let Some(e) = end {
                walk_expression(visitor, e);
            }
        }
        // Leaves with no sub-expressions. Struct declarations belong here:
        // fields are names + type annotations, not expressions.
        Expression::Identifier { .. }
        | Expression::BoolLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::Break
        | Expression::Continue
        | Expression::StructDeclaration { .. } => {}
    }
}

fn walk_target(visitor: &mut impl AstVisitor, target: &AssignmentTargetLocation) {
    match &target.target {
        AssignmentTarget::Identifier {
            identifier,
            inferred_type,
            span,
            ..
        } => {
            visitor.on_declaration(identifier, inferred_type.as_ref(), false, *span);
        }
        AssignmentTarget::Sequence(targets) => {
            for target in targets {
                walk_target(visitor, target);
            }
        }
        AssignmentTarget::Index { value, index, .. } => {
            walk_expression(visitor, value);
            walk_expression(visitor, index);
        }
        AssignmentTarget::Member { receiver, .. } => walk_expression(visitor, receiver),
    }
}

fn walk_pattern(
    visitor: &mut impl AstVisitor,
    pattern: &BindingPatternLocation,
    has_annotation: bool,
) {
    match &pattern.pattern {
        BindingPattern::Identifier {
            identifier,
            inferred_type,
            span,
            ..
        } => visitor.on_declaration(identifier, inferred_type.as_ref(), has_annotation, *span),
        BindingPattern::Sequence(patterns) => {
            for pattern in patterns {
                walk_pattern(visitor, pattern, has_annotation);
            }
        }
    }
}
