use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, NodeId};

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
/// rather than visiting via the [`AstVisitor`] trait. Lvalue (declaration)
/// positions are not expression children and are intentionally omitted, except
/// for the expression operands inside an `Lvalue::Index`.
fn child_expressions(expr: &ExpressionLocation) -> Vec<&ExpressionLocation> {
    let mut out: Vec<&ExpressionLocation> = Vec::new();
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_lvalue_index(l_value, &mut out);
            out.push(value);
        }
        Expression::FunctionDeclaration { body, .. } => out.push(body),
        Expression::Statement(inner) | Expression::Grouping(inner) => out.push(inner),
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
                    ForIteration::Iteration { l_value, sequence } => {
                        push_lvalue_index(l_value, &mut out);
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
            push_lvalue_index(l_value, &mut out);
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
        Expression::Identifier { .. }
        | Expression::BoolLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::Int64Literal(_)
        | Expression::Float64Literal(_)
        | Expression::BigIntLiteral(_)
        | Expression::ComplexLiteral(_)
        | Expression::Break
        | Expression::Continue => {}
        Expression::StructDeclaration { .. } => {
            // TODO: @Claude do we deal with structs here?
        }
    }
    out
}

/// Push the expression operands of an `Lvalue::Index` (the indexed value and
/// the index expression) so they participate in position lookup.
fn push_lvalue_index<'a>(lvalue: &'a Lvalue, out: &mut Vec<&'a ExpressionLocation>) {
    match lvalue {
        Lvalue::Index { value, index, .. } => {
            out.push(value);
            out.push(index);
        }
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                push_lvalue_index(lv, out);
            }
        }
        Lvalue::Identifier { .. } => {}
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
            walk_lvalue(visitor, l_value, annotated_type.is_some());
            walk_expression(visitor, value);
        }
        Expression::FunctionDeclaration {
            return_type,
            parameters,
            parameters_span,
            body,
            ..
        } => {
            for p in parameters {
                walk_lvalue(visitor, &p.lvalue, p.annotation.is_some());
            }
            visitor.on_function_declaration(return_type.as_ref(), *parameters_span, expr.id);
            walk_expression(visitor, body);
        }
        Expression::Statement(inner) | Expression::Grouping(inner) => {
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
                        walk_lvalue(visitor, l_value, false);
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
            walk_lvalue(visitor, l_value, false);
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
        Expression::Identifier { .. }
        | Expression::BoolLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::Int64Literal(_)
        | Expression::Float64Literal(_)
        | Expression::BigIntLiteral(_)
        | Expression::ComplexLiteral(_)
        | Expression::Break
        | Expression::Continue => {}
        Expression::StructDeclaration { .. } => {
            // TODO: @Claude do we deal with structs here
        }
    }
}

fn walk_lvalue(visitor: &mut impl AstVisitor, lvalue: &Lvalue, has_annotation: bool) {
    match lvalue {
        Lvalue::Identifier {
            identifier,
            inferred_type,
            span,
            ..
        } => {
            visitor.on_declaration(identifier, inferred_type.as_ref(), has_annotation, *span);
        }
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                walk_lvalue(visitor, lv, has_annotation);
            }
        }
        Lvalue::Index { value, index, .. } => {
            walk_expression(visitor, value);
            walk_expression(visitor, index);
        }
    }
}
