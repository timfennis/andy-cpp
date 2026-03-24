use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue};

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
    ) {
    }
}

/// Walk a slice of top-level expressions, invoking visitor hooks along the way.
pub fn walk_ast(visitor: &mut impl AstVisitor, expressions: &[ExpressionLocation]) {
    for expr in expressions {
        walk_expression(visitor, expr);
    }
}

fn walk_expression(visitor: &mut impl AstVisitor, expr: &ExpressionLocation) {
    visitor.on_expression(expr);
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value, .. } => {
            // TODO: if we have an explicit type we should not show the inlays. How though?
            walk_lvalue(visitor, l_value);
            walk_expression(visitor, value);
        }
        Expression::FunctionDeclaration {
            return_type,
            parameters_span,
            body,
            ..
        } => {
            visitor.on_function_declaration(return_type.as_ref(), *parameters_span);
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
                        walk_lvalue(visitor, l_value);
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
        _ => {}
    }
}

fn walk_lvalue(visitor: &mut impl AstVisitor, lvalue: &Lvalue) {
    match lvalue {
        Lvalue::Identifier {
            identifier,
            inferred_type,
            span,
            ..
        } => {
            visitor.on_declaration(identifier, inferred_type.as_ref(), *span);
        }
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                walk_lvalue(visitor, lv);
            }
        }
        Lvalue::Index { .. } => {}
    }
}
