use ndc_core::NodeId;
use ndc_lexer::Span;
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForIteration,
};

/// Trait for visiting interesting nodes during an AST walk.
///
/// All methods have default no-op implementations, so visitors only need to
/// override the hooks they care about. Add new hooks here (with defaults) as
/// new LSP features require them — existing visitors won't break.
pub trait AstVisitor {
    /// An identifier in a declaration position (variable or for-loop binding).
    /// `node` is its binding pattern or assignment target. The analysis keys a
    /// declared name's type by its binding pattern; assignment targets have none.
    fn on_declaration(
        &mut self,
        _identifier: &str,
        _node: NodeId,
        _has_annotation: bool,
        _span: Span,
    ) {
    }

    /// Called for every expression node, before its children are walked.
    fn on_expression(&mut self, _expr: &ExpressionLocation) {}

    /// A function declaration, called before the body is walked.
    fn on_function_declaration(
        &mut self,
        _has_return_annotation: bool,
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
    for child in expr.children() {
        find_node_at(child, offset, best);
    }
}

/// Calls the visitor's hooks for `expr`, then walks its children. Only the
/// nodes that declare names or functions need matching here.
fn walk_expression(visitor: &mut impl AstVisitor, expr: &ExpressionLocation) {
    visitor.on_expression(expr);
    match &expr.expression {
        Expression::VariableDeclaration {
            l_value,
            annotated_type,
            ..
        } => walk_pattern(visitor, l_value, annotated_type.is_some()),
        Expression::FunctionDeclaration {
            return_annotation,
            parameters,
            parameters_span,
            ..
        } => {
            for p in parameters {
                walk_pattern(visitor, &p.lvalue, p.annotation.is_some());
            }
            visitor.on_function_declaration(return_annotation.is_some(), *parameters_span, expr.id);
        }
        Expression::For { iterations, .. } => {
            for iteration in iterations {
                if let ForIteration::Iteration { l_value, .. } = iteration {
                    walk_pattern(visitor, l_value, false);
                }
            }
        }
        Expression::Assignment { l_value, .. } | Expression::OpAssignment { l_value, .. } => {
            walk_target(visitor, l_value);
        }
        _ => {}
    }
    for child in expr.children() {
        walk_expression(visitor, child);
    }
}

/// Reports the plain names an assignment writes to.
fn walk_target(visitor: &mut impl AstVisitor, target: &AssignmentTargetLocation) {
    match &target.target {
        AssignmentTarget::Identifier { identifier, span } => {
            visitor.on_declaration(identifier, target.id, false, *span);
        }
        AssignmentTarget::Sequence(targets) => {
            for target in targets {
                walk_target(visitor, target);
            }
        }
        // Writes through an existing value declare nothing; their
        // expressions are among the assignment's children.
        AssignmentTarget::Index { .. } | AssignmentTarget::Member { .. } => {}
    }
}

fn walk_pattern(
    visitor: &mut impl AstVisitor,
    pattern: &BindingPatternLocation,
    has_annotation: bool,
) {
    match &pattern.pattern {
        BindingPattern::Identifier { identifier, span } => {
            visitor.on_declaration(identifier, pattern.id, has_annotation, *span)
        }
        BindingPattern::Sequence(patterns) => {
            for pattern in patterns {
                walk_pattern(visitor, pattern, has_annotation);
            }
        }
    }
}
