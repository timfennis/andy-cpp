//! Lexical scope resolution over the analysed AST.
//!
//! Both go-to-definition and in-scope completion need the same structural
//! question answered: *which declarations are visible at a given source offset?*
//! This module walks the AST once, recording each declaration together with the
//! source region in which it is visible, and exposes a visibility predicate.
//!
//! Resolution is done structurally (not via the analyser's resolved slots)
//! because local slot numbers are stack-relative — they reset to 0 per function,
//! so they are not unique across the document and can't key a global map. The
//! follow-up that exposes the analyser's resolution will replace go-to-definition
//! here; completion's visibility enumeration is a pure structural query that
//! stays.

use ndc_lexer::{SourceId, Span};
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue};

/// A declaration discovered while walking the AST.
pub struct Decl {
    pub name: String,
    /// Span of the declared name — the go-to-definition target.
    pub name_span: Span,
    /// Region of source in which this binding is visible.
    pub scope_span: Span,
    /// Functions are hoisted: visible throughout their scope regardless of
    /// whether the use textually precedes the declaration.
    pub is_function: bool,
}

/// The outermost (document) scope. Its end is one past the source length so a
/// cursor at end-of-file still counts as inside the file scope.
pub fn file_scope(source_id: SourceId, source_len: usize) -> Span {
    Span::new(source_id, 0, source_len + 1)
}

/// Collect every declaration in `ast`, scoped to the region it is visible in.
/// `file_scope` is the outermost scope (the whole document).
pub fn collect_declarations(ast: &[ExpressionLocation], file_scope: Span) -> Vec<Decl> {
    let mut out = Vec::new();
    for expr in ast {
        collect(expr, file_scope, &mut out);
    }
    out
}

/// Is `decl` visible at `offset`? It must be in an enclosing scope, and — unless
/// it is a hoisted function — declared at or before the use.
pub fn is_visible(decl: &Decl, offset: usize) -> bool {
    contains(decl.scope_span, offset) && (decl.is_function || decl.name_span.offset() <= offset)
}

pub fn contains(span: Span, offset: usize) -> bool {
    offset >= span.offset() && offset < span.end()
}

pub fn scope_len(span: Span) -> usize {
    span.end().saturating_sub(span.offset())
}

fn collect(expr: &ExpressionLocation, scope: Span, out: &mut Vec<Decl>) {
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_lvalue(l_value, scope, out);
            collect(value, scope, out);
        }
        Expression::FunctionDeclaration {
            name,
            parameters,
            body,
            ..
        } => {
            if let Some(name) = name {
                // No dedicated name span exists yet (the parser keeps only the
                // whole-declaration span); the follow-up adds `name_span`.
                out.push(Decl {
                    name: name.clone(),
                    name_span: expr.span,
                    scope_span: scope,
                    is_function: true,
                });
            }
            // Parameters and body locals are scoped to the body.
            let body_scope = body.span;
            for p in parameters {
                push_lvalue(&p.lvalue, body_scope, out);
            }
            collect(body, body_scope, out);
        }
        Expression::Block { statements } => {
            for s in statements {
                collect(s, expr.span, out);
            }
        }
        Expression::Statement(inner) | Expression::Grouping(inner) => collect(inner, scope, out),
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            collect(condition, scope, out);
            collect(on_true, scope, out);
            if let Some(f) = on_false {
                collect(f, scope, out);
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            collect(expression, scope, out);
            collect(loop_body, scope, out);
        }
        Expression::For { iterations, body } => {
            // For-loop bindings are visible across the whole loop expression.
            let loop_scope = expr.span;
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { l_value, sequence } => {
                        push_lvalue(l_value, loop_scope, out);
                        collect(sequence, scope, out);
                    }
                    ForIteration::Guard(e) => collect(e, loop_scope, out),
                }
            }
            match body.as_ref() {
                ForBody::Block(e) | ForBody::List { expr: e, .. } => collect(e, loop_scope, out),
                ForBody::Map {
                    key,
                    value,
                    default,
                    ..
                } => {
                    collect(key, loop_scope, out);
                    if let Some(v) = value {
                        collect(v, loop_scope, out);
                    }
                    if let Some(d) = default {
                        collect(d, loop_scope, out);
                    }
                }
            }
        }
        Expression::Return { value } => collect(value, scope, out),
        Expression::Logical { left, right, .. } => {
            collect(left, scope, out);
            collect(right, scope, out);
        }
        Expression::Assignment { r_value, .. } | Expression::OpAssignment { r_value, .. } => {
            collect(r_value, scope, out);
        }
        Expression::Call {
            function,
            arguments,
        }
        | Expression::OperatorCall {
            function,
            arguments,
        } => {
            collect(function, scope, out);
            for arg in arguments {
                collect(arg, scope, out);
            }
        }
        Expression::Tuple { values } | Expression::List { values } => {
            for v in values {
                collect(v, scope, out);
            }
        }
        Expression::Map { values, default } => {
            for (key, value) in values {
                collect(key, scope, out);
                if let Some(v) = value {
                    collect(v, scope, out);
                }
            }
            if let Some(d) = default {
                collect(d, scope, out);
            }
        }
        Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
            if let Some(s) = start {
                collect(s, scope, out);
            }
            if let Some(e) = end {
                collect(e, scope, out);
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
    }
}

fn push_lvalue(lvalue: &Lvalue, scope: Span, out: &mut Vec<Decl>) {
    match lvalue {
        Lvalue::Identifier {
            identifier, span, ..
        } => out.push(Decl {
            name: identifier.clone(),
            name_span: *span,
            scope_span: scope,
            is_function: false,
        }),
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                push_lvalue(lv, scope, out);
            }
        }
        Lvalue::Index { .. } => {}
    }
}
