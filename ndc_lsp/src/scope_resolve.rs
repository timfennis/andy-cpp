//! Lexical scope resolution over the analysed AST.
//!
//! Both go-to-definition and in-scope completion need the same structural
//! question answered: *which declarations are visible at a given source offset?*
//! This module walks the AST once, recording each declaration together with the
//! source region in which it is visible, and exposes a visibility predicate.
//!
//! Resolution is done structurally (not via the analyser's resolved slots)
//! because local slot numbers are stack-relative — they reset to 0 per function,
//! so they are not unique across the document and can't key a global map.
//! Issue #254 tracks moving go-to-definition onto the resolver's symbols;
//! completion's visibility enumeration is a pure structural query that stays.

use ndc_lexer::{SourceId, Span};
use ndc_parser::{
    BindingPattern, BindingPatternLocation, Expression, ExpressionLocation, ForBody, ForIteration,
};

/// A declaration discovered while walking the AST.
pub struct Decl {
    pub name: String,
    /// Span of the declared name — the go-to-definition target.
    pub name_span: Span,
    /// Region of source in which this binding is visible.
    pub scope_span: Span,
    /// Byte offset from which the binding is in scope. For `let`/for-loop
    /// bindings this is the END of the initializer (RHS / sequence), so the
    /// binding is not visible inside its own initializer — matching the analyser,
    /// which resolves the initializer before creating the binding (`let x = x`
    /// must resolve the RHS to an outer `x`). Functions and structs are
    /// visible from the start of their declaration, so a function can call
    /// itself; like the language, nothing is hoisted.
    pub visible_from: usize,
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

/// Is `decl` visible at `offset`? It must be in an enclosing scope, and the use
/// must come at or after `visible_from`.
pub fn is_visible(decl: &Decl, offset: usize) -> bool {
    contains(decl.scope_span, offset) && decl.visible_from <= offset
}

pub fn contains(span: Span, offset: usize) -> bool {
    offset >= span.offset() && offset < span.end()
}

pub fn scope_len(span: Span) -> usize {
    span.end().saturating_sub(span.offset())
}

/// Records the declarations in `expr`, which sits in `scope`. Only the nodes
/// that declare names or open a scope need matching here; every other node's
/// children are in the same scope as the node.
fn collect(expr: &ExpressionLocation, scope: Span, out: &mut Vec<Decl>) {
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value, .. } => {
            // Visible only after the initializer, so `let x = x` resolves the RHS
            // to an outer binding rather than itself.
            push_pattern(l_value, scope, value.span.end(), out);
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
                    visible_from: expr.span.offset(),
                });
            }
            // Parameters and body locals are scoped to the body; parameters are
            // visible throughout it (the body follows the parameter list).
            let body_scope = body.span;
            for p in parameters {
                push_pattern(&p.lvalue, body_scope, p.span.offset(), out);
            }
            collect(body, body_scope, out);
        }
        Expression::Block { statements } => {
            for s in statements {
                collect(s, expr.span, out);
            }
        }
        Expression::For { iterations, body } => {
            // For-loop bindings are visible across the loop expression, but only
            // after their own sequence (so `for x in x` resolves the sequence to
            // an outer `x`; later iterations/guards/body still see the binding).
            let loop_scope = expr.span;
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { l_value, sequence } => {
                        push_pattern(l_value, loop_scope, sequence.span.end(), out);
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
        Expression::StructDeclaration { name, .. } => {
            // The declaration binds the constructor, visible from the
            // declaration onward (structs are not hoisted, matching the
            // analyser). No dedicated name span exists yet (the parser keeps
            // only the whole-declaration span).
            out.push(Decl {
                name: name.clone(),
                name_span: expr.span,
                scope_span: scope,
                visible_from: expr.span.offset(),
            });
        }
        _ => {
            for child in expr.children() {
                collect(child, scope, out);
            }
        }
    }
}

fn push_pattern(
    pattern: &BindingPatternLocation,
    scope: Span,
    visible_from: usize,
    out: &mut Vec<Decl>,
) {
    match &pattern.pattern {
        BindingPattern::Identifier {
            identifier, span, ..
        } => out.push(Decl {
            name: identifier.clone(),
            name_span: *span,
            scope_span: scope,
            visible_from,
        }),
        BindingPattern::Sequence(patterns) => {
            for pattern in patterns {
                push_pattern(pattern, scope, visible_from, out);
            }
        }
    }
}
