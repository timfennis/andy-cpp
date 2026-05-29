use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue};
use tower_lsp::lsp_types::{Location, Position, Url};

use crate::state::DocumentState;
use crate::visitor::node_at_offset;

/// Resolve the declaration of the identifier under the cursor.
///
/// Uses lexical scope resolution over the analysed AST: it gathers every
/// declaration with the scope region in which it is visible, then picks the
/// innermost-scoped declaration of the right name that is visible at the cursor.
///
/// This is done structurally rather than via the analyser's resolved slots
/// because local slot numbers are stack-relative (reset to 0 per function), so
/// they are not unique across the whole document and cannot be used as a map key.
pub fn goto_definition(state: &DocumentState, position: Position, uri: Url) -> Option<Location> {
    let offset = state.line_index.offset(&state.source, position)?;
    let node = node_at_offset(&state.ast, offset)?;
    let Expression::Identifier { name, .. } = &node.expression else {
        return None;
    };

    // The whole document is the outermost scope.
    let file_scope = Span::new(node.span.source_id(), 0, state.source.len());
    let mut decls = Vec::new();
    for expr in &state.ast {
        collect_decls(expr, file_scope, &mut decls);
    }

    let best = decls
        .iter()
        .filter(|d| d.name == *name && scope_contains(d.scope, offset))
        .reduce(|a, b| if better(b, a, offset) { b } else { a })?;

    Some(Location {
        uri,
        range: state.line_index.range(&state.source, best.decl_span),
    })
}

struct Decl {
    name: String,
    /// Span of the declared identifier (the go-to-definition target).
    decl_span: Span,
    /// Region of source in which the binding is visible.
    scope: Span,
}

fn scope_contains(scope: Span, offset: usize) -> bool {
    offset >= scope.offset() && offset < scope.end()
}

fn scope_len(scope: Span) -> usize {
    scope.end().saturating_sub(scope.offset())
}

/// Is `candidate` a better match than `current` for a usage at `offset`?
/// Innermost scope wins; within the same scope, the most recent declaration at
/// or before the usage wins (handling shadowing); functions declared later
/// (hoisted) are still acceptable when nothing precedes the usage.
fn better(candidate: &Decl, current: &Decl, offset: usize) -> bool {
    let (cand_len, cur_len) = (scope_len(candidate.scope), scope_len(current.scope));
    if cand_len != cur_len {
        return cand_len < cur_len;
    }
    let cand_before = candidate.decl_span.offset() <= offset;
    let cur_before = current.decl_span.offset() <= offset;
    match (cand_before, cur_before) {
        (true, false) => true,
        (false, true) => false,
        (true, true) => candidate.decl_span.offset() > current.decl_span.offset(),
        (false, false) => candidate.decl_span.offset() < current.decl_span.offset(),
    }
}

/// Walk an expression, recording declarations against the scope they live in.
fn collect_decls(expr: &ExpressionLocation, scope: Span, out: &mut Vec<Decl>) {
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value, .. } => {
            push_lvalue(l_value, scope, out);
            collect_decls(value, scope, out);
        }
        Expression::FunctionDeclaration {
            name,
            parameters,
            body,
            ..
        } => {
            // The function name is visible in the enclosing scope.
            if let Some(name) = name {
                out.push(Decl {
                    name: name.clone(),
                    decl_span: expr.span,
                    scope,
                });
            }
            // Parameters and body locals are scoped to the body.
            let body_scope = body.span;
            for p in parameters {
                push_lvalue(&p.lvalue, body_scope, out);
            }
            collect_decls(body, body_scope, out);
        }
        Expression::Block { statements } => {
            for s in statements {
                collect_decls(s, expr.span, out);
            }
        }
        Expression::Statement(inner) | Expression::Grouping(inner) => {
            collect_decls(inner, scope, out);
        }
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            collect_decls(condition, scope, out);
            collect_decls(on_true, scope, out);
            if let Some(f) = on_false {
                collect_decls(f, scope, out);
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            collect_decls(expression, scope, out);
            collect_decls(loop_body, scope, out);
        }
        Expression::For { iterations, body } => {
            // For-loop bindings are visible across the whole loop expression.
            let loop_scope = expr.span;
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { l_value, sequence } => {
                        push_lvalue(l_value, loop_scope, out);
                        collect_decls(sequence, scope, out);
                    }
                    ForIteration::Guard(e) => collect_decls(e, loop_scope, out),
                }
            }
            match body.as_ref() {
                ForBody::Block(e) | ForBody::List { expr: e, .. } => {
                    collect_decls(e, loop_scope, out);
                }
                ForBody::Map {
                    key,
                    value,
                    default,
                    ..
                } => {
                    collect_decls(key, loop_scope, out);
                    if let Some(v) = value {
                        collect_decls(v, loop_scope, out);
                    }
                    if let Some(d) = default {
                        collect_decls(d, loop_scope, out);
                    }
                }
            }
        }
        Expression::Return { value } => collect_decls(value, scope, out),
        Expression::Logical { left, right, .. } => {
            collect_decls(left, scope, out);
            collect_decls(right, scope, out);
        }
        Expression::Assignment { r_value, .. } | Expression::OpAssignment { r_value, .. } => {
            collect_decls(r_value, scope, out);
        }
        Expression::Call {
            function,
            arguments,
        }
        | Expression::OperatorCall {
            function,
            arguments,
        } => {
            collect_decls(function, scope, out);
            for arg in arguments {
                collect_decls(arg, scope, out);
            }
        }
        Expression::Tuple { values } | Expression::List { values } => {
            for v in values {
                collect_decls(v, scope, out);
            }
        }
        Expression::Map { values, default } => {
            for (key, value) in values {
                collect_decls(key, scope, out);
                if let Some(v) = value {
                    collect_decls(v, scope, out);
                }
            }
            if let Some(d) = default {
                collect_decls(d, scope, out);
            }
        }
        Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
            if let Some(s) = start {
                collect_decls(s, scope, out);
            }
            if let Some(e) = end {
                collect_decls(e, scope, out);
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
            decl_span: *span,
            scope,
        }),
        Lvalue::Sequence(lvalues) => {
            for lv in lvalues {
                push_lvalue(lv, scope, out);
            }
        }
        Lvalue::Index { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;

    fn analyse(source: &str) -> DocumentState {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        DocumentState::from_analysis(source.to_string(), ast, analysis)
    }

    fn uri() -> Url {
        Url::parse("file:///test.ndc").unwrap()
    }

    fn start_offset(state: &DocumentState, location: &Location) -> usize {
        state
            .line_index
            .offset(&state.source, location.range.start)
            .unwrap()
    }

    #[test]
    fn jump_from_usage_to_let_declaration() {
        //          0         1
        //          0123456789012345678
        let src = "let total = 5;\ntotal + 1;";
        let state = analyse(src);
        // Cursor on the usage `total` on line 1.
        let loc = goto_definition(&state, Position::new(1, 2), uri()).expect("definition found");
        // Declaration `total` starts at byte 4.
        assert_eq!(start_offset(&state, &loc), 4);
    }

    #[test]
    fn jump_to_function_parameter() {
        let src = "fn square(n) { n * n }";
        let state = analyse(src);
        // First `n` usage is in the body. `n` parameter is at byte 10.
        let body_n = src.rfind("n *").unwrap();
        let pos = state.line_index.position(&state.source, body_n);
        let loc = goto_definition(&state, pos, uri()).expect("definition found");
        assert_eq!(start_offset(&state, &loc), 10);
    }

    #[test]
    fn innermost_scope_shadows_outer() {
        let src = "let x = 1;\nfn f() { let x = 2; x }";
        let state = analyse(src);
        // The `x` usage inside `f` should resolve to the inner `x` (byte offset
        // of the inner declaration), not the top-level one at byte 4.
        let inner_decl = src.rfind("x = 2").unwrap();
        let usage = src.rfind('x').unwrap();
        let pos = state.line_index.position(&state.source, usage);
        let loc = goto_definition(&state, pos, uri()).expect("definition found");
        assert_eq!(start_offset(&state, &loc), inner_decl);
    }
}
