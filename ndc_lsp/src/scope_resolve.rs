//! Which declarations are visible at a source offset, for completion.
//!
//! What a name *refers to* comes from the analysis (`Analysis::definitions`).
//! Completion asks something the analysis doesn't answer: every name it could
//! write at the cursor, including at a place no name is used yet. This module
//! walks the AST once, recording each declaration with the region in which
//! it is visible, and exposes a visibility predicate.

use ndc_core::NodeId;
use ndc_lexer::{SourceId, Span};
use ndc_parser::{
    BindingPattern, BindingPatternLocation, Expression, ExpressionLocation, ForBody, ForIteration,
};

/// What a declaration declares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclKind {
    /// A name a binding pattern declares: a `let`, a parameter or a loop
    /// variable, keyed in the analysis by its pattern.
    Variable(NodeId),
    /// A named function, keyed in the analysis by its declaration.
    Function(NodeId),
    /// A struct's constructor and field accessors, keyed in the analysis by
    /// its declaration.
    Struct(NodeId),
}

/// A declaration discovered while walking the AST.
pub struct Decl {
    pub name: String,
    pub kind: DeclKind,
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
    /// Where else it is visible: a comprehension's bindings are visible in
    /// the expression that comes before them, `x * 2` in `[x * 2 for x in xs]`.
    pub also_visible_in: Vec<Span>,
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
    contains(decl.scope_span, offset)
        && (decl.visible_from <= offset
            // The cursor at the end of what's being typed counts.
            || decl
                .also_visible_in
                .iter()
                .any(|span| span.offset() <= offset && offset <= span.end()))
}

pub fn contains(span: Span, offset: usize) -> bool {
    offset >= span.offset() && offset < span.end()
}

/// Records the declarations in `expr`, which sits in `scope`. Only the nodes
/// that declare names or open a scope need matching here; every other node's
/// children are in the same scope as the node.
fn collect(expr: &ExpressionLocation, scope: Span, out: &mut Vec<Decl>) {
    match &expr.expression {
        Expression::VariableDeclaration { l_value, value, .. } => {
            // Visible only after the initializer, so `let x = x` resolves the RHS
            // to an outer binding rather than itself.
            push_pattern(l_value, scope, value.span.end(), &[], out);
            collect(value, scope, out);
        }
        Expression::FunctionDeclaration {
            name,
            parameters,
            body,
            ..
        } => {
            if let Some(name) = name {
                out.push(Decl {
                    name: name.clone(),
                    kind: DeclKind::Function(expr.id),
                    scope_span: scope,
                    visible_from: expr.span.offset(),
                    also_visible_in: Vec::new(),
                });
            }
            // Parameters and body locals are scoped to the body; parameters are
            // visible throughout it (the body follows the parameter list).
            let body_scope = body.span;
            for p in parameters {
                push_pattern(&p.lvalue, body_scope, p.span.offset(), &[], out);
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
            // A comprehension's expression comes before its iterations.
            let head: Vec<Span> = match body.as_ref() {
                ForBody::Block(_) => Vec::new(),
                ForBody::List { expr } => vec![expr.span],
                // A map comprehension's default is outside the loop.
                ForBody::Map { key, value, .. } => {
                    std::iter::once(key).chain(value).map(|e| e.span).collect()
                }
            };
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { l_value, sequence } => {
                        push_pattern(l_value, loop_scope, sequence.span.end(), &head, out);
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
            // analyser).
            out.push(Decl {
                name: name.clone(),
                kind: DeclKind::Struct(expr.id),
                scope_span: scope,
                visible_from: expr.span.offset(),
                also_visible_in: Vec::new(),
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
    also_visible_in: &[Span],
    out: &mut Vec<Decl>,
) {
    match &pattern.pattern {
        BindingPattern::Identifier { identifier, .. } => out.push(Decl {
            name: identifier.clone(),
            kind: DeclKind::Variable(pattern.id),
            scope_span: scope,
            visible_from,
            also_visible_in: also_visible_in.to_vec(),
        }),
        BindingPattern::Sequence(patterns) => {
            for pattern in patterns {
                push_pattern(pattern, scope, visible_from, also_visible_in, out);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;
    use crate::resolved::{Definition, Resolved};
    use crate::visitor::{AstVisitor, walk_ast};

    /// Each name use, with what the analysis resolved it to.
    struct Uses<'a> {
        resolved: &'a Resolved,
        found: Vec<(usize, String, NodeId)>,
    }

    impl AstVisitor for Uses<'_> {
        fn on_expression(&mut self, expr: &ExpressionLocation) {
            if let Expression::Identifier {
                name,
                identifier_span,
                ..
            } = &expr.expression
                && let Some(definitions) = self.resolved.definitions_of(expr.id)
            {
                for definition in definitions {
                    if let Definition::Node(node) = definition {
                        self.found
                            .push((identifier_span.offset(), name.clone(), *node));
                    }
                }
            }
        }
    }

    fn programs(dir: &Path, out: &mut Vec<std::path::PathBuf>) {
        for entry in std::fs::read_dir(dir).expect("readable directory") {
            let path = entry.expect("an entry").path();
            if path.is_dir() {
                programs(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "ndc") {
                out.push(path);
            }
        }
    }

    /// Completion's structural answer to "what's visible here" must agree
    /// with the resolver: for every name in the functional test programs, the
    /// declaration the analysis resolved it to is one completion would offer
    /// there.
    #[test]
    fn every_resolved_declaration_is_visible_where_it_is_used() {
        // Some programs nest deeply enough to need more than a test thread's
        // stack in a debug build.
        std::thread::Builder::new()
            .stack_size(256 << 20)
            .spawn(check_every_program)
            .expect("a thread")
            .join()
            .unwrap_or_else(|panic| std::panic::resume_unwind(panic));
    }

    fn check_every_program() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/functional/programs");
        let mut paths = Vec::new();
        programs(&root, &mut paths);
        assert!(paths.len() > 100, "found {} programs", paths.len());

        let mut checked = 0;
        let mut disagreements = Vec::new();
        for path in paths {
            let source = std::fs::read_to_string(&path).expect("a readable program");
            let (_, analysed) = crate::document::analyse(ndc_stdlib::register, &source);
            let Some(crate::document::Analysed { ast, resolved, .. }) = analysed else {
                continue;
            };
            let Some(first) = ast.first() else {
                continue;
            };
            let decls =
                collect_declarations(&ast, file_scope(first.span.source_id(), source.len()));
            let mut uses = Uses {
                resolved: &resolved,
                found: Vec::new(),
            };
            walk_ast(&mut uses, &ast);
            for (offset, name, node) in uses.found {
                let decl = decls.iter().find(|decl| match decl.kind {
                    DeclKind::Variable(id) | DeclKind::Function(id) | DeclKind::Struct(id) => {
                        id == node
                    }
                });
                // A type parameter isn't something completion offers.
                let Some(decl) = decl else { continue };
                checked += 1;
                if !is_visible(decl, offset) {
                    disagreements.push(format!("{}: `{name}` at byte {offset}", path.display()));
                }
            }
        }
        assert!(checked > 1000, "checked only {checked} uses");
        assert!(
            disagreements.is_empty(),
            "not visible where the resolver says they're used:\n{}",
            disagreements.join("\n")
        );
    }
}
