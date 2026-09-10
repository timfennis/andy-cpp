use crate::symbols::{SymbolId, SymbolTable};
use ahash::AHashMap;
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, NodeId};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) struct ScopeId(usize);

#[derive(Debug, Default)]
pub(crate) struct Scope {
    parent: Option<ScopeId>,
    bindings: Vec<SymbolId>,
}

pub(crate) struct ScopeTree {
    current: ScopeId,
    scopes: Vec<Scope>,
}

pub(crate) struct NameResolver {
    symbols: SymbolTable,
    scopes: ScopeTree,
    references: AHashMap<NodeId, SymbolId>,
    // Lvalues have source spans but no NodeId. Keep their declaration/write
    // bindings separately so later passes do not have to resolve them again.
    lvalue_bindings: Vec<(Span, SymbolId)>,
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum NameResolutionError {
    #[error("identifier '{name}' has not been declared")]
    UnknownName { name: String, span: Span },
    #[error("index and member targets cannot declare names")]
    InvalidDeclaration { span: Span },
    #[error("name resolution for {feature} is not implemented yet")]
    Unsupported { feature: &'static str, span: Span },
}

impl Scope {
    pub(crate) fn new_with_parent(parent: ScopeId) -> Self {
        Self {
            parent: Some(parent),
            bindings: Vec::default(),
        }
    }
    pub(crate) fn bind(&mut self, symbol_id: SymbolId) {
        self.bindings.push(symbol_id);
    }

    pub(crate) fn lookup(&self, name: &str, table: &SymbolTable) -> Option<SymbolId> {
        for id in self.bindings.iter().rev() {
            if table.get(*id) == name {
                return Some(*id);
            }
        }
        None
    }
}

impl ScopeTree {
    pub(crate) fn new() -> Self {
        Self {
            current: ScopeId(0),
            scopes: vec![Scope::default()],
        }
    }

    pub(crate) fn bind(&mut self, symbol_id: SymbolId) {
        self.scopes[self.current.0].bind(symbol_id);
    }

    pub(crate) fn lookup(&self, name: &str, table: &SymbolTable) -> Option<SymbolId> {
        let mut current_scope = &self.scopes[self.current.0];
        loop {
            if let Some(id) = current_scope.lookup(name, table) {
                return Some(id);
            }

            current_scope = if let Some(parent) = current_scope.parent {
                &self.scopes[parent.0]
            } else {
                return None;
            }
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.scopes.push(Scope::new_with_parent(self.current));
        self.current = ScopeId(self.scopes.len() - 1);
    }

    pub(crate) fn leave_scope(&mut self) {
        self.current = self.scopes[self.current.0]
            .parent
            .expect("internal error: scope stack is empty");
    }
}

impl NameResolver {
    pub(crate) fn new() -> Self {
        Self {
            symbols: Default::default(),
            scopes: ScopeTree::new(),
            references: Default::default(),
            lvalue_bindings: Default::default(),
        }
    }

    /// Resolve lexical value names. Type annotations and member selection
    /// belong to later passes; this does not validate types or control flow.
    pub(crate) fn resolve(&mut self, loc: &ExpressionLocation) -> Result<(), NameResolutionError> {
        match &loc.expression {
            Expression::BoolLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::NumericLiteral(_)
            | Expression::Break
            | Expression::Continue => {}
            Expression::Identifier { name, .. } => {
                let symbol = self.lookup(name, loc.span)?;
                self.references.insert(loc.id, symbol);
            }
            Expression::Statement(inner)
            | Expression::Grouping(inner)
            | Expression::Cast { value: inner, .. }
            | Expression::Return { value: inner } => self.resolve(inner)?,
            Expression::Logical { left, right, .. } => {
                self.resolve(left)?;
                self.resolve(right)?;
            }
            Expression::VariableDeclaration { l_value, value, .. } => {
                self.resolve(value)?;
                self.declare_pattern(l_value, loc.span)?;
            }
            Expression::Assignment { l_value, r_value } => {
                self.resolve_target(l_value)?;
                self.resolve(r_value)?;
            }
            Expression::Block { statements } => {
                self.in_scope(|resolver| {
                    for statement in statements {
                        resolver.resolve(statement)?;
                    }
                    Ok(())
                })?;
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.resolve(condition)?;
                self.resolve(on_true)?;
                if let Some(on_false) = on_false {
                    self.resolve(on_false)?;
                }
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                self.resolve(expression)?;
                self.resolve(loop_body)?;
            }
            Expression::For { iterations, body } => self.resolve_for(iterations, body, loc.span)?,
            Expression::Call {
                function,
                arguments,
            }
            | Expression::OperatorCall {
                function,
                arguments,
            } => {
                self.resolve(function)?;
                for argument in arguments {
                    self.resolve(argument)?;
                }
            }
            Expression::MemberAccess { receiver, .. } => self.resolve(receiver)?,
            Expression::Tuple { values } | Expression::List { values } => {
                for value in values {
                    self.resolve(value)?;
                }
            }
            Expression::Map { values, default } => {
                for (key, value) in values {
                    self.resolve(key)?;
                    if let Some(value) = value {
                        self.resolve(value)?;
                    }
                }
                if let Some(default) = default {
                    self.resolve(default)?;
                }
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.resolve(start)?;
                }
                if let Some(end) = end {
                    self.resolve(end)?;
                }
            }
            Expression::FunctionDeclaration { name, body, .. } => {
                s
                return Err(NameResolutionError::Unsupported {
                    feature: "function declarations",
                    span: loc.span,
                });
            }
            Expression::StructDeclaration { .. } => {
                return Err(NameResolutionError::Unsupported {
                    feature: "struct declarations",
                    span: loc.span,
                });
            }
            Expression::OpAssignment { .. } => {
                return Err(NameResolutionError::Unsupported {
                    feature: "augmented assignment operator selection",
                    span: loc.span,
                });
            }
        }
        Ok(())
    }

    fn lookup(&self, name: &str, span: Span) -> Result<SymbolId, NameResolutionError> {
        self.scopes
            .lookup(name, &self.symbols)
            .ok_or_else(|| NameResolutionError::UnknownName {
                name: name.to_string(),
                span,
            })
    }

    fn declare_pattern(&mut self, pattern: &Lvalue, span: Span) -> Result<(), NameResolutionError> {
        // Validate the whole pattern before publishing any of its names.
        // The parser already rejects these targets in declarations, but a
        // malformed AST should still produce an error rather than new names.
        if pattern.non_binding_target().is_some() {
            return Err(NameResolutionError::InvalidDeclaration { span });
        }
        match pattern {
            Lvalue::Identifier {
                identifier, span, ..
            } => {
                let symbol = self.symbols.declare(identifier.clone());
                self.scopes.bind(symbol);
                self.lvalue_bindings.push((*span, symbol));
            }
            Lvalue::Sequence(items) => {
                for item in items {
                    self.declare_pattern(item, span)?;
                }
            }
            Lvalue::Index { .. } | Lvalue::Member { .. } => {
                unreachable!("validated declaration pattern");
            }
        }
        Ok(())
    }

    fn resolve_target(&mut self, target: &Lvalue) -> Result<(), NameResolutionError> {
        match target {
            Lvalue::Identifier {
                identifier, span, ..
            } => {
                let symbol = self.lookup(identifier, *span)?;
                self.lvalue_bindings.push((*span, symbol));
            }
            Lvalue::Sequence(items) => {
                for item in items {
                    self.resolve_target(item)?;
                }
            }
            Lvalue::Index { value, index, .. } => {
                self.resolve(value)?;
                self.resolve(index)?;
            }
            Lvalue::Member { receiver, .. } => self.resolve(receiver)?,
        }
        Ok(())
    }

    fn in_scope(
        &mut self,
        resolve: impl FnOnce(&mut Self) -> Result<(), NameResolutionError>,
    ) -> Result<(), NameResolutionError> {
        self.scopes.enter_scope();
        let result = resolve(self);
        // Restore visibility on errors as well as successful traversal.
        self.scopes.leave_scope();
        result
    }

    fn resolve_for(
        &mut self,
        iterations: &[ForIteration],
        body: &ForBody,
        span: Span,
    ) -> Result<(), NameResolutionError> {
        match iterations.split_first() {
            Some((ForIteration::Iteration { l_value, sequence }, tail)) => {
                // A binder is not visible in its own iterable. Later
                // iterables and guards can see the earlier binders.
                self.resolve(sequence)?;
                self.in_scope(|resolver| {
                    resolver.declare_pattern(l_value, span)?;
                    resolver.resolve_for(tail, body, span)
                })
            }
            Some((ForIteration::Guard(guard), tail)) => {
                self.resolve(guard)?;
                self.resolve_for(tail, body, span)
            }
            None => match body {
                ForBody::Block(block) | ForBody::List { expr: block } => self.resolve(block),
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    self.resolve(key)?;
                    if let Some(value) = value {
                        self.resolve(value)?;
                    }
                    if let Some(default) = default {
                        self.resolve(default)?;
                    }
                    Ok(())
                }
            },
        }
    }
}

#[cfg(test)]
mod resolver_tests;

#[cfg(test)]
mod test {
    use crate::name_resolution::Scope;
    use crate::name_resolution::ScopeTree;
    use crate::symbols::SymbolTable;

    #[test]
    fn binding_follows_shadowing_rules() {
        let mut table = SymbolTable::default();
        let first = table.declare("foo".to_string());
        let second = table.declare("foo".to_string());
        let other = table.declare("another".to_string());

        let mut scope = Scope::default();
        scope.bind(first);
        scope.bind(second);
        scope.bind(other);

        // The second binding shadows the first
        assert_eq!(scope.lookup("foo", &table), Some(second));

        // Invalid bindings return None
        assert_eq!(scope.lookup("nope", &table), None);

        // Bindings don't interfere
        assert_eq!(scope.lookup("another", &table), Some(other));
    }

    #[test]
    fn scope_tree_traverses() {
        let mut table = SymbolTable::default();
        let first = table.declare("foo".to_string());
        let middle = table.declare("bar".to_string());

        let mut tree = ScopeTree::new();
        tree.bind(first);
        tree.enter_scope();
        tree.bind(middle);
        tree.enter_scope();
        assert_eq!(tree.lookup("foo", &table), Some(first));
        assert_eq!(tree.lookup("bar", &table), Some(middle));
        assert_eq!(tree.lookup("missing", &table), None);

        tree.leave_scope();
        assert_eq!(tree.lookup("bar", &table), Some(middle));
        tree.leave_scope();
        assert_eq!(tree.lookup("foo", &table), Some(first));
        assert_eq!(tree.lookup("bar", &table), None);
    }

    #[test]
    fn scope_tree_shadows() {
        let mut table = SymbolTable::default();
        let first = table.declare("foo".to_string());
        let second = table.declare("foo".to_string());

        let mut tree = ScopeTree::new();
        tree.bind(first);
        tree.enter_scope();
        tree.bind(second);
        assert_eq!(tree.lookup("foo", &table), Some(second));

        tree.leave_scope();
        assert_eq!(tree.lookup("foo", &table), Some(first));
    }

    #[test]
    fn sibling_scopes_do_not_share_bindings() {
        let mut table = SymbolTable::default();
        let first = table.declare("first".to_string());
        let second = table.declare("second".to_string());

        let mut tree = ScopeTree::new();
        tree.enter_scope();
        tree.bind(first);
        tree.leave_scope();
        assert_eq!(tree.lookup("first", &table), None);

        tree.enter_scope();
        assert_eq!(tree.lookup("first", &table), None);
        tree.bind(second);
        assert_eq!(tree.lookup("second", &table), Some(second));
        tree.leave_scope();
        assert_eq!(tree.lookup("first", &table), None);
        assert_eq!(tree.lookup("second", &table), None);
    }

    #[test]
    fn lookup_does_not_change_where_new_bindings_are_created() {
        let mut table = SymbolTable::default();
        let outer = table.declare("outer".to_string());
        let inner = table.declare("inner".to_string());

        let mut tree = ScopeTree::new();
        tree.bind(outer);
        tree.enter_scope();
        assert_eq!(tree.lookup("outer", &table), Some(outer));
        assert_eq!(tree.lookup("missing", &table), None);

        tree.bind(inner);
        assert_eq!(tree.lookup("inner", &table), Some(inner));
        tree.leave_scope();
        assert_eq!(tree.lookup("inner", &table), None);
        assert_eq!(tree.lookup("outer", &table), Some(outer));
    }

    #[test]
    #[should_panic(expected = "internal error: scope stack is empty")]
    fn leaving_root_scope_is_rejected() {
        ScopeTree::new().leave_scope();
    }
}
