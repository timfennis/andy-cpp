use crate::symbols::{BindingKind, SymbolId, SymbolTable};
use ahash::{AHashMap, AHashSet};
use ndc_lexer::Span;
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForBody, ForIteration, NodeId,
};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) struct ScopeId(usize);

#[derive(Debug)]
pub(crate) struct Scope {
    parent: Option<ScopeId>,
    bindings: Vec<SymbolId>,
    kind: ScopeKind,
}

pub(crate) struct ScopeTree {
    current: ScopeId,
    scopes: Vec<Scope>,
    bound_in: AHashMap<SymbolId, ScopeId>,
}

pub(crate) struct NameResolver {
    symbols: SymbolTable,
    scopes: ScopeTree,
    /// The symbol each binding pattern identifier or named function
    /// introduces. In `let (a, b) = ...`, keyed by the `a` and `b` pattern
    /// nodes. In `fn a(x) { }`, keyed by the function declaration's node,
    /// since the name has no node of its own.
    declarations: AHashMap<NodeId, SymbolId>,
    /// Every symbol an identifier use could mean, keyed by the identifier's
    /// node. The type pass picks between them.
    references: AHashMap<NodeId, Candidates>,
    /// The symbol each identifier assignment target writes to, keyed by the
    /// target's node. In `x = 1`, the `x` node.
    assignments: AHashMap<NodeId, SymbolId>,
    /// The outer symbols each function captures, keyed by its declaration node, in order of first use.
    /// In `fn outer() { let x = 1; fn inner() { x } }`, `inner` captures `[x]`.
    captures: AHashMap<NodeId, Vec<SymbolId>>,
    /// Symbols some closure captures. They must outlive their scope (see #227).
    captured: AHashSet<SymbolId>,
    /// The operations each augmented assignment can use, keyed by its node.
    /// In `x += 1`, the candidates for `+=` and `+`.
    augmented: AHashMap<NodeId, AugmentedCandidates>,
    /// Getter candidates for reads through an accessor, keyed by the reading node.
    /// For `p.x`, the member access node's `x`. For `xs[i] += 1`, the target node's `[]`.
    getters: AHashMap<NodeId, Candidates>,
    /// Setter candidates for writes through an accessor, keyed by the target node.
    /// In `p.x = 1`, `x=`. In `xs[i] = 1`, `[]=`.
    setters: AHashMap<NodeId, Candidates>,
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum NameResolutionError {
    #[error("Identifier {name} has not previously been declared")]
    UnknownName { name: String, span: Span },
    #[error("Name resolution for {feature} is not implemented yet")]
    Unsupported { feature: &'static str, span: Span },
    #[error("Illegal redefinition of parameter {name}")]
    DuplicateParameter { name: String, span: Span },
    #[error("Illegal redefinition of variable {name} as a function in the same scope")]
    IllegalFunctionRedefinition { name: String, span: Span },
    #[error("This lvalue is required to be a single identifier")]
    SequenceTargetNotAllowed { span: Span },
}

/// Same-named symbols visible at a use, nearest scope first, newest first
/// within a scope. For `x` in `let x = 1; { let x = 2; x }`: `[[x2], [x1]]`.
/// Identifiers always have at least one candidate, augmented operators may
/// not.
#[derive(Debug)]
pub(crate) struct Candidates(Vec<Vec<SymbolId>>);

/// The two operations an augmented assignment can use. For `xs ++= ys`:
/// `in_place` holds `++=` (mutates `xs`), `operator` holds `++` (builds a new value
/// that is written back). Either may be empty; the type pass picks.
#[derive(Debug)]
pub(crate) struct AugmentedCandidates {
    in_place: Candidates,
    operator: Candidates,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ScopeKind {
    /// Natives such as `len` and `+`. The VM keeps them as globals, so they're never captured.
    Builtins,
    /// The program's top level. In `let x = 1; fn f() { x }`, `x` lives here and `f` captures it.
    Program,
    /// The parameters of the function declared at this node, named or lambda.
    Function(NodeId),
    /// `{ … }` bodies and `for` iterations. Not a function boundary.
    Block,
}

impl Scope {
    pub(crate) fn new(kind: ScopeKind, parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            bindings: Vec::default(),
            kind,
        }
    }

    pub(crate) fn bind(&mut self, symbol_id: SymbolId) {
        self.bindings.push(symbol_id);
    }

    pub(crate) fn lookup(&self, name: &str, table: &SymbolTable) -> Option<SymbolId> {
        for id in self.bindings.iter().rev() {
            if table.name(*id) == name {
                return Some(*id);
            }
        }
        None
    }

    fn candidates(&self, name: &str, table: &SymbolTable) -> Vec<SymbolId> {
        self.bindings
            .iter()
            .rev()
            .filter(|id| table.name(**id) == name)
            .copied()
            .collect()
    }
}

impl ScopeTree {
    pub(crate) fn new() -> Self {
        Self {
            current: ScopeId(0),
            scopes: vec![Scope::new(ScopeKind::Builtins, None)],
            bound_in: Default::default(),
        }
    }

    pub(crate) fn bind(&mut self, symbol_id: SymbolId) {
        self.scopes[self.current.0].bind(symbol_id);
        self.bound_in.insert(symbol_id, self.current);
    }

    /// The current scope followed by its ancestors, ending at the root.
    fn chain(&self) -> impl Iterator<Item = &Scope> {
        self.ancestors(self.current).map(|id| &self.scopes[id.0])
    }

    /// `from` and its ancestors, ending at the root.
    fn ancestors(&self, from: ScopeId) -> impl Iterator<Item = ScopeId> {
        std::iter::successors(Some(from), |id| self.scopes[id.0].parent)
    }

    /// Functions a use of `symbol` from the current scope reaches through, innermost
    /// first. In `fn f() { let y = 1; fn g() { y } }`, a use of `y` in `g` crosses `[g]`.
    /// Natives are globals, so uses of them cross nothing.
    pub(crate) fn crossed_functions(&self, symbol: SymbolId) -> Vec<NodeId> {
        let bound_in = self.bound_in[&symbol];

        if self.scopes[bound_in.0].kind == ScopeKind::Builtins {
            return Vec::new();
        }

        self.ancestors(self.current)
            .take_while(|&id| id != bound_in)
            .filter_map(|id| match self.scopes[id.0].kind {
                ScopeKind::Function(node) => Some(node),
                _ => None,
            })
            .collect()
    }

    pub(crate) fn lookup(&self, name: &str, table: &SymbolTable) -> Option<SymbolId> {
        self.chain().find_map(|scope| scope.lookup(name, table))
    }

    pub(crate) fn enter_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind, Some(self.current)));
        self.current = ScopeId(self.scopes.len() - 1);
    }

    pub(crate) fn leave_scope(&mut self) {
        self.current = self.scopes[self.current.0]
            .parent
            .expect("internal error: scope stack is empty");
    }

    pub(crate) fn candidates(&self, name: &str, table: &SymbolTable) -> Candidates {
        Candidates(
            self.chain()
                .map(|scope| scope.candidates(name, table))
                .filter(|group| !group.is_empty())
                .collect(),
        )
    }

    /// Same-named symbols in the current scope only, newest first. Empty when
    /// the name is unbound here, even if an enclosing scope binds it.
    pub(crate) fn current_candidates(&self, name: &str, table: &SymbolTable) -> Vec<SymbolId> {
        self.scopes[self.current.0].candidates(name, table)
    }
}

impl NameResolver {
    /// Creates a new name resolver with the supplied `builtins` declared in a scope outside
    /// the program. With `new(["len"])`, a top-level `let len = 99` shadows the native
    /// instead of sharing its scope, so a later `len` resolves to `[[let len], [native len]]`.
    pub(crate) fn new<'a>(builtins: impl IntoIterator<Item = &'a str>) -> Self {
        let mut scopes = ScopeTree::new();
        let mut symbols = SymbolTable::default();

        for builtin in builtins {
            let symbol = symbols.declare(builtin.to_string(), BindingKind::Function);
            scopes.bind(symbol);
        }

        scopes.enter_scope(ScopeKind::Program);
        Self {
            symbols,
            scopes,
            declarations: Default::default(),
            references: Default::default(),
            assignments: Default::default(),
            captures: Default::default(),
            captured: Default::default(),
            augmented: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        }
    }

    /// Records a use of `symbol` from the current scope. Every function the use
    /// reaches through captures it. In `fn outer() { let x = 1; fn inner() { x } }`,
    /// the use of `x` adds `x` to `inner`'s captures.
    fn record_use(&mut self, symbol: SymbolId) {
        let crossed = self.scopes.crossed_functions(symbol);
        if crossed.is_empty() {
            return;
        }

        for node in crossed {
            let captures = self.captures.entry(node).or_default();
            if !captures.contains(&symbol) {
                captures.push(symbol);
            }
        }

        self.captured.insert(symbol);
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
                let candidates = self.use_name(name, loc.span)?;
                self.references.insert(loc.id, candidates);
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
                self.declare_pattern(l_value);
            }
            Expression::Assignment { l_value, r_value } => {
                self.resolve_target(l_value)?;
                self.resolve(r_value)?;
            }
            Expression::Block { statements } => {
                self.in_scope(ScopeKind::Block, |resolver| {
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
            Expression::For { iterations, body } => self.resolve_for(iterations, body)?,
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
            Expression::MemberAccess {
                receiver,
                member,
                member_span,
                ..
            } => {
                self.resolve(receiver)?;
                let candidates = self.use_name(member, *member_span)?;
                self.getters.insert(loc.id, candidates);
            }
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
            Expression::FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            } => {
                if let Some(name) = name {
                    if self
                        .scopes
                        .current_candidates(name, &self.symbols)
                        .iter()
                        .any(|binding| self.symbols.kind(*binding) == BindingKind::Plain)
                    {
                        return Err(NameResolutionError::IllegalFunctionRedefinition {
                            name: name.clone(),
                            span: loc.span,
                        });
                    }
                    let symbol = self.symbols.declare(name.clone(), BindingKind::Function);
                    self.scopes.bind(symbol);
                    self.declarations.insert(loc.id, symbol);
                }

                // Parameter lists are short, so a linear scan beats hashing.
                let mut names = Vec::new();
                self.in_scope(ScopeKind::Function(loc.id), |resolver| {
                    for parameter in parameters {
                        let param_name = parameter.name();
                        if names.contains(&param_name) {
                            return Err(NameResolutionError::DuplicateParameter {
                                name: param_name.to_string(),
                                span: parameter.span,
                            });
                        }
                        names.push(param_name);
                        resolver.declare_pattern(&parameter.lvalue);
                    }

                    resolver.resolve(body)
                })?;
            }
            Expression::StructDeclaration { .. } => {
                return Err(NameResolutionError::Unsupported {
                    feature: "struct declarations",
                    span: loc.span,
                });
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
                ..
            } => {
                if let AssignmentTarget::Sequence(_) = l_value.target {
                    return Err(NameResolutionError::SequenceTargetNotAllowed { span: loc.span });
                }

                self.resolve_target(l_value)?;
                // Reading the old value goes through the target's getter.
                // Identifiers are read directly, and sequences were rejected above.
                let getter = match &l_value.target {
                    AssignmentTarget::Index { .. } => Some(self.use_name("[]", l_value.span)?),
                    AssignmentTarget::Member {
                        member,
                        member_span,
                        ..
                    } => Some(self.use_name(member, *member_span)?),
                    AssignmentTarget::Identifier { .. } | AssignmentTarget::Sequence(_) => None,
                };
                if let Some(getter) = getter {
                    self.getters.insert(l_value.id, getter);
                }
                self.resolve(r_value)?;

                let in_place_op_name = format!("{operation}=");
                // Ask the scope tree directly: an empty list isn't an error here.
                let in_place_candidates = self.scopes.candidates(&in_place_op_name, &self.symbols);
                let candidates = self.scopes.candidates(operation, &self.symbols);

                if in_place_candidates.0.is_empty() && candidates.0.is_empty() {
                    return Err(NameResolutionError::UnknownName {
                        name: operation.clone(),
                        // The operator has no span of its own, so point at the whole assignment.
                        span: loc.span,
                    });
                }

                in_place_candidates
                    .iter()
                    .chain(candidates.iter())
                    .flatten()
                    .for_each(|op| self.record_use(*op));

                self.augmented.insert(
                    loc.id,
                    AugmentedCandidates {
                        in_place: in_place_candidates,
                        operator: candidates,
                    },
                );
            }
        }
        Ok(())
    }

    /// Resolves a use of `name` from the current scope: every candidate it could
    /// mean, each recorded as a use so closures capture it. Any candidate may be
    /// the one the type pass picks, so all of them are captured.
    fn use_name(&mut self, name: &str, span: Span) -> Result<Candidates, NameResolutionError> {
        let candidates = self.scopes.candidates(name, &self.symbols);

        if candidates.0.is_empty() {
            return Err(NameResolutionError::UnknownName {
                name: name.to_string(),
                span,
            });
        }

        candidates
            .iter()
            .flatten()
            .for_each(|symbol| self.record_use(*symbol));

        Ok(candidates)
    }

    fn lookup(&self, name: &str, span: Span) -> Result<SymbolId, NameResolutionError> {
        self.scopes
            .lookup(name, &self.symbols)
            .ok_or_else(|| NameResolutionError::UnknownName {
                name: name.to_string(),
                span,
            })
    }

    fn declare_pattern(&mut self, pattern: &BindingPatternLocation) {
        match &pattern.pattern {
            BindingPattern::Identifier { identifier, .. } => {
                let symbol = self
                    .symbols
                    .declare(identifier.to_owned(), BindingKind::Plain);
                self.scopes.bind(symbol);
                self.declarations.insert(pattern.id, symbol);
            }
            BindingPattern::Sequence(items) => {
                for item in items {
                    self.declare_pattern(item);
                }
            }
        }
    }

    fn resolve_target(
        &mut self,
        target: &AssignmentTargetLocation,
    ) -> Result<(), NameResolutionError> {
        match &target.target {
            AssignmentTarget::Identifier {
                identifier, span, ..
            } => {
                let symbol = self.lookup(identifier, *span)?;
                self.record_use(symbol);
                self.assignments.insert(target.id, symbol);
            }
            AssignmentTarget::Sequence(items) => {
                for item in items {
                    self.resolve_target(item)?;
                }
            }
            AssignmentTarget::Index { value, index, .. } => {
                self.resolve(value)?;
                self.resolve(index)?;
                let candidates = self.use_name("[]=", target.span)?;
                self.setters.insert(target.id, candidates);
            }
            AssignmentTarget::Member {
                receiver,
                member,
                member_span,
                ..
            } => {
                self.resolve(receiver)?;
                let candidates = self.use_name(&format!("{member}="), *member_span)?;
                self.setters.insert(target.id, candidates);
            }
        }
        Ok(())
    }

    fn in_scope(
        &mut self,
        kind: ScopeKind,
        resolve: impl FnOnce(&mut Self) -> Result<(), NameResolutionError>,
    ) -> Result<(), NameResolutionError> {
        self.scopes.enter_scope(kind);
        let result = resolve(self);
        // Restore visibility on errors as well as successful traversal.
        self.scopes.leave_scope();
        result
    }

    fn resolve_for(
        &mut self,
        iterations: &[ForIteration],
        body: &ForBody,
    ) -> Result<(), NameResolutionError> {
        match iterations.split_first() {
            Some((ForIteration::Iteration { l_value, sequence }, tail)) => {
                // A binder is not visible in its own iterable. Later
                // iterables and guards can see the earlier binders.
                self.resolve(sequence)?;
                self.in_scope(ScopeKind::Block, |resolver| {
                    resolver.declare_pattern(l_value);
                    resolver.resolve_for(tail, body)
                })
            }
            Some((ForIteration::Guard(guard), tail)) => {
                self.resolve(guard)?;
                self.resolve_for(tail, body)
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

impl Candidates {
    fn iter(&self) -> impl Iterator<Item = &Vec<SymbolId>> {
        self.0.iter()
    }
}

#[cfg(test)]
mod resolver_tests;

#[cfg(test)]
mod test {
    use crate::name_resolution::{Scope, ScopeKind, ScopeTree};
    use crate::symbols::{BindingKind, SymbolTable};

    #[test]
    fn binding_follows_shadowing_rules() {
        let mut table = SymbolTable::default();
        let first = table.declare("foo".to_string(), BindingKind::Plain);
        let second = table.declare("foo".to_string(), BindingKind::Plain);
        let other = table.declare("another".to_string(), BindingKind::Plain);

        let mut scope = Scope::new(ScopeKind::Block, None);
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
        let first = table.declare("foo".to_string(), BindingKind::Plain);
        let middle = table.declare("bar".to_string(), BindingKind::Plain);

        let mut tree = ScopeTree::new();
        tree.bind(first);
        tree.enter_scope(ScopeKind::Block);
        tree.bind(middle);
        tree.enter_scope(ScopeKind::Block);
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
        let first = table.declare("foo".to_string(), BindingKind::Plain);
        let second = table.declare("foo".to_string(), BindingKind::Plain);

        let mut tree = ScopeTree::new();
        tree.bind(first);
        tree.enter_scope(ScopeKind::Block);
        tree.bind(second);
        assert_eq!(tree.lookup("foo", &table), Some(second));

        tree.leave_scope();
        assert_eq!(tree.lookup("foo", &table), Some(first));
    }

    #[test]
    fn sibling_scopes_do_not_share_bindings() {
        let mut table = SymbolTable::default();
        let first = table.declare("first".to_string(), BindingKind::Plain);
        let second = table.declare("second".to_string(), BindingKind::Plain);

        let mut tree = ScopeTree::new();
        tree.enter_scope(ScopeKind::Block);
        tree.bind(first);
        tree.leave_scope();
        assert_eq!(tree.lookup("first", &table), None);

        tree.enter_scope(ScopeKind::Block);
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
        let outer = table.declare("outer".to_string(), BindingKind::Plain);
        let inner = table.declare("inner".to_string(), BindingKind::Plain);

        let mut tree = ScopeTree::new();
        tree.bind(outer);
        tree.enter_scope(ScopeKind::Block);
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
