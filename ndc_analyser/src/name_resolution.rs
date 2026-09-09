use crate::symbols::{SymbolId, SymbolTable};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) struct ScopeId(usize);
#[derive(Debug, Default)]
pub(crate) struct Scope {
    parent: Option<ScopeId>,
    bindings: Vec<SymbolId>,
}

#[derive(Debug)]
pub(crate) struct ScopeTree {
    current: ScopeId,
    scopes: Vec<Scope>,
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
