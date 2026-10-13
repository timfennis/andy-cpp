#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct SymbolId(usize);

#[derive(Debug)]
struct Symbol {
    name: String,
    kind: BindingKind,
}

#[derive(Debug, Default)]
pub(crate) struct SymbolTable {
    symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub(crate) fn declare(&mut self, name: String, kind: BindingKind) -> SymbolId {
        self.symbols.push(Symbol { name, kind });
        SymbolId(self.symbols.len() - 1)
    }

    pub(crate) fn name(&self, id: SymbolId) -> &str {
        &self.symbols[id.0].name
    }

    pub(crate) fn kind(&self, id: SymbolId) -> BindingKind {
        self.symbols[id.0].kind
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum BindingKind {
    /// Example: function declaration like `fn x(y)`: these join into an overload set
    Function,
    /// Example: bindings, parameters, and loop variables: these don't participate in overloading
    Plain,
}

#[cfg(test)]
mod test {
    use crate::symbols::{BindingKind, SymbolTable};

    #[test]
    fn it_tracks_multiple_identifiers() {
        let mut table = SymbolTable::default();
        let a = table.declare("foo".to_string(), BindingKind::Plain);
        let b = table.declare("bar".to_string(), BindingKind::Plain);

        assert_ne!(a, b);
        assert_eq!(table.name(a), "foo");
        assert_eq!(table.name(b), "bar");
    }

    #[test]
    fn identifiers_with_the_same_name_are_not_equal() {
        let mut table = SymbolTable::default();
        let a = table.declare("foo".to_string(), BindingKind::Function);
        let b = table.declare("foo".to_string(), BindingKind::Plain);

        assert_ne!(a, b);
    }

    #[test]
    fn it_tracks_binding_kinds() {
        let mut table = SymbolTable::default();
        let function = table.declare("foo".to_string(), BindingKind::Function);
        let plain = table.declare("bar".to_string(), BindingKind::Plain);

        assert_eq!(table.name(function), "foo");
        assert_eq!(table.kind(function), BindingKind::Function);
        assert_eq!(table.name(plain), "bar");
        assert_eq!(table.kind(plain), BindingKind::Plain);
    }
}
