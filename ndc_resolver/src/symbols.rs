#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct SymbolId(usize);

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    kind: BindingKind,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn declare(&mut self, name: String, kind: BindingKind) -> SymbolId {
        self.symbols.push(Symbol { name, kind });
        SymbolId(self.symbols.len() - 1)
    }

    pub fn name(&self, id: SymbolId) -> &str {
        &self.symbols[id.0].name
    }

    pub fn kind(&self, id: SymbolId) -> BindingKind {
        self.symbols[id.0].kind
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BindingKind {
    /// Example: function declaration like `fn x(y)`: these join into an overload set
    Function,
    /// Example: bindings, parameters, and loop variables: these don't participate in overloading
    Plain,
    /// Example: `struct Foo { }`: these live in a separate namespace
    Type,
}

#[cfg(test)]
mod test {
    use crate::symbols::{BindingKind, SymbolTable};

    #[test]
    fn a_symbol_keeps_its_name_and_kind() {
        let mut table = SymbolTable::default();
        let function = table.declare("foo".to_string(), BindingKind::Function);
        let plain = table.declare("bar".to_string(), BindingKind::Plain);

        assert_eq!(table.name(function), "foo");
        assert_eq!(table.kind(function), BindingKind::Function);
        assert_eq!(table.name(plain), "bar");
        assert_eq!(table.kind(plain), BindingKind::Plain);
    }

    #[test]
    fn each_declaration_gets_its_own_symbol() {
        let mut table = SymbolTable::default();
        let first = table.declare("foo".to_string(), BindingKind::Plain);
        let second = table.declare("foo".to_string(), BindingKind::Plain);

        assert_ne!(first, second);
    }
}
