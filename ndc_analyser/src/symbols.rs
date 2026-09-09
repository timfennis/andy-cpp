#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub(crate) struct SymbolId(usize);

#[derive(Debug, Default)]
pub(crate) struct SymbolTable {
    names: Vec<String>,
}

impl SymbolTable {
    pub(crate) fn declare(&mut self, name: String) -> SymbolId {
        self.names.push(name);
        SymbolId(self.names.len() - 1)
    }

    pub(crate) fn get(&self, id: SymbolId) -> &str {
        &self.names[id.0]
    }
}

#[cfg(test)]
mod test {
    use crate::symbols::SymbolTable;

    #[test]
    fn it_tracks_multiple_identifiers() {
        let mut table = SymbolTable::default();
        let a = table.declare("foo".to_string());
        let b = table.declare("bar".to_string());

        assert_ne!(a, b);
        assert_eq!(table.get(a), "foo");
        assert_eq!(table.get(b), "bar");
    }

    #[test]
    fn identifiers_with_the_same_name_are_not_equal() {
        let mut table = SymbolTable::default();
        let a = table.declare("foo".to_string());
        let b = table.declare("foo".to_string());

        assert_ne!(a, b);
    }
}
