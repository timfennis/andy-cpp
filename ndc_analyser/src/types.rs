use ahash::AHashMap;
use ndc_core::StaticType;
use ndc_resolver::SymbolId;

#[derive(Debug, Clone)]
pub(crate) enum TypeBinding {
    Inferred(StaticType),
    Annotated(StaticType),
}

impl TypeBinding {
    pub fn typ(&self) -> &StaticType {
        match self {
            Self::Inferred(t) | Self::Annotated(t) => t,
        }
    }

    pub fn is_annotated(&self) -> bool {
        matches!(self, Self::Annotated(_))
    }
}

/// The type of every value the name resolver declared, natives included,
/// keyed by its symbol. An assignment can widen an inferred type, but an
/// annotated one is fixed.
#[derive(Debug, Default)]
pub(crate) struct TypeTable {
    types: AHashMap<SymbolId, TypeBinding>,
}

impl TypeTable {
    /// Records the type of a value the resolver declared.
    pub(crate) fn declare(&mut self, symbol: SymbolId, binding: TypeBinding) {
        self.types.insert(symbol, binding);
    }

    pub(crate) fn get_type(&self, symbol: SymbolId) -> &StaticType {
        self.types
            .get(&symbol)
            .expect("internal error: a value is analysed after its declaration")
            .typ()
    }

    /// Try to update a value's type. Returns `Err` with the annotated type if
    /// the value has an explicit type annotation and cannot be widened.
    pub(crate) fn update_binding_type(
        &mut self,
        symbol: SymbolId,
        new_type: StaticType,
    ) -> Result<(), StaticType> {
        let binding = self
            .types
            .get_mut(&symbol)
            .expect("internal error: a value is analysed after its declaration");
        if binding.is_annotated() {
            return Err(binding.typ().clone());
        }
        *binding = TypeBinding::Inferred(new_type);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_resolver::{BindingKind, SymbolTable};

    #[test]
    fn inferred_types_widen_but_annotated_types_are_fixed() {
        let mut symbols = SymbolTable::default();
        let inferred = symbols.declare("a".to_string(), BindingKind::Plain);
        let annotated = symbols.declare("b".to_string(), BindingKind::Plain);
        let mut table = TypeTable::default();
        table.declare(inferred, TypeBinding::Inferred(StaticType::Int));
        table.declare(annotated, TypeBinding::Annotated(StaticType::Int));

        assert_eq!(
            table.update_binding_type(inferred, StaticType::Number),
            Ok(())
        );
        assert_eq!(table.get_type(inferred), &StaticType::Number);
        assert_eq!(
            table.update_binding_type(annotated, StaticType::Number),
            Err(StaticType::Int)
        );
        assert_eq!(table.get_type(annotated), &StaticType::Int);
    }
}
