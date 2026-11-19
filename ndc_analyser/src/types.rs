use bounds::Bounds;
use ndc_core::collections::{HashMap, HashSet};
use ndc_core::{Builtins, StaticType, TypeVar};
use ndc_lexer::Span;
use ndc_resolver::SymbolId;
use std::rc::Rc;

mod bounds;
mod literals;
mod store;
mod unify;

use store::Undo;
use unify::Deferred;

/// The type of every value the name resolver declared, keyed by its symbol.
/// A value keeps its type: an assignment must fit it. A built-in's type is
/// the one [`Builtins`] gives for its slot, which may only be worked out when
/// it's first asked for.
pub(crate) struct TypeTable {
    types: HashMap<SymbolId, StaticType>,
    builtins: Rc<dyn Builtins>,
    /// The global slot of each built-in's symbol.
    builtin_slots: HashMap<SymbolId, usize>,
}

impl Default for TypeTable {
    /// A table of no built-ins.
    fn default() -> Self {
        Self::with_builtins(
            Rc::new(Vec::<(String, StaticType, Option<ndc_core::Intrinsic>)>::new()),
            HashMap::default(),
        )
    }
}

impl std::fmt::Debug for TypeTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeTable")
            .field("types", &self.types)
            .field("builtin_slots", &self.builtin_slots)
            .finish_non_exhaustive()
    }
}

impl TypeTable {
    /// A table whose built-ins are `builtins`, each under the symbol
    /// `builtin_slots` gives its slot.
    pub(crate) fn with_builtins(
        builtins: Rc<dyn Builtins>,
        builtin_slots: HashMap<SymbolId, usize>,
    ) -> Self {
        Self {
            types: HashMap::default(),
            builtins,
            builtin_slots,
        }
    }

    /// The global slot of `symbol`, if it's a built-in's.
    pub(crate) fn builtin_slot(&self, symbol: SymbolId) -> Option<usize> {
        self.builtin_slots.get(&symbol).copied()
    }

    /// The type parameters `symbol`'s signature declares, if it's a
    /// built-in's, in the order `f::<…>` binds them in.
    pub(crate) fn builtin_type_parameters(
        &self,
        symbol: SymbolId,
    ) -> Option<std::borrow::Cow<'_, [StaticType]>> {
        let slot = self.builtin_slot(symbol)?;
        Some(self.builtins.type_parameters(slot))
    }

    /// Records the type of a value the resolver declared. A function's
    /// placeholder type is replaced this way once its body is analysed.
    pub(crate) fn declare(&mut self, symbol: SymbolId, typ: StaticType) {
        self.types.insert(symbol, typ);
    }

    pub(crate) fn get_type(&self, symbol: SymbolId) -> &StaticType {
        if let Some(typ) = self.types.get(&symbol) {
            return typ;
        }
        let slot = self
            .builtin_slot(symbol)
            .expect("internal error: a value is analysed after its declaration");
        self.builtins.static_type(slot)
    }

    /// Replaces the settled type variables in every value's type with what
    /// they stand for. Unless `open` allows it, as after a REPL line, none
    /// is left unsettled. A built-in's type holds no type variables.
    pub(crate) fn resolve_all(&mut self, vars: &TypeVars, open: bool) {
        for typ in self.types.values_mut() {
            *typ = vars.resolve(typ);
            debug_assert!(open || !typ.has_vars(), "unsettled type variable in {typ}");
        }
    }
}

/// What each type variable of the analysis stands for once something settled
/// it. In `let xs = []; xs[0] = 1;`, the element type of `xs` is a variable
/// that the assignment settles as `Int`.
#[derive(Debug, Default)]
pub(crate) struct TypeVars {
    /// What each variable stands for, once settled.
    solutions: Vec<Option<StaticType>>,
    /// What each variable becomes if nothing settles it: `Unknown`, as
    /// for what `[]` holds, or `Any` for a literal's type whose values
    /// don't say what it is, or a failed check's type: see
    /// [`Self::fall_back_as_errors`].
    fallbacks: Vec<StaticType>,
    /// The variables in the order they were settled, so a [`store::Snapshot`] can
    /// unsettle the ones settled after it, and the solver can tell which
    /// ones were settled since it last looked.
    trail: Vec<TypeVar>,
    /// How deep in generic functions each variable belongs: 0 for the
    /// program's top level, 1 for the body of a top-level `fn`. A variable
    /// that a function's body made but that something outside it now refers
    /// to, like `n` in `let xs = []; fn f(n) { xs.push(n) }`, belongs outside.
    levels: Vec<u32>,
    /// The level new variables get.
    level: u32,
    /// What each variable may stand for beyond what unifying says, which
    /// settling it checks: the type of the literal `1` is an `Int`, a
    /// `Float` or a `Number`.
    bounds: Vec<Bounds>,
    /// What was changed since the variables were made, other than what the
    /// trail says was settled, so a [`store::Snapshot`] can take it back.
    undo: Vec<Undo>,
    /// The variables that got a bound, in the order they got it, which are
    /// the types of literals.
    literals: Vec<TypeVar>,
    /// Where each literal holding values is, keyed by the type its elements
    /// were made as: the whole of `[1, "a"]`, or of an `if`.
    literal_spans: HashMap<TypeVar, Span>,
    /// The literals settled as [`StaticType::Error`] because their values
    /// share no type, which the analyser reports.
    unshared: Vec<(TypeVar, Unshared)>,
    /// The types of the `if`s and `match`es whose branches aren't known yet:
    /// see [`Self::mark_join`].
    joins: HashSet<TypeVar>,
    /// Those an `Any` was expected of.
    expects_any: Vec<TypeVar>,
    /// While [`Self::bind_arguments`] runs, the values passed for the type
    /// parameters, which decide them once every argument is in.
    deferred: Option<Deferred>,
    /// The variables made as what an empty `[]`, `%{}` or `None` holds, in
    /// the order they were made: see [`Self::fresh_contents`].
    contents: Vec<TypeVar>,
}

/// Two values of a literal that share no type, as `1` and `"a"` in `[1,
/// "a"]`, with where the literal is.
#[derive(Debug, Clone)]
pub(crate) struct Unshared {
    /// The literal's element type, as it was made.
    pub(crate) literal: TypeVar,
    pub(crate) first: StaticType,
    pub(crate) second: StaticType,
    pub(crate) span: Span,
}

#[cfg(test)]
mod tests {
    use super::TypeVars;
    use ndc_core::StaticType;
    use ndc_core::collections::HashSet;
    use ndc_lexer::Span;

    fn span() -> Span {
        Span::synthetic()
    }

    #[test]
    fn an_attempt_keeps_what_it_settled_only_when_it_succeeds() {
        let mut vars = TypeVars::default();
        let var = vars.fresh();
        assert!(!vars.attempt(|vars| vars.unify(&var, &StaticType::Int) && false));
        assert_eq!(vars.resolve(&var), var);
        assert!(vars.attempt(|vars| vars.unify(&var, &StaticType::Int)));
        assert_eq!(vars.resolve(&var), StaticType::Int);
    }

    #[test]
    fn a_probe_takes_back_what_it_settled_and_made() {
        let mut vars = TypeVars::default();
        let var = vars.fresh();
        let made = vars.probe(|vars| {
            vars.fresh();
            vars.unify(&var, &StaticType::Int)
        });
        assert!(made);
        assert_eq!(vars.resolve(&var), var);
        assert_eq!(vars.count(), 1);
    }

    #[test]
    fn a_rollback_takes_back_a_lowered_level() {
        let mut vars = TypeVars::default();
        vars.enter_level();
        let StaticType::Var(var) = vars.fresh() else {
            unreachable!("a fresh type is a variable");
        };
        vars.leave_level();
        vars.probe(|vars| vars.lower_to_current(&StaticType::Var(var)));
        assert!(vars.is_generalizable(var));
        vars.lower_to_current(&StaticType::Var(var));
        assert!(!vars.is_generalizable(var));
    }

    #[test]
    fn a_rollback_takes_back_a_fallback() {
        let mut vars = TypeVars::default();
        let var = vars.fresh();
        vars.probe(|vars| vars.fall_back_as_errors(&var));
        vars.settle_from(0, &HashSet::default());
        assert_eq!(vars.resolve(&var), StaticType::Unknown);
    }

    #[test]
    fn a_number_literal_is_a_number_or_any() {
        let mut vars = TypeVars::default();
        for typ in [
            StaticType::Int,
            StaticType::Float,
            StaticType::Number,
            StaticType::Any,
        ] {
            let literal = vars.fresh_number(span());
            assert!(vars.try_unify(&literal, &typ), "{typ}");
        }
        let literal = vars.fresh_number(span());
        assert!(!vars.try_unify(&literal, &StaticType::String));
    }

    #[test]
    fn a_literal_holds_what_each_value_fits() {
        let mut vars = TypeVars::default();
        let number = vars.fresh_number(span());
        let element = vars.fresh_holding(
            vec![(number.clone(), span()), (StaticType::Float, span())],
            span(),
        );
        assert!(!vars.try_unify(&element, &StaticType::String));
        assert!(vars.try_unify(&element, &StaticType::Float));
        // Holding a Float made the number a Float too.
        assert_eq!(vars.resolve(&number), StaticType::Float);
    }

    #[test]
    fn a_rollback_takes_back_the_bounds_another_variable_got() {
        let mut vars = TypeVars::default();
        let literal = vars.fresh_number(span());
        let other = vars.fresh();
        vars.probe(|vars| {
            assert!(vars.unify(&literal, &other));
            // `other` is the literal now, so it can't be a String.
            assert!(!vars.try_unify(&other, &StaticType::String));
        });
        assert!(vars.try_unify(&other, &StaticType::String));
    }

    #[test]
    fn a_literal_defaults_to_what_its_values_share() {
        let mut vars = TypeVars::default();
        let number = vars.fresh_number(span());
        let StaticType::Var(element) =
            vars.fresh_holding(vec![(number, span()), (StaticType::Float, span())], span())
        else {
            unreachable!("a literal's type is a variable");
        };
        assert_eq!(
            vars.literal_defaults(element),
            Some(vec![StaticType::Float])
        );
        // Values that share no type are an error.
        let StaticType::Var(mixed) = vars.fresh_holding(
            vec![(StaticType::String, span()), (StaticType::Int, span())],
            span(),
        ) else {
            unreachable!("a literal's type is a variable");
        };
        vars.settle_with_fallbacks(&[mixed]);
        assert_eq!(vars.resolve(&StaticType::Var(mixed)), StaticType::Error);
        let unshared = vars.take_unshared();
        assert_eq!(unshared.len(), 1);
        assert_eq!(
            (&unshared[0].first, &unshared[0].second),
            (&StaticType::String, &StaticType::Int)
        );
    }
}
