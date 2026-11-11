//! A function's name used as a value, as `split` in `xs.map(split)`. When
//! the name has several overloads, the type expected where it's used picks
//! one: `map` passes one `String`, so `split(String)` is meant, not
//! `split(String, String)`.

use super::Analyser;
use super::solver::Constraint;
use crate::error::AnalysisError;
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_resolver::{BindingKind, SymbolId};

/// An overloaded function's name used as a value, waiting for the type
/// expected of it to say which overload is meant.
#[derive(Debug, Clone)]
pub(super) struct FunctionValue {
    id: NodeId,
    name: String,
    /// The overloads the name can mean, grouped by scope, nearest first.
    groups: Vec<Vec<SymbolId>>,
    /// The type expected of the value, which its uses settle.
    expected: StaticType,
    span: Span,
}

impl FunctionValue {
    pub(super) fn expected(&self) -> &StaticType {
        &self.expected
    }
}

/// Which overload a function value means, given what's known of the type
/// expected of it.
#[derive(Debug, PartialEq)]
pub(super) enum Choice {
    /// This overload.
    One(SymbolId),
    /// Several fit what's known so far.
    Undecided,
    /// None fits.
    None,
}

impl Analyser {
    /// Analyses an identifier read as a value and returns its type. A name
    /// with several overloads waits until the type expected of it picks one.
    pub(super) fn analyse_value_reference(
        &mut self,
        id: NodeId,
        name: &str,
        span: Span,
    ) -> StaticType {
        let symbol = self.resolver.innermost_reference(id);
        let groups = self.overloaded_function_groups(id);
        if groups.iter().map(Vec::len).sum::<usize>() > 1 {
            let expected = self.vars.fresh(StaticType::Any);
            let value = FunctionValue {
                id,
                name: name.to_string(),
                groups,
                expected: expected.clone(),
                span,
            };
            if !self.try_function_value(&value) {
                self.wait(Constraint::FunctionValue(value));
            }
            return self.vars.resolve(&expected);
        }
        self.picks
            .bindings
            .insert(id, Binding::Resolved(Candidate::Scalar(symbol)));
        // A generic function used as a value, as in `let p = push`, has one
        // type there, which its uses settle.
        self.instantiated_type(symbol)
    }

    /// The functions the name at `id` can mean, grouped by scope, nearest
    /// first, if the nearest thing it can mean is a function.
    fn overloaded_function_groups(&self, id: NodeId) -> Vec<Vec<SymbolId>> {
        let symbols = self.resolver.symbols();
        if symbols.kind(self.resolver.innermost_reference(id)) != BindingKind::Function {
            return Vec::new();
        }
        self.overloads()
            .reachable_groups(self.resolver.reference_candidates(id))
            .into_iter()
            .map(|group| {
                group
                    .into_iter()
                    .filter(|&symbol| symbols.kind(symbol) == BindingKind::Function)
                    .collect::<Vec<_>>()
            })
            .filter(|group| !group.is_empty())
            .collect()
    }

    /// `symbol`'s type with fresh variables for its type parameters.
    fn instantiated_type(&mut self, symbol: SymbolId) -> StaticType {
        let typ = self.vars.resolve(self.types.get_type(symbol));
        if typ.contains(&|part| matches!(part, StaticType::Generic(_))) {
            self.vars.instantiate(&typ)
        } else {
            typ
        }
    }

    /// Which overload `value` means with the expected type as it's known
    /// now: the one in the nearest scope that has any that fit, if it's the
    /// only one there. While the expected type isn't fully known, only an
    /// overload that's the only one fitting in any scope is chosen, as more
    /// knowledge could still rule out a nearer one.
    pub(super) fn choose_function_value(&mut self, value: &FunctionValue) -> Choice {
        let expected = self.vars.resolve(&value.expected);
        let fitting = value
            .groups
            .iter()
            .map(|group| {
                group
                    .iter()
                    .copied()
                    .filter(|&symbol| self.fits_expected(symbol, &expected))
                    .collect::<Vec<_>>()
            })
            .filter(|group| !group.is_empty())
            .collect::<Vec<_>>();
        let nearest = if expected.has_vars() {
            match &fitting[..] {
                [only] => only.clone(),
                [] => return Choice::None,
                _ => return Choice::Undecided,
            }
        } else {
            match fitting.first() {
                Some(nearest) => nearest.clone(),
                None => return Choice::None,
            }
        };
        match nearest[..] {
            [only] => Choice::One(only),
            // Overloads with the same parameters are a redefinition, which
            // is reported where it's declared. The newest one counts.
            [newest, ..] if self.all_take_the_same(&nearest) => Choice::One(newest),
            _ => Choice::Undecided,
        }
    }

    /// Whether the functions `symbols` all take the same parameter types.
    fn all_take_the_same(&self, symbols: &[SymbolId]) -> bool {
        let parameters = |symbol: SymbolId| match self.types.get_type(symbol) {
            StaticType::Function { parameters, .. } => parameters.clone(),
            _ => None,
        };
        symbols
            .iter()
            .all(|&symbol| parameters(symbol) == parameters(symbols[0]))
    }

    /// Whether a value of `symbol`'s type can be passed where `expected` is.
    fn fits_expected(&mut self, symbol: SymbolId, expected: &StaticType) -> bool {
        let snapshot = self.vars.snapshot();
        let typ = self.instantiated_type(symbol);
        let fits = self.vars.bind_argument(expected, &typ);
        self.vars.rollback(snapshot);
        fits
    }

    /// Picks the overload `value` means if what's known of the type expected
    /// of it decides it, and returns whether it did.
    pub(super) fn try_function_value(&mut self, value: &FunctionValue) -> bool {
        let expected = self.vars.resolve(&value.expected);
        match self.choose_function_value(value) {
            Choice::One(symbol) => {
                self.commit_function_value(value, symbol, expected.has_vars());
                true
            }
            Choice::Undecided if expected.has_vars() => false,
            Choice::Undecided => {
                let example = self.overload_example(value);
                self.emit(AnalysisError::ambiguous_function_value(
                    &value.name,
                    &self.vars.shown(&expected),
                    &example,
                    value.span,
                ));
                self.give_up_on_function_value(value);
                true
            }
            Choice::None => {
                self.emit(AnalysisError::no_overload_fits(
                    &value.name,
                    &self.vars.shown(&expected),
                    value.span,
                ));
                self.give_up_on_function_value(value);
                true
            }
        }
    }

    /// The type of the overload the name means innermost, as an example of
    /// a type that picks one.
    fn overload_example(&self, value: &FunctionValue) -> StaticType {
        let symbol = self.resolver.innermost_reference(value.id);
        self.vars.shown(self.types.get_type(symbol))
    }

    fn commit_function_value(&mut self, value: &FunctionValue, symbol: SymbolId, early: bool) {
        let typ = self.instantiated_type(symbol);
        self.vars.bind_argument(&value.expected, &typ);
        if early && cfg!(debug_assertions) {
            self.remember_early_function_value(value, symbol);
        }
        self.picks
            .bindings
            .insert(value.id, Binding::Resolved(Candidate::Scalar(symbol)));
    }

    /// Picks the nearest overload after an error, so analysis carries on
    /// without reporting the same value again.
    fn give_up_on_function_value(&mut self, value: &FunctionValue) {
        let symbol = self.resolver.innermost_reference(value.id);
        let typ = self.instantiated_type(symbol);
        self.vars.try_unify(&value.expected, &typ);
        self.picks
            .bindings
            .insert(value.id, Binding::Resolved(Candidate::Scalar(symbol)));
    }
}
