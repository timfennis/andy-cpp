//! A function's name used as a value, as `split` in `xs.map(split)`. When
//! the name has several overloads, the type expected where it's used picks
//! one: `map` passes one `String`, so `split(String)` is meant, not
//! `split(String, String)`.

use super::Analyser;
use super::solver::Constraint;
use crate::error::AnalysisError;
use crate::overloads::CallKind;
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_resolver::{BindingKind, Candidates, SymbolId};

/// An overloaded function's name used as a value, waiting for the type
/// expected of it to say which overload is meant.
#[derive(Debug, Clone)]
pub(super) struct FunctionValue {
    id: NodeId,
    name: String,
    /// What the name can mean, as for a call of it.
    candidates: Candidates,
    /// The functions among them, grouped by scope, nearest first.
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
        let groups = self.overloaded_function_groups(id, symbol);
        if groups.iter().map(Vec::len).sum::<usize>() > 1 {
            let expected = self.vars.fresh(StaticType::Any);
            let value = FunctionValue {
                id,
                name: name.to_string(),
                candidates: self.resolver.reference_candidates(id).clone(),
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
    /// first, if the nearest thing it can mean, `innermost`, is a function.
    fn overloaded_function_groups(&self, id: NodeId, innermost: SymbolId) -> Vec<Vec<SymbolId>> {
        let symbols = self.resolver.symbols();
        if symbols.kind(innermost) != BindingKind::Function {
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

    /// Which overload `value` means where a value of type `expected` is
    /// expected. Once the expected function's parameter types are known, it
    /// means the overload a call with arguments of those types would pick,
    /// so the nearest scope, the most specific overload and `Any` losing to
    /// an exact type all work as for calls, and the overload must return
    /// what's expected too. Before that, only an overload that's the only
    /// one fitting in any scope is chosen, as more knowledge could still
    /// rule out a nearer one.
    pub(super) fn choose_function_value(
        &mut self,
        value: &FunctionValue,
        expected: &StaticType,
    ) -> Choice {
        if let StaticType::Function {
            parameters: Some(parameters),
            ..
        } = expected
            && !parameters.iter().any(StaticType::has_vars)
        {
            let binding = self
                .overloads()
                .resolve_call(&value.candidates, parameters, CallKind::Regular)
                .binding;
            return match binding {
                Binding::Resolved(Candidate::Scalar(symbol))
                    if self.fits_expected(symbol, expected) =>
                {
                    Choice::One(symbol)
                }
                Binding::Dynamic(_) => Choice::Undecided,
                _ => Choice::None,
            };
        }
        let fitting = self.fitting_groups(value, expected);
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

    /// The overloads of `value` that a value of type `expected` can be,
    /// grouped by scope, nearest first, leaving out scopes with none.
    fn fitting_groups(
        &mut self,
        value: &FunctionValue,
        expected: &StaticType,
    ) -> Vec<Vec<SymbolId>> {
        value
            .groups
            .iter()
            .map(|group| {
                group
                    .iter()
                    .copied()
                    .filter(|&symbol| self.fits_expected(symbol, expected))
                    .collect::<Vec<_>>()
            })
            .filter(|group| !group.is_empty())
            .collect()
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
        match self.choose_function_value(value, &expected) {
            Choice::One(symbol) => {
                self.commit_function_value(value, symbol, expected.has_vars());
                true
            }
            Choice::Undecided if expected.has_vars() => false,
            Choice::Undecided => {
                let example = self.overload_example(value, &expected);
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

    /// The type of an overload that fits `expected`, nearest first, as an
    /// example of an annotation that picks one. A type parameter can't be
    /// written in an annotation, so it's shown as `Any`.
    fn overload_example(&mut self, value: &FunctionValue, expected: &StaticType) -> StaticType {
        let symbol = self
            .fitting_groups(value, expected)
            .first()
            .and_then(|group| group.first().copied())
            .unwrap_or_else(|| self.resolver.innermost_reference(value.id));
        self.vars
            .shown(self.types.get_type(symbol))
            .replace(&|part| matches!(part, StaticType::Generic(_)).then_some(StaticType::Any))
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
