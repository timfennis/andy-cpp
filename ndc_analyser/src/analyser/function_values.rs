//! A function's name used as a value, as `split` in `xs.map(split)`. When
//! the name has several overloads, the type expected where it's used picks
//! one: `map` passes one `String`, so `split(String)` is meant, not
//! `split(String, String)`.

use super::Analyser;
use super::solver::{CallSite, Constraint, EarlyOverload};
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
    /// Whether it's a value of a literal, as `abs` is in `[len, abs]`, whose
    /// other values say what's expected of it, but not which overload it is.
    in_literal: bool,
    span: Span,
}

impl FunctionValue {
    pub(super) fn expected(&self) -> &StaticType {
        &self.expected
    }

    /// This value with `expected` as the type expected of it, as a copy of
    /// a generic function's constraints gets the types of its call.
    pub(super) fn with_expected(&self, expected: StaticType) -> Self {
        Self {
            expected,
            ..self.clone()
        }
    }

    pub(super) fn id(&self) -> NodeId {
        self.id
    }

    pub(super) fn mark_in_literal(&mut self) {
        self.in_literal = true;
    }

    /// Every overload the name can mean, for the program to pick from as it
    /// calls the value.
    pub(super) fn every_overload(&self) -> Binding<SymbolId> {
        Binding::Dynamic(
            self.groups
                .iter()
                .flatten()
                .map(|&symbol| Candidate::Scalar(symbol))
                .collect(),
        )
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
                in_literal: false,
                span,
            };
            if !self.try_function_value(&value) {
                self.waiting_function_values.insert(id);
                self.wait(Constraint::FunctionValue(value));
            }
            return self.vars.resolve(&expected);
        }
        self.picks
            .bindings
            .insert(id, Binding::Resolved(Candidate::Scalar(symbol)));
        // A generic function used as a value, as in `let p = push`, has one
        // type there, which its uses settle.
        let typ = self.instantiate_function(symbol);
        self.flush_copies(None, span);
        typ
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
        self.vars.instantiate(&typ)
    }

    /// Which overload `value` means where a value of type `expected` is
    /// expected. Once that's a function type, the value is called with
    /// arguments of its parameter types, so it means the overload a call
    /// with those arguments means, decided by the same rules: while they
    /// aren't fully known, only an overload that's the only one able to take
    /// them, and not through an `Any` parameter, can be committed to; once
    /// they are, the nearest scope, the most specific overload and `Any`
    /// losing to an exact type pick it. The return type doesn't pick, as it
    /// doesn't for a call: the overload must return what's expected, or none
    /// fits. Before the expected type is a function type, an overload is
    /// only chosen when it's the only one there is to choose.
    pub(super) fn choose_function_value(
        &mut self,
        value: &FunctionValue,
        expected: &StaticType,
    ) -> Choice {
        if let StaticType::Function {
            parameters: Some(parameters),
            ..
        } = expected
        {
            let picked = if parameters.iter().any(StaticType::has_vars) {
                match self.early_overload(&value.candidates, parameters, CallKind::Regular) {
                    EarlyOverload::Only(only) => Some(only),
                    EarlyOverload::NoneFits => return Choice::None,
                    EarlyOverload::Undecided => return Choice::Undecided,
                }
            } else {
                match self
                    .overloads()
                    .resolve_call(&value.candidates, parameters, CallKind::Regular)
                    .binding
                {
                    Binding::Resolved(Candidate::Scalar(symbol)) => Some(symbol),
                    // The program would pick one as it runs, but a function
                    // value is one function. When the expected type's `Any`
                    // could be what's not known yet, as in a generic
                    // function's own body, more than one might be meant.
                    Binding::Dynamic(candidates) => {
                        let might = candidates.into_iter().any(|candidate| match candidate {
                            Candidate::Scalar(symbol) => self.might_be_expected(symbol, expected),
                            Candidate::Vec(_) => false,
                        });
                        return if might {
                            Choice::Undecided
                        } else {
                            Choice::None
                        };
                    }
                    _ => None,
                }
            };
            return match picked {
                Some(symbol) if self.fits_expected(symbol, expected) => Choice::One(symbol),
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

    /// Whether a value of `symbol`'s type might be passed where `expected`
    /// is, once what's not known about either is: an `Fn(Int) -> Int` might
    /// be the `Fn(Any) -> Any` a generic function's body expects, but an
    /// `Fn(Int, Int) -> Int` isn't.
    fn might_be_expected(&self, symbol: SymbolId, expected: &StaticType) -> bool {
        self.vars
            .resolve(self.types.get_type(symbol))
            .may_fit(&self.vars.resolve(expected))
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
            // The other values of a literal can't say which overload is
            // meant, as the literal could hold `Any`s: `[len, abs]`.
            Choice::None if value.in_literal => {
                let example = self.overload_example(value, &StaticType::Any);
                self.emit(AnalysisError::ambiguous_function_value(
                    &value.name,
                    &StaticType::Any,
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
            .or_else(|| {
                value
                    .groups
                    .iter()
                    .flatten()
                    .copied()
                    .find(|&symbol| self.might_be_expected(symbol, expected))
            })
            .unwrap_or_else(|| self.resolver.innermost_reference(value.id));
        self.vars
            .shown(self.types.get_type(symbol))
            .replace(&|part| matches!(part, StaticType::Generic(_)).then_some(StaticType::Any))
    }

    fn commit_function_value(&mut self, value: &FunctionValue, symbol: SymbolId, early: bool) {
        let typ = self.instantiate_function(symbol);
        self.vars.bind_argument(&value.expected, &typ);
        self.flush_copies(None, value.span);
        if early && cfg!(debug_assertions) {
            self.remember_early_function_value(value, symbol);
        }
        self.record_function_value(value, symbol);
    }

    /// Records that `value` means `symbol`, in the instance whose
    /// constraints are being solved, if any. A copy of a generic function's
    /// constraint records nothing, as the value is in that function's body,
    /// whose instance for the call records it.
    fn record_function_value(&mut self, value: &FunctionValue, symbol: SymbolId) {
        if self.copied_through.is_none() {
            self.pick_tables(self.recording).insert(
                CallSite::Call(value.id),
                Binding::Resolved(Candidate::Scalar(symbol)),
            );
        }
    }

    /// Picks the nearest overload after an error, so analysis carries on
    /// without reporting the same value again.
    fn give_up_on_function_value(&mut self, value: &FunctionValue) {
        let symbol = self.resolver.innermost_reference(value.id);
        let typ = self.instantiated_type(symbol);
        self.vars.try_unify(&value.expected, &typ);
        self.record_function_value(value, symbol);
    }
}
