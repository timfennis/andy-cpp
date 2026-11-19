//! Calls and other uses whose types aren't known yet wait here until they
//! are. In `xs.map(fn(x) => x + 1)`, the `+` waits for `x`'s type, which
//! picking `map` settles from the type of `xs`.

use super::Analyser;
use super::calls::is_known_before;
use super::function_values::{Choice, FunctionValue};
use super::generics::redefinable_parameters;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads, ResolvedCall, is_strictly_more_specific};
use crate::types::TypeVars;
use itertools::Itertools;
use ndc_analysis::{Binding, Candidate};
use ndc_core::collections::HashSet;
use ndc_core::{NodeId, StaticType, TypeVar};
use ndc_lexer::Span;
use ndc_resolver::{Candidates, SymbolId};
use std::borrow::Cow;
use std::collections::BTreeSet;
use std::rc::Rc;

/// What a call can commit to before its arguments are fully known.
pub(super) enum EarlyOverload {
    /// This overload, whatever the arguments turn out to be.
    Only(SymbolId),
    /// More than one could still be picked.
    Undecided,
    /// None can take the arguments.
    NoneFits,
}

/// Where a call's candidates come from and where its pick goes.
#[derive(Debug, Clone, Copy)]
pub(super) enum CallSite {
    /// A call or an operator: the overloads the callee's name can mean.
    Call(NodeId),
    /// `x.f`, or reading the old value in `xs[i] += 1`.
    Getter(NodeId),
    /// The write in `xs[i] = v`.
    Setter(NodeId),
    /// The write in `p.x = v`.
    FieldSetter(NodeId),
    /// The operator in `a op= b`, which is `a = op(a, b)`.
    OpAssignment(NodeId),
}

/// Something that waits until the types it depends on are known.
#[derive(Debug, Clone)]
pub(super) enum Constraint {
    /// A call whose overload can't be picked yet.
    Call(CallConstraint),
    /// An overloaded function's name used as a value, waiting for the type
    /// expected of it to pick an overload.
    FunctionValue(FunctionValue),
    /// `for x in s`: what iterating over `sequence` gives.
    Iterate {
        sequence: StaticType,
        element: StaticType,
        span: Span,
    },
    /// `let (a, b) = v`: what each name gets from `value`.
    Unpack {
        value: StaticType,
        parts: Vec<StaticType>,
        span: Span,
    },
    /// The values a map default makes: what it returns if it's a function
    /// that's called for each missing key, and the default itself if not.
    MapDefault {
        default: StaticType,
        value: StaticType,
        span: Span,
    },
    /// The one type the two branches of an `if` have, once neither is
    /// wholly unknown: the branches' types and then the result's. If an
    /// `Any` was expected of the `if` in a generic function's body before
    /// that, a copy of it for a call is told so by `expects_any`: see
    /// [`crate::types::TypeVars::mark_join`].
    Join {
        types: [StaticType; 3],
        expects_any: bool,
        span: Span,
    },
}

impl Constraint {
    /// The types whose variables the constraint waits on.
    pub(super) fn inputs(&self) -> &[StaticType] {
        match self {
            Self::Call(call) => &call.arguments,
            Self::FunctionValue(value) => std::slice::from_ref(value.expected()),
            Self::Iterate { sequence, .. } => std::slice::from_ref(sequence),
            Self::Unpack { value, .. } => std::slice::from_ref(value),
            Self::MapDefault { default, .. } => std::slice::from_ref(default),
            // A result known from outside says what each branch is.
            Self::Join { types, .. } => types,
        }
    }

    /// The types the constraint settles once it's solved, which are known
    /// once its inputs are.
    pub(super) fn outputs(&self) -> &[StaticType] {
        match self {
            Self::Call(call) => std::slice::from_ref(&call.result),
            Self::FunctionValue(_) => &[],
            Self::Iterate { element, .. } => std::slice::from_ref(element),
            Self::Unpack { parts, .. } => parts,
            Self::MapDefault { value, .. } => std::slice::from_ref(value),
            Self::Join { types, .. } => &types[2..],
        }
    }

    /// Every type the constraint waits on or settles: its inputs, then its
    /// outputs, then a call's type arguments, which are known when it's made.
    pub(super) fn types(&self) -> impl Iterator<Item = &StaticType> {
        let type_arguments = match self {
            Self::Call(call) => call.type_arguments.as_slice(),
            Self::FunctionValue(value) => value.type_arguments(),
            _ => &[],
        };
        self.inputs()
            .iter()
            .chain(self.outputs())
            .chain(type_arguments)
    }

    /// The constraint with `replace` applied to each of its types.
    pub(super) fn map_types(&self, replace: &mut impl FnMut(&StaticType) -> StaticType) -> Self {
        match self {
            Self::Call(call) => Self::Call(CallConstraint {
                type_arguments: call.type_arguments.iter().map(&mut *replace).collect(),
                arguments: call.arguments.iter().map(&mut *replace).collect(),
                result: replace(&call.result),
                ..call.clone()
            }),
            Self::FunctionValue(value) => Self::FunctionValue(value.map_types(replace)),
            Self::Iterate {
                sequence,
                element,
                span,
            } => Self::Iterate {
                sequence: replace(sequence),
                element: replace(element),
                span: *span,
            },
            Self::Unpack { value, parts, span } => Self::Unpack {
                value: replace(value),
                parts: parts.iter().map(&mut *replace).collect(),
                span: *span,
            },
            Self::MapDefault {
                default,
                value,
                span,
            } => Self::MapDefault {
                default: replace(default),
                value: replace(value),
                span: *span,
            },
            Self::Join {
                types,
                expects_any,
                span,
            } => Self::Join {
                types: types.clone().map(|typ| replace(&typ)),
                expects_any: *expects_any,
                span: *span,
            },
        }
    }
}

/// A call to a generic function, through which a constraint of the
/// function's body was copied to the code calling it.
#[derive(Debug, Clone)]
pub(super) struct Frame {
    /// The generic function called.
    pub(super) function: SymbolId,
    /// Its name, for errors.
    pub(super) name: String,
    /// The call.
    pub(super) call: Span,
}

/// The calls a constraint was copied through, outermost first: the call in
/// the code being analysed, then the call in that function's body that
/// copied it there, and so on. Its own span is in the last function's body.
pub(super) type Origin = Rc<[Frame]>;

/// A constraint that waits, and the calls it was copied through if it's a
/// copy of a generic function's constraint.
#[derive(Debug, Clone)]
pub(super) struct Waiting {
    pub(super) constraint: Constraint,
    pub(super) origin: Option<Origin>,
}

/// An overload a call committed to before its arguments were known, which
/// [`Analyser::check_early_picks`] checks once they are.
#[derive(Debug)]
pub(super) enum EarlyPick {
    Call {
        call: CallConstraint,
        picked: SymbolId,
    },
    FunctionValue {
        value: FunctionValue,
        picked: SymbolId,
    },
}

/// What [`Analyser::pick_overload`] picks.
enum Pick {
    /// The overload the call means.
    One(SymbolId),
    /// Overloads that take the call, none more specific than the others.
    Tied(Vec<SymbolId>),
    /// No overload takes the call.
    None,
}

/// What [`Analyser::type_literal_operands`] did with an operator's operands.
enum LiteralOperands {
    /// None is a number literal it types.
    None,
    /// It gave a literal the other operand's type: the call can be tried
    /// again.
    Typed,
    /// They're literals whose type isn't known yet: the call waits for it.
    Wait,
}

#[derive(Debug, Clone)]
pub(super) struct CallConstraint {
    site: CallSite,
    candidates: Candidates,
    /// The type arguments written after the callee's name: `Int` in
    /// `f::<Int>(x)`. Empty when none are written.
    type_arguments: Vec<StaticType>,
    arguments: Vec<StaticType>,
    /// The call's type, which the picked overload's return type settles.
    result: StaticType,
    kind: CallKind,
    name: String,
    span: Span,
    /// The index of `t[1]`, a number written in the code, which picks the
    /// element of a tuple `t` and its type.
    tuple_index: Option<i64>,
}

impl CallConstraint {
    pub(super) fn arguments(&self) -> &[StaticType] {
        &self.arguments
    }

    pub(super) fn type_arguments(&self) -> &[StaticType] {
        &self.type_arguments
    }

    pub(super) fn span(&self) -> Span {
        self.span
    }

    pub(super) fn candidates(&self) -> &Candidates {
        &self.candidates
    }

    pub(super) fn result(&self) -> &StaticType {
        &self.result
    }

    /// Whether `other` calls what this calls, the same way, with arguments
    /// of the same types, so it returns the same type.
    pub(super) fn is_same_call(&self, other: &Self, vars: &TypeVars) -> bool {
        std::mem::discriminant(&self.site) == std::mem::discriminant(&other.site)
            && self.kind == other.kind
            && self.candidates == other.candidates
            && self.tuple_index == other.tuple_index
            && self.arguments.len() == other.arguments.len()
            && self.type_arguments.len() == other.type_arguments.len()
            && self
                .arguments
                .iter()
                .chain(&self.type_arguments)
                .zip(other.arguments.iter().chain(&other.type_arguments))
                .all(|(a, b)| vars.resolve(a) == vars.resolve(b))
    }
}

impl Analyser {
    /// Runs `try_it`, keeping what it settled only if it returns `Some`: see
    /// [`TypeVars::attempt_in`].
    pub(super) fn attempt<T>(&mut self, try_it: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        TypeVars::attempt_in(self, |analyser| &mut analyser.vars, try_it)
    }

    /// Runs `look`, then takes back what it settled: see
    /// [`TypeVars::probe_in`].
    pub(super) fn probe<R>(&mut self, look: impl FnOnce(&mut Self) -> R) -> R {
        TypeVars::probe_in(self, |analyser| &mut analyser.vars, look)
    }

    /// Picks the overload a call means and returns the call's type. When the
    /// arguments aren't known well enough to pick one, the call waits, and
    /// its type is a variable that the pick settles later.
    pub(super) fn constrain_call(
        &mut self,
        site: CallSite,
        arguments: Vec<StaticType>,
        kind: CallKind,
        name: &str,
        span: Span,
    ) -> StaticType {
        self.constrain_index_call(site, Vec::new(), arguments, kind, name, span, None)
    }

    /// [`Self::constrain_call`] for a call with the type arguments
    /// `type_arguments`, as `f::<Int>(x)`, which only the overloads that take
    /// that many can take, with their type parameters bound to them.
    pub(super) fn constrain_explicit_call(
        &mut self,
        site: CallSite,
        type_arguments: Vec<StaticType>,
        arguments: Vec<StaticType>,
        name: &str,
        span: Span,
    ) -> StaticType {
        self.constrain_index_call(
            site,
            type_arguments,
            arguments,
            CallKind::Regular,
            name,
            span,
            None,
        )
    }

    /// [`Self::constrain_call`], for `[]` with the index `tuple_index` when
    /// it's a number written in the code, which picks a tuple's element: see
    /// [`Self::constrain_index_read`].
    #[allow(clippy::too_many_arguments)]
    pub(super) fn constrain_index_call(
        &mut self,
        site: CallSite,
        type_arguments: Vec<StaticType>,
        arguments: Vec<StaticType>,
        kind: CallKind,
        name: &str,
        span: Span,
        tuple_index: Option<i64>,
    ) -> StaticType {
        let mut call = CallConstraint {
            candidates: self.site_candidates(site),
            site,
            type_arguments,
            arguments,
            result: StaticType::Never,
            kind,
            name: name.to_string(),
            span,
            tuple_index,
        };
        if let Some(typ) = self.try_call(&call) {
            return typ;
        }
        call.result = self.vars.fresh();
        let result = call.result.clone();
        // Arithmetic on literals gives a literal of their type: `-1` and
        // `1 + 1` are Floats in `if c { 2.5 } else { -1 }`.
        if let Some(literal) = self.literal_arithmetic(&call) {
            self.vars.unify(&result, &literal);
        }
        self.wait(Constraint::Call(call));
        result
    }

    /// What iterating over a value of type `sequence` gives, which waits
    /// until the kind of sequence is known.
    pub(super) fn constrain_iteration(&mut self, sequence: StaticType, span: Span) -> StaticType {
        if self.vars.waits_to_iterate(&sequence) {
            let element = self.vars.fresh();
            self.wait(Constraint::Iterate {
                sequence,
                element: element.clone(),
                span,
            });
            return element;
        }
        self.iteration_gives(&sequence, span)
    }

    /// What iterating over a value of the known type `sequence` gives,
    /// reporting a value that doesn't iterate, like `true`.
    fn iteration_gives(&mut self, sequence: &StaticType, span: Span) -> StaticType {
        if self.vars.resolve(sequence) == StaticType::Any {
            let error = AnalysisError::not_iterable(&StaticType::Any, span);
            self.emit(error);
            return StaticType::Error;
        }
        if let Some(element) = self.vars.element_type(sequence) {
            return element;
        }
        let error = AnalysisError::not_iterable(&self.vars.shown(sequence), span);
        self.emit(error);
        StaticType::Error
    }

    /// The types of the `count` parts a value of type `value` unpacks into,
    /// as in `let a, b = value`: what a tuple holds at each place, what
    /// another sequence holds, or, for a value whose type isn't known yet,
    /// what it holds once it is. A value that doesn't unpack into `count`
    /// parts is reported, and its parts are failed checks.
    pub(super) fn unpack(
        &mut self,
        value: &StaticType,
        count: usize,
        span: Span,
    ) -> Vec<StaticType> {
        let value = self.vars.resolve(value);
        if matches!(value, StaticType::Var(_)) {
            return self.constrain_unpacking(value, count, span);
        }
        self.unpack_known(&value, count, span)
            .unwrap_or_else(|| vec![StaticType::Error; count])
    }

    /// [`Self::unpack`] for a value whose type is known, or `None`, reported,
    /// if it doesn't unpack into `count` parts.
    pub(super) fn unpack_known(
        &mut self,
        value: &StaticType,
        count: usize,
        span: Span,
    ) -> Option<Vec<StaticType>> {
        match value {
            StaticType::Tuple(elements) if elements.len() != count => {
                self.emit(AnalysisError::tuple_arity_mismatch(
                    count,
                    elements.len(),
                    span,
                ));
                None
            }
            value => {
                let parts = value
                    .unpack()
                    .map(|parts| parts.take(count).cloned().collect::<Vec<_>>());
                if parts.is_none() {
                    self.emit(AnalysisError::unable_to_unpack_type(value, span));
                }
                parts
            }
        }
    }

    /// The types `let (a, b, …) = value` gives each name when the kind of
    /// value isn't known yet. They're settled once it is.
    fn constrain_unpacking(
        &mut self,
        value: StaticType,
        count: usize,
        span: Span,
    ) -> Vec<StaticType> {
        let parts = (0..count).map(|_| self.vars.fresh()).collect::<Vec<_>>();
        self.wait(Constraint::Unpack {
            value,
            parts: parts.clone(),
            span,
        });
        parts
    }

    /// The one type that values of type `a` and `b` both have, as the two
    /// branches of an `if`: the type they are once made the same. `if c { 1
    /// } else { 2.5 }` is a Float, and `if c { [] } else { [1] }` a
    /// `[Int]`. Types that can't be made the same are held as a
    /// literal's values are, so `if c { 1 } else { "a" }` is an `Any` where
    /// one is expected, and an error where not.
    /// A branch nothing is known about yet waits, so a lambda's parameter
    /// isn't fixed by the branch next to it: in `fn(x) => if c { x } else
    /// { 0 }`, `x` can still be a `String`.
    pub(super) fn join(&mut self, a: &StaticType, b: &StaticType, span: Span) -> StaticType {
        let (a, b) = (self.vars.resolve(a), self.vars.resolve(b));
        match (&a, &b) {
            (StaticType::Never, _) => b,
            (_, StaticType::Never) => a,
            _ if a == b => a,
            _ if self.vars.is_unknown(&a) || self.vars.is_unknown(&b) => {
                let result = self.vars.fresh();
                self.wait(Constraint::Join {
                    types: [a, b, result.clone()],
                    expects_any: false,
                    span,
                });
                result
            }
            _ => self.join_known(&a, &b, span),
        }
    }

    /// [`Self::join`] of two types that aren't wholly unknown: the type
    /// they are once made the same, or a literal's type holding both.
    fn join_known(&mut self, a: &StaticType, b: &StaticType, span: Span) -> StaticType {
        if self.vars.attempt(|vars| vars.unify(a, b)) {
            return self.vars.resolve(a);
        }
        self.fresh_holding(vec![(a.clone(), span), (b.clone(), span)], span)
    }

    /// Commits `call` to `picked` while its arguments still have unknown
    /// parts, which settles them from `picked`'s parameters, and returns the
    /// call's type, or `None` when they don't fit after all.
    fn commit_early(
        &mut self,
        call: &CallConstraint,
        arguments: &[StaticType],
        picked: SymbolId,
    ) -> Option<StaticType> {
        let Some(typ) = self.commit_call(
            &Binding::Resolved(Candidate::Scalar(picked)),
            &call.type_arguments,
            arguments,
            StaticType::Any,
        ) else {
            self.pending.clear();
            return None;
        };
        let arguments = arguments
            .iter()
            .map(|argument| self.vars.resolve(argument))
            .collect::<Vec<_>>();
        if cfg!(debug_assertions) {
            self.early_picks.push(EarlyPick::Call {
                call: call.clone(),
                picked,
            });
        }
        self.record_pick(
            call.site,
            Binding::Resolved(Candidate::Scalar(picked)),
            &arguments,
        );
        self.flush_copies(Some(call.site), call.span);
        Some(typ)
    }

    /// Checks that each call that committed to an overload before its
    /// arguments were known got the overload it would get now that they
    /// are. Committing early is only right when nothing learned later could
    /// change the pick, which is easy to get wrong, so debug builds check
    /// every early commit the tests make.
    pub(super) fn check_early_picks(&mut self) {
        let early_picks = std::mem::take(&mut self.early_picks);
        for pick in early_picks {
            match pick {
                EarlyPick::Call { call, picked } => {
                    let picked = Binding::Resolved(Candidate::Scalar(picked));
                    let arguments = call
                        .arguments
                        .iter()
                        .map(|argument| self.vars.resolve(argument))
                        .collect::<Vec<_>>();
                    // A call passed a value whose check failed later picks
                    // nothing, and the program doesn't run.
                    if arguments.iter().any(StaticType::has_errors) {
                        continue;
                    }
                    let binding = self
                        .resolve_overloads(
                            &call.candidates,
                            &call.type_arguments,
                            &arguments,
                            call.kind,
                        )
                        .binding;
                    // A call that nothing takes any more is one an error
                    // was reported for, and a program with errors doesn't
                    // run.
                    if binding == Binding::None && !self.result.errors.is_empty() {
                        continue;
                    }
                    assert!(
                        binding == picked,
                        "internal error: `{}` committed to {picked:?} before its arguments \
                         were known, but with ({}) it means {binding:?}",
                        call.name,
                        arguments.iter().join(", "),
                    );
                }
                EarlyPick::FunctionValue { value, picked } => {
                    let expected = self.vars.resolve(value.expected());
                    if expected.has_errors() {
                        continue;
                    }
                    let choice = self.choose_function_value(&value, &expected);
                    // As for a call: a value nothing fits any more is one an
                    // error was reported for.
                    if choice == Choice::None && !self.result.errors.is_empty() {
                        continue;
                    }
                    assert!(
                        choice == Choice::One(picked),
                        "internal error: the function value {value:?} committed to {picked:?} \
                         before its type was known, but with {} it means {choice:?}",
                        self.vars.resolve(value.expected()),
                    );
                }
            }
        }
    }

    /// Remembers that `value` committed to `picked` before the type expected
    /// of it was known, for [`Self::check_early_picks`].
    pub(super) fn remember_early_function_value(
        &mut self,
        value: &FunctionValue,
        picked: SymbolId,
    ) {
        self.early_picks.push(EarlyPick::FunctionValue {
            value: value.clone(),
            picked,
        });
    }

    /// The values a map default of type `default` makes, which waits until
    /// it's known whether the default is a function: in `fn(f) => %{:f}`, it
    /// depends on what's passed for `f`.
    pub(super) fn constrain_default_value(
        &mut self,
        default: StaticType,
        span: Span,
    ) -> StaticType {
        match self.vars.resolve(&default) {
            // A number is never a function, so it's the map's values itself,
            // and takes the type their other uses need: in `%{:0}`, a write
            // of a String is the mismatch, not the `0`.
            StaticType::Var(var) if self.vars.is_number(var) => default,
            StaticType::Var(_) => {
                let value = self.vars.fresh();
                self.wait(Constraint::MapDefault {
                    default,
                    value: value.clone(),
                    span,
                });
                value
            }
            known => Self::default_value_type(known),
        }
    }

    /// Marks the result of `constraint`, if it's a join, as the type of an
    /// `if` whose branches aren't known yet, of which an `Any` is expected if
    /// the constraint says so.
    fn mark_join(&mut self, constraint: &Constraint) {
        if let Constraint::Join {
            types: [_, _, result],
            expects_any,
            ..
        } = constraint
        {
            self.vars.mark_join(result);
            if *expects_any {
                self.vars.bind_argument(&StaticType::Any, result);
            }
        }
    }

    /// Makes `constraint` wait, in the order it started waiting, until one
    /// of the variables it waits on is settled.
    pub(super) fn wait(&mut self, constraint: Constraint) {
        let origin = self.copied_through.clone();
        self.wait_as(Waiting { constraint, origin });
    }

    /// Makes `waiting` wait, as [`Self::wait`].
    pub(super) fn wait_as(&mut self, waiting: Waiting) {
        self.mark_join(&waiting.constraint);
        let serial = self.next_serial;
        self.next_serial += 1;
        self.wait_again(serial, waiting);
    }

    fn wait_again(&mut self, serial: u64, waiting: Waiting) {
        for var in self.waits_on(&waiting.constraint) {
            self.watchers.entry(var).or_default().push(serial);
        }
        self.waiting.insert(serial, waiting);
    }

    /// The unsettled variables `constraint` waits on: those of its inputs,
    /// and for a call, those of the variables it may call, as `f` in `for f
    /// in [fn() => 1] { f() }` holds what the literal holds.
    fn waits_on(&self, constraint: &Constraint) -> Vec<TypeVar> {
        let mut vars = constraint
            .inputs()
            .iter()
            .flat_map(|input| self.vars.unsettled(input))
            .collect::<Vec<_>>();
        if let Constraint::Call(call) = constraint {
            for symbol in self.overloads().reachable_symbols(&call.candidates) {
                if let StaticType::Var(var) = self.vars.resolve(self.types.get_type(symbol)) {
                    vars.push(var);
                }
            }
        }
        vars
    }

    /// The unsettled variables the waiting constraints settle once they're
    /// solved, which no fallback settles: what a call returns is what the
    /// overload it picks returns, once what it waits on is known.
    fn produced_by_waiting(&self) -> HashSet<TypeVar> {
        self.waiting
            .values()
            .flat_map(|waiting| {
                waiting
                    .constraint
                    .outputs()
                    .iter()
                    .flat_map(|output| self.vars.unsettled(output))
                    .chain(self.returned_by(&waiting.constraint))
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    /// Solves `waiting` if what it waits on is known, as
    /// [`Self::try_constraint`], and returns whether it did. A copy of a
    /// generic function's constraint reports its errors at the call it was
    /// copied through, and records no pick: the function's body was
    /// compiled with its own.
    pub(super) fn try_waiting(&mut self, waiting: &mut Waiting) -> bool {
        let outer = std::mem::replace(&mut self.copied_through, waiting.origin.clone());
        let solved = self.try_constraint(&mut waiting.constraint);
        self.copied_through = outer;
        solved
    }

    /// Tries again each waiting constraint that waits on a variable settled
    /// since this last ran, oldest first, until that settles nothing more.
    pub(super) fn solve_waiting(&mut self) {
        self.solve_waiting_and_report();
    }

    /// [`Self::solve_waiting`], returning the constraints it tried that
    /// still wait.
    fn solve_waiting_and_report(&mut self) -> BTreeSet<u64> {
        self.report_unshared();
        let mut still_waiting = BTreeSet::new();
        loop {
            let settled = self.vars.settled_since(self.watched_settlings);
            if settled.is_empty() {
                break;
            }
            let woken = settled
                .iter()
                .filter_map(|var| self.watchers.remove(var))
                .flatten()
                .collect::<BTreeSet<_>>();
            self.watched_settlings = self.vars.settled_count();
            for serial in woken {
                // A constraint woken by an earlier variable may be solved already.
                let Some(mut waiting) = self.waiting.remove(&serial) else {
                    continue;
                };
                if self.try_waiting(&mut waiting) {
                    still_waiting.remove(&serial);
                } else {
                    self.wait_again(serial, waiting);
                    still_waiting.insert(serial);
                }
            }
        }
        self.watched_settlings = self.vars.settled_count();
        still_waiting
    }

    /// Solves what waits on the variables made since `first`, which nothing
    /// outside the code they were made in can settle. Literals take their
    /// defaults first. A branch of an `if` that's still unknown is the other
    /// branch's type. Then what empty literals hold takes its default, all
    /// at once: each default depends only on the value it's for, so the
    /// order the calls are solved in can't change it. What's left gets its
    /// fallback, oldest constraint first, but for what a waiting constraint
    /// settles once it's solved. Constraints that wait only on older
    /// variables keep waiting.
    pub(super) fn settle_waiting(&mut self, first: usize) {
        self.settle_waiting_keeping(first, &HashSet::default());
    }

    /// [`Self::settle_waiting`], but for the variables in `open`, which
    /// nothing waits on and later code may still settle: see
    /// [`Self::not_needed_now`].
    pub(super) fn settle_waiting_keeping(&mut self, first: usize, open: &HashSet<TypeVar>) {
        // Each constraint is checked for a default rule once, and again only
        // after something it waits on changed.
        let mut unchecked = self.waiting.keys().copied().collect::<BTreeSet<_>>();
        unchecked.extend(self.solve_waiting_and_report());
        // Fallbacks only ever make more known, so a constraint found to wait
        // on no variable made since `first` never needs them.
        let mut fallen_back = 0;
        loop {
            // A literal takes its default before anything else's is applied,
            // as a call its type would settle may be waiting for it.
            if self.default_literals(|_, var| var.0 as usize >= first) {
                unchecked.extend(self.waiting.keys().copied());
            }
            // A branch's default may settle what a call returns, as the
            // `+` in `fib(n - 1) + fib(n - 2)` returns what `fib` does.
            while let Some(serial) = unchecked.pop_first() {
                let vars = self.waits_on_vars_since(serial, first, &HashSet::default());
                if !vars.is_empty() && self.default_to_branch(serial, &vars) {
                    unchecked.extend(self.solve_waiting_and_report());
                }
            }
            let contents = self
                .vars
                .unsettled_contents(first)
                .into_iter()
                .filter(|var| !open.contains(var))
                .collect::<Vec<_>>();
            if !contents.is_empty() {
                self.vars.settle_contents(&contents);
                unchecked.extend(self.solve_waiting_and_report());
                continue;
            }
            let produced = self.produced_by_waiting();
            let next = self
                .waiting
                .range(fallen_back..)
                .map(|(&serial, _)| serial)
                .find_map(|serial| {
                    let vars = self.waits_on_vars_since(serial, first, &produced);
                    (!vars.is_empty()).then_some((serial, vars))
                });
            let Some((serial, vars)) = next else {
                break;
            };
            fallen_back = serial;
            self.vars.settle_with_fallbacks(&vars);
            unchecked.extend(self.solve_waiting_and_report());
        }
        // What's left gets its fallback, but for what a waiting constraint
        // settles, which only gets one if that constraint can't be solved.
        let mut produced = self.produced_by_waiting();
        produced.extend(open);
        self.vars.settle_from(first, &produced);
        self.solve_waiting();
        self.vars.settle_from(first, open);
        self.solve_waiting();
    }

    /// The unsettled variables that only what values hold mentions: not a
    /// literal's, which is what the line writes; not what a waiting
    /// constraint or a generic function's use waits on, or what those hold;
    /// and not what the type of a function the batch makes mentions, which
    /// is compiled into the function and checked against it as the program
    /// runs, as a cast to a function type does, and which a `pure fn`'s
    /// parameters are checked by. What an empty `[]`, `%{}` or `None` holds
    /// is one; a lambda's parameter isn't, even if nothing has used it yet.
    pub(super) fn not_needed_now(&self) -> HashSet<TypeVar> {
        let needed_types = self
            .waiting
            .values()
            .flat_map(|waiting| waiting.constraint.types().cloned())
            .chain(
                self.uses
                    .iter()
                    .flat_map(|generic_use| generic_use.key.clone()),
            )
            .chain(self.result.analysis.function_types.values().cloned())
            .collect::<Vec<_>>();
        let needed = self.vars.vars_made_from(&needed_types);
        (0..self.vars.count())
            .map(|index| TypeVar(u32::try_from(index).expect("fewer than 2^32 type variables")))
            .filter(|&var| {
                self.vars.resolve(&StaticType::Var(var)) == StaticType::Var(var)
                    && !self.vars.is_literal(var)
                    && !needed.contains(&var)
            })
            .collect()
    }

    /// Gives the number literals in `typ` their defaults, as what a variable
    /// is given: in `let n = 7`, `n` is an Int, whatever its uses are, so
    /// `n * 0.5` is a Float and `0..n` a range of Ints in any order. A literal
    /// list of numbers takes the type they share, as in `let xs = [1, 2.5]`
    /// and `let grid = [[1], [2.5]]`.
    /// Within one expression a literal still takes its operand's type, as
    /// the `2` in `x / 2` with a Float `x` does.
    pub(super) fn settle_number_literals(&mut self, typ: &StaticType) {
        // A literal whose values share no type, as `if c { 1 } else { "a" }`,
        // stays one, as what it is depends on where it's stored; the number
        // literals it holds don't.
        // What a literal that's settled holds is settled with it, as the
        // `[2.5]` in `[[1], [2.5]]`.
        let trees = self
            .vars
            .literals_within(typ)
            .into_iter()
            .map(|var| (var, self.vars.literals_within(&StaticType::Var(var))))
            .collect::<Vec<_>>();
        let mut literals = HashSet::default();
        for (var, tree) in trees {
            if tree.iter().any(|&held| self.vars.is_number(held))
                && (self.vars.is_number(var) || !self.vars.holds_unshared_values(var))
            {
                literals.extend(tree);
            }
        }
        if !literals.is_empty() {
            self.default_literals(|_, var| literals.contains(&var));
            self.solve_waiting();
        }
    }

    /// Settles the types of the literals that `belongs` says to that nothing
    /// settled, and returns whether it settled any. Each takes its first
    /// default, which depends only on the literal, never on the calls
    /// waiting on it: `1` is an Int, and `[1, 2.5]` holds Floats.
    pub(super) fn default_literals(
        &mut self,
        belongs: impl Fn(&TypeVars, TypeVar) -> bool,
    ) -> bool {
        let mut settled_any = false;
        loop {
            let mut progress = false;
            let holders = self.vars.holders();
            for var in self.vars.unsettled_literals(&belongs) {
                if !self.vars.is_literal(var) {
                    continue;
                }
                let Some(defaults) = self.vars.literal_defaults(var) else {
                    continue;
                };
                let default = &defaults[0];
                let holders_of_var = holders
                    .get(&var)
                    .into_iter()
                    .flatten()
                    .copied()
                    .filter(|&holder| self.vars.is_literal(holder))
                    .collect::<Vec<_>>();
                // Values that share no type are an error, which its fellows
                // aren't part of.
                let settled = self.attempt(|this| {
                    if *default == StaticType::Error {
                        this.vars.settle_default(var, default);
                        Some(())
                    } else {
                        this.settle_with_fellows(var, default, &holders_of_var)
                            .map(|_| ())
                    }
                });
                if settled.is_none() {
                    if self.vars.holds_several_literals(var) {
                        self.report_literals_that_differ(var, default);
                    } else {
                        self.vars.settle_with_fallbacks(&[var]);
                    }
                }
                self.solve_waiting();
                progress = true;
                settled_any = true;
            }
            if !progress {
                // What's left holds literals that aren't these to settle,
                // like `n` in `let n = 1; fn f() { [n] }`, and is what they
                // are: a list of whatever `n` is.
                let left = self.vars.unsettled_literals(&belongs);
                if left.is_empty() {
                    return settled_any;
                }
                for var in left {
                    match self.vars.held_literal(var) {
                        Some(held) if self.vars.is_literal(var) => {
                            self.vars.settle_literal(var, &StaticType::Var(held));
                        }
                        // A literal that holds only values of its own type,
                        // as `[x]` does once a parameter `x` is its type, is
                        // what they are, which isn't known yet: settling it
                        // as `Any` would make `x` one.
                        _ if self.vars.settle_literal_as_its_values(var) => {}
                        _ => self.vars.settle_with_fallbacks(&[var]),
                    }
                }
                self.solve_waiting();
                settled_any = true;
            }
        }
    }

    /// Reports the values of the literals whose element type is `var` that
    /// don't fit `default`, and settles it as `default`: after `let v =
    /// ["s"]; v = [1]`, `1` isn't a String.
    fn report_literals_that_differ(&mut self, var: TypeVar, default: &StaticType) {
        for (held, span) in self.vars.held(var) {
            let fits = self.vars.probe(|vars| vars.bind_argument(default, &held));
            if !fits {
                let error = AnalysisError::mismatched_types(&self.vars.shown(&held), default, span);
                self.emit(error);
            }
        }
        self.vars.settle_literal(var, default);
    }

    /// Settles the literal `var` as `default`, and the other literals that
    /// `holders` hold too where they can take it, as the values of `[1, 2]`
    /// have one type. Returns the literals it settled, or `None` if `var`
    /// can't be `default`.
    fn settle_with_fellows(
        &mut self,
        var: TypeVar,
        default: &StaticType,
        holders: &[TypeVar],
    ) -> Option<Vec<TypeVar>> {
        if !self.vars.settle_literal(var, default) {
            return None;
        }
        let mut settled = vec![var];
        for &holder in holders {
            for other in self.vars.held_literals(holder) {
                if self
                    .vars
                    .attempt(|vars| vars.settle_literal(other, default))
                {
                    settled.push(other);
                }
            }
        }
        Some(settled)
    }

    /// The unsettled variables made since `first` that the waiting
    /// constraint `serial` waits on, if it's still waiting, but for those a
    /// waiting constraint settles: see [`Self::produced_by_waiting`].
    fn waits_on_vars_since(
        &self,
        serial: u64,
        first: usize,
        produced: &HashSet<TypeVar>,
    ) -> Vec<TypeVar> {
        self.waiting.get(&serial).map_or_else(Vec::new, |waiting| {
            self.waits_on(&waiting.constraint)
                .into_iter()
                .filter(|var| var.0 as usize >= first && !produced.contains(var))
                .collect()
        })
    }

    /// The unsettled variables of what an overloaded function's name used
    /// as a value returns, which the overload its parameters pick says: in
    /// `let f = pick; let g = fn(x) => f(x);`, what `f` returns is what the
    /// `pick` that takes what `x` falls back to returns.
    fn returned_by(&self, constraint: &Constraint) -> Vec<TypeVar> {
        match constraint {
            Constraint::FunctionValue(value) => match self.vars.resolve(value.expected()) {
                StaticType::Function {
                    parameters: Some(_),
                    return_type,
                } => self.vars.unsettled(&return_type),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    /// Solves `constraint` if what it waits on is known, and returns whether
    /// it did.
    fn try_constraint(&mut self, constraint: &mut Constraint) -> bool {
        match constraint {
            Constraint::Call(call) => {
                let Some(typ) = self.try_call(call) else {
                    return false;
                };
                let result = call.result.clone();
                match call.site {
                    // What `p.x = v` writes was settled by the value before
                    // the read said what the field holds, so it's the value
                    // that doesn't fit. The write fails the same way and
                    // isn't reported again.
                    CallSite::Getter(id) if !self.accepts_write(&result, &typ) => {
                        self.emit_rejected_write(&typ, &result, call.span, false);
                        self.failed_reads.insert(id);
                    }
                    CallSite::Getter(_) => {}
                    _ => {
                        self.check_write(&result, &typ, call.span);
                    }
                }
                true
            }
            Constraint::FunctionValue(value) => self.try_function_value(value),
            Constraint::Iterate {
                sequence,
                element,
                span,
            } => {
                let sequence = self.vars.resolve(sequence);
                if self.vars.waits_to_iterate(&sequence) {
                    return false;
                }
                let (element, span) = (element.clone(), *span);
                let found = self.iteration_gives(&sequence, span);
                self.check_write(&element, &found, span);
                true
            }
            Constraint::Unpack { value, parts, span } => {
                let value = self.vars.resolve(value);
                if matches!(value, StaticType::Var(_)) {
                    return false;
                }
                let (parts, span) = (parts.clone(), *span);
                let found = self
                    .unpack_known(&value, parts.len(), span)
                    .unwrap_or_else(|| vec![StaticType::Error; parts.len()]);
                for (part, found) in parts.iter().zip(found) {
                    self.check_write(part, &found, span);
                }
                true
            }
            Constraint::MapDefault {
                default,
                value,
                span,
            } => {
                let default = self.vars.resolve(default);
                if matches!(default, StaticType::Var(_)) {
                    return false;
                }
                let value = value.clone();
                self.check_write(&value, &Self::default_value_type(default), *span);
                true
            }
            Constraint::Join {
                types: [a, b, result],
                span,
                ..
            } => {
                let (a, b, result) = (a.clone(), b.clone(), result.clone());
                // A result known from outside is what's expected of each
                // branch: in `let f: fn(Int) -> Int = if c { abs } else
                // { floor }`, it says which `abs` and `floor` are meant.
                if !matches!(self.vars.resolve(&result), StaticType::Var(_)) {
                    for side in [&a, &b] {
                        if self.vars.resolve(side).has_vars() {
                            self.vars.attempt(|vars| vars.bind_argument(&result, side));
                        }
                    }
                }
                let (a, b) = (self.vars.resolve(&a), self.vars.resolve(&b));
                // Branches that became one type are that type, known or not,
                // as in [`Self::join`].
                if a == b {
                    self.check_write(&result, &a, *span);
                    return true;
                }
                if self.vars.is_unknown(&a) || self.vars.is_unknown(&b) {
                    return false;
                }
                let joined = match (&a, &b) {
                    (StaticType::Never, other) | (other, StaticType::Never) => other.clone(),
                    _ => self.join_known(&a, &b, *span),
                };
                // Branches that share no type are an `Any` where one was
                // expected of the `if` while they weren't known.
                if self.vars.any_is_expected_of(&result) {
                    self.vars.bind_argument(&StaticType::Any, &joined);
                }
                self.check_write(&result, &joined, *span);
                true
            }
        }
    }

    /// Picks the native that indexes a tuple or an `Any` for `call`, if it's
    /// a read of `[]` and there is one, and returns whether it did: see
    /// [`ndc_core::ANY_INDEX`].
    fn index_any(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> bool {
        let Some(any_index) = self.any_index else {
            return false;
        };
        if call.name != "[]" || arguments.len() != 2 {
            return false;
        }
        self.record_pick(
            call.site,
            Binding::Resolved(Candidate::Scalar(any_index)),
            arguments,
        );
        true
    }

    /// Picks the overload `call` means, records it, and returns the call's
    /// type, or `None` when the arguments aren't known well enough yet. A
    /// call whose arguments are known is resolved the usual way. Otherwise
    /// the candidates that can't accept what's known are crossed off, and
    /// when one is left, the call commits to it, which settles the arguments
    /// from its parameters.
    fn try_call(&mut self, call: &CallConstraint) -> Option<StaticType> {
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.vars.resolve(argument))
            .collect::<Vec<_>>();
        if arguments.iter().any(StaticType::has_errors) {
            return Some(self.skip_failed_call(call, &arguments));
        }
        match self.type_literal_operands(call, &arguments) {
            LiteralOperands::Typed => return self.try_call(call),
            LiteralOperands::Wait => return None,
            LiteralOperands::None => {}
        }
        if self.type_literals_by_type_arguments(call, &arguments) {
            return self.try_call(call);
        }
        // A tuple indexed by a number written in the code gives that
        // element, of its own type: `(1, "a")[1]` is a String, and
        // `(1, "a")[-2]` an Int.
        if let (Some(index), Some(StaticType::Tuple(elements))) =
            (call.tuple_index, arguments.first())
        {
            let length = i64::try_from(elements.len()).expect("a tuple's length fits an i64");
            let position = if index < 0 { length + index } else { index };
            let element = usize::try_from(position)
                .ok()
                .and_then(|position| elements.get(position))
                .cloned();
            let indexed = vec![arguments[0].clone(), StaticType::Int];
            if let [_, index_type] = &arguments[..] {
                self.vars.bind_argument(&StaticType::Int, index_type);
            }
            if !self.index_any(call, &indexed) {
                self.resolve_call_now(call, &indexed);
            }
            let Some(element) = element else {
                self.emit(AnalysisError::tuple_index_out_of_bounds(
                    index,
                    elements.len(),
                    call.span,
                ));
                return Some(StaticType::Error);
            };
            return Some(element);
        }

        // Any other index of a tuple, and any of an `Any`, is the program's
        // to check as it runs.
        if matches!(
            arguments.first(),
            Some(StaticType::Tuple(_) | StaticType::Any)
        ) && self.index_any(call, &arguments)
        {
            return Some(StaticType::Any);
        }

        let reachable = self.overloads().reachable_symbols(&call.candidates);

        // A function's body can only pass its own type parameters on to
        // itself: `f::<Int>` in `fn f<T>` would need another version of `f`
        // while `f` is still being worked out.
        if let Some(function) = self.passes_other_type_arguments(&reachable, &call.type_arguments) {
            self.emit(self.recursive_type_arguments(function, call.span));
            self.record_pick(
                call.site,
                Binding::Resolved(Candidate::Scalar(function)),
                &call.arguments,
            );
            return Some(StaticType::Error);
        }

        // A generic function that calls itself with an argument made from
        // the parameter it's passed for, as `nest([x], n - 1)` in `fn
        // nest(x, n)`, would need a version of itself for `x`'s type, one
        // for lists of it, one for lists of those, without end.
        // Another overload may take the call, as `f(xs: [Any])` does
        // `f([x])`, so it's only this function's when no other can.
        if let Some(function) = self.declaring
            && self.copied_through.is_none()
            && self.recording.is_none()
            && reachable.contains(&function)
            && reachable.iter().all(|&symbol| {
                symbol == function || !self.could_accept(symbol, &call.type_arguments, &arguments)
            })
            && let StaticType::Function {
                parameters: Some(parameters),
                ..
            } = self.vars.resolve(self.types.get_type(function))
            && parameters.len() == arguments.len()
            && parameters
                .iter()
                .zip(&arguments)
                .any(|(parameter, argument)| {
                    matches!(parameter, StaticType::Var(var)
                    if *argument != *parameter && self.vars.is_made_from(argument, *var))
                })
        {
            let name = self.resolver.symbol_name(function).to_string();
            self.emit(AnalysisError::recursion_changes_types(&name, call.span));
            self.record_pick(
                call.site,
                Binding::Resolved(Candidate::Scalar(function)),
                &call.arguments,
            );
            return Some(StaticType::Error);
        }

        if let [only] = reachable[..] {
            match self.vars.resolve(self.types.get_type(only)) {
                // A variable whose type isn't known yet, like the parameter
                // `f` in `fn(f) => f(1)`, holds a function that takes these
                // arguments.
                callee @ StaticType::Var(var) if !self.vars.is_literal(var) => {
                    let typ = self.call_unknown_function(&callee, arguments);
                    self.record_pick(
                        call.site,
                        Binding::Resolved(Candidate::Scalar(only)),
                        &call.arguments,
                    );
                    return Some(typ);
                }
                // A variable whose value's check failed, like `f` in `let f
                // = missing(); f(1)`, was reported there.
                StaticType::Error => return Some(self.skip_failed_call(call, &arguments)),
                _ => {}
            }
        }

        // A variable holding a literal, like `n` in `let n = 1; n(2)`, can't
        // be called, which is said best once it's settled.
        if reachable.iter().any(|&symbol| {
            matches!(
                self.vars.resolve(self.types.get_type(symbol)),
                StaticType::Var(var) if self.vars.is_literal(var)
            )
        }) {
            return None;
        }

        // A write waits for the value it stores, which is analysed after the
        // target: only then can it tell whether the value is what's wrong.
        if let (CallSite::Setter(_) | CallSite::FieldSetter(_), Some(StaticType::Var(var))) =
            (call.site, arguments.last())
            && !self.vars.is_literal(*var)
        {
            return None;
        }

        if arguments.iter().all(|argument| !argument.has_vars()) {
            return Some(self.resolve_call_now(call, &arguments));
        }

        // A literal whose values share no type can only be an `Any`, which
        // is fine where the overload it picks as one asks for that: in
        // `f([1, "a"])` with `f(xs: [Any])`.
        if let Some(as_any) = self.unshared_literals_as_any(&arguments)
            && let ResolvedCall {
                binding: Binding::Resolved(Candidate::Scalar(picked)),
                ..
            } =
                self.resolve_overloads(&call.candidates, &call.type_arguments, &as_any, call.kind)
        {
            // An overload picked through a type parameter that the `Any`
            // fits, as `push([T], T)` with a Float for `[1, "a"]`, may
            // not take the literal itself, which then isn't an `Any`.
            if let Some(typ) = self.attempt(|this| this.commit_early(call, &arguments, picked)) {
                return Some(typ);
            }
        }

        match self.early_overload(
            &call.candidates,
            &call.type_arguments,
            &arguments,
            call.kind,
        ) {
            // A write that nothing takes may still fit once its target's
            // type is known, which also says better what's wrong.
            EarlyOverload::NoneFits
                if matches!(call.site, CallSite::Setter(_) | CallSite::FieldSetter(_)) =>
            {
                None
            }
            // A literal takes the type its uses need, or else its default,
            // which says better what's wrong: `f(1)` with `f(String)` gets
            // an Int. On tuples, an operator may apply to each element.
            EarlyOverload::NoneFits
                if arguments
                    .iter()
                    .any(|argument| self.vars.has_unsettled_literals(argument)) =>
            {
                None
            }
            // An operator applies to each element of a tuple, which waits
            // for the operands and elements nothing is known about yet: in
            // `(xs[0], 1) - (1, 1)`, the first element is whatever `xs`
            // holds, and in `p - (1, 1)`, `p` may be a tuple of anything. An
            // element partly known, as `[]` is a list, is settled by the
            // overload each position picks.
            EarlyOverload::NoneFits
                if call.kind == CallKind::Operator
                    && arguments.iter().any(|argument| match argument {
                        StaticType::Var(_) => true,
                        StaticType::Tuple(elements) => elements
                            .iter()
                            .any(|element| matches!(element, StaticType::Var(_))),
                        _ => false,
                    }) =>
            {
                None
            }
            EarlyOverload::NoneFits => Some(self.resolve_call_now(call, &arguments)),
            EarlyOverload::Only(only) => {
                let Some(typ) = self.commit_early(call, &arguments, only) else {
                    self.report_failed_call(call, &arguments);
                    return Some(StaticType::Error);
                };
                Some(typ)
            }
            EarlyOverload::Undecided => None,
        }
    }

    /// Settles what isn't known yet of each argument of a call with type
    /// arguments as its parameter says in every overload that can take the
    /// call, once the type arguments are bound, as a literal operand is
    /// typed by the other operand: in `len::<Float>([1])`, the `[1]` is a
    /// `[Float]`, which its default would otherwise make a `[Int]` that no
    /// `len::<Float>` takes, and in `len::<Float>([])` the `[]` is one too.
    /// Whichever of them the call picks settles it so. Returns whether it
    /// settled something.
    fn type_literals_by_type_arguments(
        &mut self,
        call: &CallConstraint,
        arguments: &[StaticType],
    ) -> bool {
        if call.type_arguments.is_empty() || !arguments.iter().any(StaticType::has_vars) {
            return false;
        }
        let viable = self
            .overloads()
            .reachable_symbols(&call.candidates)
            .into_iter()
            .filter(|&symbol| self.could_accept(symbol, &call.type_arguments, arguments))
            .collect::<Vec<_>>();
        let mut agreed: Option<Vec<Option<StaticType>>> = None;
        for symbol in viable {
            let Some(mut made) = self.type_argument_bindings(symbol, &call.type_arguments) else {
                continue;
            };
            let function = self.vars.resolve(self.types.get_type(symbol));
            let first = self.vars.count();
            let parameters = self.vars.probe(|vars| {
                vars.instantiate_parameters(&function, &mut made)
                    .map(|parameters| {
                        parameters
                            .iter()
                            .map(|parameter| {
                                Some(vars.resolve(parameter))
                                    .filter(|parameter| is_known_before(parameter, first))
                            })
                            .collect::<Vec<_>>()
                    })
            });
            let Some(parameters) = parameters else {
                return false;
            };
            agreed = Some(match agreed {
                None => parameters,
                Some(agreed) => agreed
                    .into_iter()
                    .zip(parameters)
                    .map(|(agreed, parameter)| {
                        agreed.filter(|agreed| Some(agreed) == parameter.as_ref())
                    })
                    .collect(),
            });
        }
        let mut typed = false;
        for (parameter, argument) in agreed.into_iter().flatten().zip(arguments) {
            let unknowns = self.vars.unsettled(argument).len();
            if let Some(parameter) = parameter
                && unknowns > 0
            {
                // Only what settles part of the argument counts, so the call
                // isn't tried again for nothing.
                if self.vars.attempt(|vars| {
                    vars.bind_argument(&parameter, argument)
                        && vars.unsettled(argument).len() < unknowns
                }) {
                    typed = true;
                }
            }
        }
        typed
    }

    /// Types the number literals among an operator's operands the way the
    /// operator's other operand says, without trying overloads, as numbers
    /// are in Haskell: in `total + 2.5`, with `total` from `let total = 0`,
    /// the `0` is a Float, and the two literals of `1 + 2` have one type.
    /// It looks only at the operator's own operands, so the order calls are
    /// solved in doesn't change it.
    fn type_literal_operands(
        &mut self,
        call: &CallConstraint,
        arguments: &[StaticType],
    ) -> LiteralOperands {
        if !Self::is_number_operator(call) {
            return LiteralOperands::None;
        }
        let literal = |vars: &TypeVars, typ: &StaticType| matches!(typ, StaticType::Var(var) if vars.is_number_literal(*var));
        let number = |typ: &StaticType| {
            matches!(
                typ,
                StaticType::Int | StaticType::Float | StaticType::Number
            )
        };
        match arguments {
            [a, b] => {
                let (a_literal, b_literal) = (literal(&self.vars, a), literal(&self.vars, b));
                let known = match (a_literal, b_literal) {
                    (true, false) if number(b) => Some(b),
                    (false, true) if number(a) => Some(a),
                    _ => None,
                };
                // Only where the operator takes two of that type, as `+`
                // takes two Floats, is that the literal's type.
                if let Some(known) = known
                    && self
                        .overloads()
                        .reachable_symbols(&call.candidates)
                        .into_iter()
                        .any(|symbol| {
                            self.overloads()
                                .parameters_of(symbol)
                                .is_some_and(|parameters| {
                                    parameters == [known.clone(), known.clone()]
                                })
                        })
                {
                    return if self.vars.try_unify(a, b) {
                        LiteralOperands::Typed
                    } else {
                        LiteralOperands::None
                    };
                }
                if a_literal && b_literal {
                    self.vars.try_unify(a, b);
                    return LiteralOperands::Wait;
                }
                LiteralOperands::None
            }
            [a] if literal(&self.vars, a) => LiteralOperands::Wait,
            _ => LiteralOperands::None,
        }
    }

    /// The literal type that `call`, arithmetic on number literals only, as
    /// `-1` or `1 + 2` is, gives.
    fn literal_arithmetic(&self, call: &CallConstraint) -> Option<StaticType> {
        if !Self::is_number_operator(call)
            || matches!(&*call.name, "==" | "!=" | "<" | ">" | "<=" | ">=")
        {
            return None;
        }
        let operands = call
            .arguments
            .iter()
            .map(|argument| self.vars.resolve(argument))
            .collect::<Vec<_>>();
        operands
            .iter()
            .all(|operand| matches!(operand, StaticType::Var(var) if self.vars.is_number_literal(*var)))
            .then(|| operands.first().cloned())
            .flatten()
    }

    /// Whether `call` is an arithmetic operator or a comparison, whose
    /// operands are numbers of one type.
    fn is_number_operator(call: &CallConstraint) -> bool {
        matches!(call.kind, CallKind::Operator | CallKind::Comparison)
            && match call.arguments.len() {
                1 => call.name == "-",
                2 => matches!(
                    &*call.name,
                    "+" | "-"
                        | "*"
                        | "/"
                        | "\\"
                        | "%"
                        | "%%"
                        | "^"
                        | "=="
                        | "!="
                        | "<"
                        | ">"
                        | "<="
                        | ">="
                ),
                _ => false,
            }
    }

    /// Skips a call that's passed a value whose check failed, or that calls
    /// one, as that was reported where it failed: the call picks nothing,
    /// settles what only it could have said about its arguments as failed
    /// too, and gives a value whose check failed. In `missing().len()`, only
    /// `missing` is reported.
    fn skip_failed_call(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> StaticType {
        for argument in arguments {
            self.vars
                .settle_unknowns_but_literals(argument, &StaticType::Error);
        }
        self.record_pick(call.site, Binding::None, arguments);
        StaticType::Error
    }

    /// Whether an operator might still apply to each element of a tuple: an
    /// operand that isn't known yet can be one, but a number literal can't,
    /// and an `Any` takes a cast to be one.
    fn operand_could_be_a_tuple(&self, kind: CallKind, arguments: &[StaticType]) -> bool {
        kind == CallKind::Operator
            && arguments.iter().any(|argument| {
                matches!(argument, StaticType::Var(var) if !self.vars.is_number_literal(*var))
            })
    }

    /// Whether `symbol` takes each operand that could still be a tuple
    /// whatever that operand turns out to be, so the operator is never
    /// applied to each element: its parameter there is a type of its own,
    /// shared with no other operand or literal. `==`, `<A, B>(A, B)`, takes
    /// `x[0] == x[2]` whole, tuples or not, while `(String, String)` takes
    /// no tuple for `x` in `x ++ "!"`, and neither does `<T>(T, Iterable<T>)`
    /// in `x in "aeiou"`, as `x` must be a String there.
    fn takes_whole_tuples(
        &mut self,
        symbol: SymbolId,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
    ) -> bool {
        let declared = self.vars.resolve(self.types.get_type(symbol));
        let Some(mut made) = self.type_argument_bindings(symbol, type_arguments) else {
            return false;
        };
        self.vars.probe(|vars| {
            match vars.instantiate_with(&declared, &mut made) {
                StaticType::Function {
                    parameters: Some(parameters),
                    ..
                } => {
                    vars.bind_arguments(&parameters, arguments) && {
                        // The overload's own unknowns, as a parameter of a
                        // function being checked has, may be settled later.
                        let resolved = arguments
                            .iter()
                            .chain(std::iter::once(&declared))
                            .map(|typ| vars.unsettled(typ))
                            .collect::<Vec<_>>();
                        arguments.iter().all(|argument| match argument {
                            StaticType::Var(var) if !vars.is_number_literal(*var) => {
                                match vars.resolve(argument) {
                                    StaticType::Var(now) => {
                                        !vars.is_literal(now)
                                            && resolved
                                                .iter()
                                                .filter(|vars| vars.contains(&now))
                                                .count()
                                                == 1
                                    }
                                    _ => false,
                                }
                            }
                            _ => true,
                        })
                    }
                }
                _ => false,
            }
        })
    }

    /// The overload a call of `candidates` with arguments of these types
    /// commits to while the arguments aren't fully known: one that is the
    /// pick however their unknown parts turn out, so learning more can't
    /// change it. That's the only overload, in any scope, that could take
    /// them, counting each unknown as able to become any type, a tuple or an
    /// `Any` among them, and each literal as any type it may become. Or it's
    /// one that wins whatever they turn out to be: see [`Self::dominant`].
    /// An operator may apply to each element of a tuple, which an unknown
    /// operand can be: `fn(x) => x ++ "!"` can take `("a", "b")`, so such a
    /// call waits, unless its overload takes a tuple there whole, as `==`
    /// does: see [`Self::takes_whole_tuples`]. Function values are picked
    /// by the same rule, as calls with the expected parameter types.
    pub(super) fn early_overload(
        &mut self,
        candidates: &Candidates,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        kind: CallKind,
    ) -> EarlyOverload {
        let reachable = self.overloads().reachable_symbols(candidates);
        let literals = arguments
            .iter()
            .flat_map(|argument| self.vars.unsettled(argument))
            .any(|var| self.vars.is_literal(var));
        let viable = reachable
            .iter()
            .copied()
            .filter(|&symbol| self.could_accept(symbol, type_arguments, arguments))
            .collect::<Vec<_>>();
        match viable[..] {
            // A literal may still become `Any`, which an overload that can't
            // take it now might take: `[1, "s"]` holds `Any`s, which
            // `p(x: String)` may get.
            [] if literals && self.fits_once_any(&reachable, type_arguments, arguments, None) => {
                EarlyOverload::Undecided
            }
            [] => EarlyOverload::NoneFits,
            [only]
                if (!self.operand_could_be_a_tuple(kind, arguments)
                    || self.takes_whole_tuples(only, type_arguments, arguments))
                    && self.iterables_known(only, arguments)
                    && !(literals
                        && self.fits_once_any(
                            &reachable,
                            type_arguments,
                            arguments,
                            Some(only),
                        )) =>
            {
                EarlyOverload::Only(only)
            }
            [_] => EarlyOverload::Undecided,
            _ => match self.dominant(candidates, &viable, type_arguments, arguments, kind) {
                Some(dominant) => EarlyOverload::Only(dominant),
                None => EarlyOverload::Undecided,
            },
        }
    }

    /// The one of `viable`, the overloads that could take `arguments`, that
    /// resolution picks however their unknown parts turn out: it's in the
    /// nearest scope with one of them, takes the arguments without settling
    /// any unknown part of them or making two of them one, and is narrower
    /// than the others in its scope. As it always takes the call, overloads
    /// further out never get it, and nearer ones can't take it. In
    /// `multi_cartesian_product([[f, g]])`, `(Iterable<[T]>)` is the
    /// pick over `(Iterable<Any>)` whatever `f` and `g` turn out to be. An
    /// argument nothing is known about could be an `Any`, which only the
    /// overloads taking an `Any` take, so it leaves the pick open.
    fn dominant(
        &mut self,
        candidates: &Candidates,
        viable: &[SymbolId],
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        kind: CallKind,
    ) -> Option<SymbolId> {
        if self.operand_could_be_a_tuple(kind, arguments)
            || arguments
                .iter()
                .any(|argument| matches!(argument, StaticType::Var(_)))
        {
            return None;
        }
        let nearest = self
            .overloads()
            .reachable_groups(candidates)
            .into_iter()
            .map(|group| {
                group
                    .into_iter()
                    .filter(|symbol| viable.contains(symbol))
                    .collect::<Vec<_>>()
            })
            .find(|group| !group.is_empty())?;
        let narrowest = self.most_specific(&nearest)?;
        if !self.iterables_known(narrowest, arguments) {
            return None;
        }
        // What isn't known yet of the overload itself, such as the parameter
        // of a function declared without a type in the function being
        // checked, is as open as what isn't known of the arguments.
        let declared = self.vars.resolve(self.types.get_type(narrowest));
        let unknowns = arguments
            .iter()
            .chain(std::iter::once(&declared))
            .flat_map(|typ| self.vars.unsettled(typ))
            .unique()
            .collect::<Vec<_>>();
        let mut made = self.type_argument_bindings(narrowest, type_arguments)?;
        let free = self
            .vars
            .probe(|vars| match vars.instantiate_with(&declared, &mut made) {
                StaticType::Function {
                    parameters: Some(parameters),
                    ..
                } => {
                    vars.bind_arguments(&parameters, arguments)
                        && unknowns
                            .iter()
                            .filter_map(|&var| match vars.resolve(&StaticType::Var(var)) {
                                StaticType::Var(now) if !vars.is_literal(var) => Some(now),
                                _ => None,
                            })
                            .unique()
                            .count()
                            == unknowns.len()
                }
                _ => false,
            });
        free.then_some(narrowest)
    }

    /// Whether `symbol`'s `Iterable` parameters are given arguments whose
    /// kind of sequence is known. An `Iterable<T>` parameter takes any kind
    /// of sequence, so it can't tell what an argument nothing is known about
    /// is, and the call waits for it as `for x in xs` does.
    fn iterables_known(&self, symbol: SymbolId, arguments: &[StaticType]) -> bool {
        self.overloads()
            .parameters_of(symbol)
            .is_some_and(|parameters| {
                parameters
                    .iter()
                    .zip(arguments)
                    .all(|(parameter, argument)| match parameter {
                        StaticType::Iterable(_) => !self.vars.waits_to_iterate(argument),
                        _ => true,
                    })
            })
    }

    /// `arguments` with each literal among them whose values share no type
    /// as `Any`, when there's one and that leaves nothing unknown.
    fn unshared_literals_as_any(&mut self, arguments: &[StaticType]) -> Option<Vec<StaticType>> {
        let mut unshared = Vec::new();
        for argument in arguments {
            for var in self.vars.unsettled(argument) {
                if !self.vars.is_literal(var) || !self.vars.holds_unshared_values(var) {
                    return None;
                }
                unshared.push(var);
            }
        }
        if unshared.is_empty() {
            return None;
        }
        Some(self.as_any(arguments, |var| unshared.contains(&var)))
    }

    /// `arguments` with each variable that `is_any` picks as `Any`.
    fn as_any(
        &self,
        arguments: &[StaticType],
        is_any: impl Fn(TypeVar) -> bool,
    ) -> Vec<StaticType> {
        arguments
            .iter()
            .map(|argument| {
                self.vars.resolve(argument).replace(&|part| match part {
                    StaticType::Var(var) if is_any(*var) => Some(StaticType::Any),
                    _ => None,
                })
            })
            .collect()
    }

    /// Whether one of `reachable` other than `except` takes `arguments`
    /// once each literal among them is `Any`.
    fn fits_once_any(
        &mut self,
        reachable: &[SymbolId],
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        except: Option<SymbolId>,
    ) -> bool {
        let as_any = self.as_any(arguments, |var| self.vars.is_literal(var));
        reachable
            .iter()
            .filter(|&&symbol| Some(symbol) != except)
            .any(|&symbol| self.could_accept(symbol, type_arguments, &as_any))
    }

    /// The overload a call with arguments of these types means, which
    /// [`Self::pick_overload`] picks with the same test the calls that are
    /// decided early pass, so one decided early and once its arguments are
    /// known is the same. An operator no overload takes applies to each
    /// element of tuple arguments, as `(1, 2) + (3, 4)` adds the pairs.
    pub(super) fn resolve_overloads(
        &mut self,
        candidates: &Candidates,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        kind: CallKind,
    ) -> ResolvedCall {
        match self.pick_overload(candidates, type_arguments, arguments) {
            Pick::One(symbol) => {
                return ResolvedCall {
                    binding: Binding::Resolved(Candidate::Scalar(symbol)),
                    return_type: self.overloads().scalar_return_type(symbol),
                    tied: Vec::new(),
                };
            }
            Pick::Tied(tied) => {
                return ResolvedCall {
                    tied,
                    ..ResolvedCall::none()
                };
            }
            Pick::None => {}
        }
        if kind == CallKind::Operator
            && let Some(axis) = Overloads::tuple_axis(arguments)
        {
            // Each position picks its overload as a call of its elements
            // would, and the call fails or is ambiguous where one does.
            let mut overloads = Vec::with_capacity(axis);
            for position in 0..axis {
                let elements = Overloads::at_position(arguments, position);
                match self.pick_overload(candidates, type_arguments, &elements) {
                    Pick::One(symbol) => overloads.push(symbol),
                    Pick::Tied(tied) => {
                        return ResolvedCall {
                            tied,
                            ..ResolvedCall::none()
                        };
                    }
                    Pick::None => return ResolvedCall::none(),
                }
            }
            let binding = if overloads.iter().all_equal() {
                Binding::Resolved(Candidate::Vec(overloads[0]))
            } else {
                Binding::PerPosition(overloads)
            };
            return ResolvedCall {
                binding,
                return_type: StaticType::Any,
                tied: Vec::new(),
            };
        }
        ResolvedCall::none()
    }

    /// The overload of `candidates` arguments of these types call: of the
    /// nearest scope with overloads that take them, as [`Self::could_accept`]
    /// says, the one more specific than the others. A variadic overload is
    /// the least specific. Overloads none of which is more specific than the
    /// others tie, and the call is ambiguous, unless they take the same
    /// parameters, as a redefinition does. What `return` gives fits any
    /// parameter, so it doesn't pick between overloads: `(return 5) + 1`
    /// is ambiguous, as it is in Rust.
    fn pick_overload(
        &mut self,
        candidates: &Candidates,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
    ) -> Pick {
        for group in self.overloads().reachable_groups(candidates) {
            let viable = group
                .into_iter()
                .filter(|&symbol| self.could_accept(symbol, type_arguments, arguments))
                .collect::<Vec<_>>();
            let Some(&first) = viable.first() else {
                continue;
            };
            let fixed = viable
                .iter()
                .copied()
                .filter(|&symbol| self.overloads().parameters_of(symbol).is_some())
                .collect::<Vec<_>>();
            let narrowest = match fixed[..] {
                [] if viable.len() == 1 => Some(first),
                [] => None,
                [only] => Some(only),
                _ => self.most_specific(&fixed),
            };
            let tied = if fixed.is_empty() { viable } else { fixed };
            // Overloads with the same parameters were reported where the
            // later one redefines the earlier, which a call then means.
            let redefined = tied
                .iter()
                .map(|&symbol| {
                    redefinable_parameters(self.overloads().parameters_of(symbol).as_deref())
                })
                .all_equal();
            return match narrowest {
                Some(symbol) => Pick::One(symbol),
                None if redefined => Pick::One(tied[0]),
                None => Pick::Tied(tied),
            };
        }
        Pick::None
    }

    /// Resolves `call` with the arguments as they are, reporting it when no
    /// overload accepts them.
    fn resolve_call_now(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> StaticType {
        let ResolvedCall {
            binding,
            return_type,
            tied,
        } = self.resolve_overloads(&call.candidates, &call.type_arguments, arguments, call.kind);
        if !tied.is_empty() {
            self.report_ambiguous_call(call, arguments, &tied);
            self.record_pick(call.site, Binding::None, arguments);
            return StaticType::Error;
        }
        // `a op= b` with no operator that takes the operands calls an in-place
        // overload if one does, as a set has `-=` but no `-`, which keeps `a`.
        if matches!(binding, Binding::None)
            && let CallSite::OpAssignment(id) = call.site
        {
            let in_place = self.resolver.augmented_candidates(id).in_place.clone();
            let in_place = self
                .resolve_overloads(&in_place, &[], arguments, call.kind)
                .binding;
            if !matches!(in_place, Binding::None) {
                self.record_augmented_pick(id, in_place);
                return arguments[0].clone();
            }
        }
        if matches!(binding, Binding::None) {
            self.report_failed_call(call, arguments);
            self.record_pick(call.site, binding, arguments);
            return StaticType::Error;
        }
        let typ = self.commit_call(&binding, &call.type_arguments, arguments, return_type);
        self.record_pick(call.site, binding, arguments);
        if let Some(typ) = typ {
            self.flush_copies(Some(call.site), call.span);
            typ
        } else {
            self.pending.clear();
            self.report_failed_call(call, arguments);
            StaticType::Error
        }
    }

    /// Settles the types a call's arguments leave unknown from the overload
    /// it picked, and returns the call's type, as [`Self::commit_overload`]
    /// does for each position of an operator on tuples. Returns `None` when
    /// an argument doesn't fit the picked overload after all.
    fn commit_call(
        &mut self,
        binding: &Binding<SymbolId>,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        return_type: StaticType,
    ) -> Option<StaticType> {
        let overloads = match binding {
            Binding::Resolved(Candidate::Scalar(symbol)) => {
                return self.commit_overload(*symbol, type_arguments, arguments, return_type);
            }
            Binding::Resolved(Candidate::Vec(symbol)) => std::slice::from_ref(symbol),
            Binding::PerPosition(overloads) => overloads.as_slice(),
            Binding::None => unreachable!("internal error: a call that picked nothing commits"),
        };
        // An operator on tuples applies an overload at each position, with a
        // scalar operand used at every position.
        let Some(length) = Overloads::tuple_axis(arguments) else {
            return Some(return_type);
        };
        let mut returns = Vec::with_capacity(length);
        for position in 0..length {
            let overload = overloads.get(position).unwrap_or(&overloads[0]);
            let arguments = Overloads::at_position(arguments, position);
            returns.push(self.commit_overload(
                *overload,
                type_arguments,
                &arguments,
                StaticType::Any,
            )?);
        }
        Some(StaticType::Tuple(returns))
    }

    /// Settles the types a call's arguments leave unknown from `symbol`,
    /// the overload it picked, and returns the call's type. The signature
    /// gets new variables for its type parameters, and each argument is
    /// passed where its parameter is expected: `push(xs, 1)` with
    /// `push<T>([T], T)` settles `xs` as a `[Int]`, and
    /// `push(xs, "a")` then doesn't fit. Returns `None` when an argument
    /// doesn't fit, which settles nothing.
    fn commit_overload(
        &mut self,
        symbol: SymbolId,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        return_type: StaticType,
    ) -> Option<StaticType> {
        let pending = self.pending.len();
        let signature = self.instantiate_explicit(symbol, type_arguments)?;
        match signature {
            StaticType::Function {
                parameters: Some(_),
                ..
            } => {
                let typ = self.bind_signature(&signature, arguments);
                if typ.is_none() {
                    self.pending.truncate(pending);
                }
                typ
            }
            // A function that takes anything, as `print` does, takes each
            // value as the type it is, as a type parameter would, so it
            // settles nothing.
            StaticType::Function {
                parameters: None, ..
            } => Some(return_type),
            // A callee whose type doesn't say what it takes gets the
            // arguments as they are: what's unknown in them is an `Any`,
            // but a literal says what it is.
            _ => {
                for argument in arguments {
                    self.vars
                        .settle_unknowns_but_literals(argument, &StaticType::Any);
                }
                Some(return_type)
            }
        }
    }

    /// `symbol`'s type with new variables for its type parameters. For a
    /// generic function declared in the program, its constraints get the
    /// same variables, and wait in [`Self::pending`] until
    /// [`Self::flush_copies`] knows which call they're for.
    pub(super) fn instantiate_function(&mut self, symbol: SymbolId) -> StaticType {
        self.instantiate_explicit(symbol, &[])
            .expect("internal error: every function takes no type arguments")
    }

    /// [`Self::instantiate_function`] with `type_arguments` for the type
    /// parameters `symbol` takes them for, or `None`, settling nothing, if
    /// it doesn't take that many or they don't fit.
    pub(super) fn instantiate_explicit(
        &mut self,
        symbol: SymbolId,
        type_arguments: &[StaticType],
    ) -> Option<StaticType> {
        let declared = self.vars.resolve(self.types.get_type(symbol));
        let mut made = self.type_argument_bindings(symbol, type_arguments)?;
        if let Some(scheme) = self.schemes.get(&symbol) {
            for name in scheme.contents.iter() {
                if !made.iter().any(|(bound, _)| bound == name) {
                    made.push((name.clone(), self.vars.fresh_contents()));
                }
            }
        }
        let typ = self.vars.instantiate_with(&declared, &mut made);
        // A copy of a function's own constraint that calls the function
        // again, as a recursive call its body couldn't decide does, is
        // checked by the copies the outer call already made, for the types
        // it was called with. A recursive call with other types, as an
        // overload that waited can make it, needs another instance, whose
        // constraints are checked as it's solved.
        let recursive = self
            .copied_through
            .iter()
            .flat_map(|frames| frames.iter())
            .any(|frame| frame.function == symbol);
        let Some(scheme) = self.schemes.get(&symbol).cloned() else {
            return Some(typ);
        };
        if !recursive {
            for waiting in scheme.constraints.iter() {
                let constraint = waiting
                    .constraint
                    .map_types(&mut |typ| self.vars.instantiate_with(typ, &mut made));
                self.pending
                    .copies
                    .push((symbol, constraint, waiting.origin.clone()));
            }
        }
        if !scheme.key.is_empty() {
            let key = scheme
                .key
                .iter()
                .map(|name| {
                    self.vars
                        .instantiate_with(&StaticType::Generic(name.clone()), &mut made)
                })
                .collect();
            self.pending.uses.push((symbol, key));
        }
        Some(typ)
    }

    /// Adds the constraints [`Self::instantiate_function`] copied for a call
    /// at `call`, and solves the ones it can. A call at `site` gets the
    /// instance its types need; a call of a value, with no `site`, calls
    /// whatever function the value is.
    pub(super) fn flush_copies(&mut self, site: Option<CallSite>, call: Span) {
        match site {
            Some(site) => self.record_uses(site, false, call),
            None => self.pending.uses.clear(),
        }
        self.add_copies(call);
    }

    /// Adds the constraints [`Self::instantiate_function`] copied for the
    /// function value at `id`, as [`Self::flush_copies`] does for a call.
    /// The value is the instance for the types it's used at: in
    /// `[[1, 2]].count(is_safe)`, the one for `[Int]`. A type of it
    /// nothing settles, as in `let f: Any = is_safe`, is a failed check's,
    /// so it's reported once, as the value's, and not by its body. A type
    /// the body's constraints connect to its parameters that the value's
    /// type doesn't show, as the list's in `fn f(x) => pick([], x)`, is left
    /// to what settles it, as at a call.
    pub(super) fn flush_value_copies(&mut self, id: NodeId, typ: &StaticType, span: Span) {
        let shown = self.vars.unsettled(typ);
        let keys = self
            .pending
            .uses
            .iter()
            .flat_map(|(_, key)| key.iter())
            .flat_map(|part| self.vars.unsettled(part))
            .filter(|var| shown.contains(var))
            .collect::<Vec<_>>();
        for var in keys {
            self.vars.fall_back_as_errors(&StaticType::Var(var));
        }
        self.record_uses(CallSite::Call(id), true, span);
        self.add_copies(span);
    }

    /// Adds the constraints [`Self::instantiate_function`] copied for a
    /// call or function value at `call`, and solves the ones it can.
    fn add_copies(&mut self, call: Span) {
        for (sequence, element) in std::mem::take(&mut self.pending.iterations) {
            self.wait(Constraint::Iterate {
                sequence,
                element,
                span: call,
            });
        }
        let copies = std::mem::take(&mut self.pending.copies);
        for (function, constraint, origin) in copies {
            let name = self.resolver.symbol_name(function).to_string();
            let frames = self
                .copied_through
                .iter()
                .flat_map(|outer| outer.iter().cloned())
                .chain(std::iter::once(Frame {
                    function,
                    name,
                    call,
                }))
                .chain(origin.iter().flat_map(|inner| inner.iter().cloned()))
                .collect::<Vec<_>>();
            self.mark_join(&constraint);
            let mut waiting = Waiting {
                constraint,
                origin: Some(frames.into()),
            };
            if !self.try_waiting(&mut waiting) {
                self.wait_as(waiting);
            }
        }
    }

    /// Passes each argument where `signature` expects its parameter, which
    /// settles what's unknown in either, and returns what the signature
    /// returns. An argument whose kind of sequence isn't known yet, passed
    /// for an `Iterable`, waits for it: see [`Self::iterations_waited_for`].
    /// Returns `None` and settles nothing when an argument doesn't fit, or
    /// the number of arguments is wrong.
    pub(super) fn bind_signature(
        &mut self,
        signature: &StaticType,
        arguments: &[StaticType],
    ) -> Option<StaticType> {
        let StaticType::Function {
            parameters: Some(parameters),
            return_type,
        } = signature
        else {
            unreachable!("only a function with known parameters has a signature to bind");
        };
        if !self
            .vars
            .attempt(|vars| vars.bind_arguments(parameters, arguments))
        {
            return None;
        }
        let iterations = self.iterations_waited_for(parameters, arguments);
        self.pending.iterations.extend(iterations);
        Some(self.vars.resolve(return_type))
    }

    /// Each argument nothing is known about yet that was passed for an
    /// `Iterable<T>`, or that a function passed for one returns, or a tuple
    /// whose elements aren't, with the
    /// `T` that iterating over it gives once it's known: in `fn total(xs)
    /// => xs.sum()`, what `xs` is waits for a call, and so does `xs` in
    /// `fn flatten(xs) => [0].flat_map(fn(_) => xs)`.
    fn iterations_waited_for(
        &self,
        parameters: &[StaticType],
        arguments: &[StaticType],
    ) -> Vec<(StaticType, StaticType)> {
        parameters
            .iter()
            .zip(arguments)
            .filter_map(|(parameter, argument)| {
                let (element, sequence) =
                    match (self.vars.resolve(parameter), self.vars.resolve(argument)) {
                        (StaticType::Iterable(element), sequence) => (*element, sequence),
                        (
                            StaticType::Function { return_type, .. },
                            StaticType::Function {
                                return_type: returns,
                                ..
                            },
                        ) => match *return_type {
                            StaticType::Iterable(element) => (*element, *returns),
                            _ => return None,
                        },
                        _ => return None,
                    };
                match sequence {
                    // A literal whose type isn't known yet either, as the
                    // element of `[1][0]` is, waits like any other value.
                    StaticType::Var(_) => Some((sequence, element)),
                    // So does a tuple whose elements aren't known yet, as
                    // `(x, x)` in `[0].flat_map(fn(_) => (x, x))`.
                    StaticType::Tuple(_) if self.vars.waits_to_iterate(&sequence) => {
                        Some((sequence, element))
                    }
                    _ => None,
                }
            })
            .collect()
    }

    /// The type of calling a value whose type isn't known yet, like `g` in
    /// `fn(g) => g(1)`: the value is a function taking these arguments. A
    /// value that can't be one, like `g` in `g(g)`, which would have to take
    /// itself, stays unknown, and the call is left to the program as it runs.
    pub(super) fn call_unknown_function(
        &mut self,
        callee: &StaticType,
        arguments: Vec<StaticType>,
    ) -> StaticType {
        let returns = self.vars.fresh();
        let function = StaticType::Function {
            parameters: Some(arguments),
            return_type: Box::new(returns.clone()),
        };
        if self.vars.try_unify(callee, &function) {
            self.vars.resolve(&returns)
        } else {
            StaticType::Any
        }
    }

    /// Whether some overload of `call` accepts arguments of these types.
    fn call_fits(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> bool {
        let fits = self.probe(|this| {
            let ResolvedCall {
                binding,
                return_type,
                ..
            } = this.resolve_overloads(
                &call.candidates,
                &call.type_arguments,
                arguments,
                call.kind,
            );
            !matches!(binding, Binding::None)
                && this
                    .commit_call(&binding, &call.type_arguments, arguments, return_type)
                    .is_some()
        });
        self.pending.clear();
        fits
    }

    /// The one of `symbols` whose parameters are narrower than every other
    /// one's, as `[](%{K: V}, K)` is than `[](Any, Any)`, if there is one.
    /// A single function is the narrowest of one.
    fn most_specific(&self, symbols: &[SymbolId]) -> Option<SymbolId> {
        let parameters = symbols
            .iter()
            .map(|&symbol| self.overloads().parameters_of(symbol))
            .collect::<Option<Vec<_>>>()?;
        let (narrowest, _) = parameters.iter().enumerate().find(|&(i, narrower)| {
            parameters
                .iter()
                .enumerate()
                .all(|(j, wider)| i == j || is_strictly_more_specific(narrower, wider))
        })?;
        Some(symbols[narrowest])
    }

    /// Whether `symbol` can accept arguments of these types, where each part
    /// not known yet fits anything.
    pub(super) fn could_accept(
        &mut self,
        symbol: SymbolId,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
    ) -> bool {
        let function = self.vars.resolve(self.types.get_type(symbol));
        let Some(mut made) = self.type_argument_bindings(symbol, type_arguments) else {
            return false;
        };
        self.vars.probe(|vars| {
            let Some(parameters) = vars.instantiate_parameters(&function, &mut made) else {
                return matches!(
                    function,
                    StaticType::Function { .. }
                        | StaticType::Var(_)
                        | StaticType::Generic(_)
                        | StaticType::Error
                );
            };
            vars.bind_arguments(&parameters, arguments)
        })
    }

    /// The type parameters `symbol` takes type arguments for, in order:
    /// see [`Analyser::explicit_type_parameters`].
    pub(super) fn explicit_type_parameters(&self, symbol: SymbolId) -> Cow<'_, [StaticType]> {
        if let Some(declared) = self.explicit_type_parameters.get(&symbol) {
            return Cow::Borrowed(declared);
        }
        self.types
            .builtin_type_parameters(symbol)
            .unwrap_or(Cow::Borrowed(&[]))
    }

    /// What `type_arguments` bind `symbol`'s type parameters to, in order,
    /// for instantiating its type with: `f::<Int>` binds `f<T>`'s `T` to
    /// `Int`. No type arguments bind nothing, as the call's arguments settle
    /// the type parameters. `None` if it doesn't take that many. Within its
    /// own body a function's type parameters are still variables, which
    /// only themselves bind: a recursive use passes its own type parameters
    /// on, as in `fn f<T>(x: T) => f::<T>(x)`. That settles nothing, so
    /// nothing has to be rolled back.
    pub(super) fn type_argument_bindings(
        &self,
        symbol: SymbolId,
        type_arguments: &[StaticType],
    ) -> Option<Vec<(Box<str>, StaticType)>> {
        let mut made = Vec::new();
        if type_arguments.is_empty() {
            return Some(made);
        }
        let parameters = self.explicit_type_parameters(symbol);
        if parameters.len() != type_arguments.len() {
            return None;
        }
        for (parameter, argument) in parameters.iter().zip(type_arguments) {
            match self.vars.resolve(parameter) {
                StaticType::Generic(name) => made.push((name, argument.clone())),
                parameter => {
                    if parameter != self.vars.resolve(argument) {
                        return None;
                    }
                }
            }
        }
        Some(made)
    }

    /// The function being declared, if `reachable`, the functions a use of
    /// a name in its body can mean, has it, and the use gives it as many
    /// type arguments as it takes, but not its own type parameters: `f::<Int>`
    /// in `fn f<T>`. That's an error even where another overload of `f`
    /// could take them, so that `f::<…>` in `f`'s body means the same
    /// wherever it's written.
    pub(super) fn passes_other_type_arguments(
        &self,
        reachable: &[SymbolId],
        type_arguments: &[StaticType],
    ) -> Option<SymbolId> {
        let function = self.declaring?;
        (!type_arguments.is_empty()
            && self.copied_through.is_none()
            && self.recording.is_none()
            && reachable.contains(&function)
            && self.explicit_type_parameters(function).len() == type_arguments.len()
            && self
                .type_argument_bindings(function, type_arguments)
                .is_none())
        .then_some(function)
    }

    /// The error for a use of `symbol` in its own body with type arguments
    /// other than its own type parameters.
    pub(super) fn recursive_type_arguments(&self, symbol: SymbolId, span: Span) -> AnalysisError {
        let parameters = self
            .explicit_type_parameters(symbol)
            .iter()
            .map(|parameter| {
                // While the body is checked, a type parameter is a variable,
                // known by the name it's declared with.
                self.type_parameter_vars
                    .iter()
                    .find(|(_, var)| *var == parameter)
                    .map_or_else(
                        || self.vars.shown(parameter).to_string(),
                        |(&declared, _)| self.resolver.symbol_name(declared).to_string(),
                    )
            })
            .collect::<Vec<_>>();
        AnalysisError::recursive_type_arguments(
            self.resolver.symbol_name(symbol),
            &parameters,
            span,
        )
    }

    /// The overloads a call at `site` can mean. An intrinsic has no
    /// signature that types it, so a call is only of one when its typing
    /// rule picks it, as `cast::<Int>(x)` is (see
    /// [`Analyser::analyse_call`]): `x.cast` reads no getter called `cast`.
    fn site_candidates(&self, site: CallSite) -> Candidates {
        Candidates::from_groups(
            self.site_references(site)
                .iter()
                .map(|group| {
                    group
                        .iter()
                        .copied()
                        .filter(|&symbol| self.resolver.intrinsic(symbol).is_none())
                        .collect::<Vec<_>>()
                })
                .filter(|group| !group.is_empty())
                .collect(),
        )
    }

    /// Every function a call at `site` can name, intrinsics too.
    fn site_references(&self, site: CallSite) -> &Candidates {
        match site {
            CallSite::Call(id) => self.resolver.reference_candidates(id),
            CallSite::Getter(id) => self
                .resolver
                .getter_candidates(id)
                .expect("internal error: a getter call without getter candidates"),
            CallSite::Setter(id) | CallSite::FieldSetter(id) => self.resolver.setter_candidates(id),
            CallSite::OpAssignment(id) => &self.resolver.augmented_candidates(id).operator,
        }
    }

    pub(super) fn record_pick(
        &mut self,
        site: CallSite,
        binding: Binding<SymbolId>,
        arguments: &[StaticType],
    ) {
        if self.copied_through.is_some() {
            return;
        }
        // A generic function's body calls itself with its own types, so an
        // instance's recursive call is a call of the instance.
        if let (CallSite::Call(id), Binding::Resolved(Candidate::Scalar(callee)), None) =
            (site, &binding, self.recording)
            && self.declaring == Some(*callee)
        {
            self.recursive_calls.push(id);
        }
        match site {
            CallSite::Call(_)
            | CallSite::Getter(_)
            | CallSite::Setter(_)
            | CallSite::FieldSetter(_) => {
                self.pick_tables(self.recording).insert(site, binding);
            }
            // An in-place overload such as `++=` does the same thing faster,
            // so it's used where one takes the operands as they are.
            CallSite::OpAssignment(id) => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.vars.resolve(argument))
                    .collect::<Vec<_>>();
                let candidates = self.resolver.augmented_candidates(id).in_place.clone();
                let in_place =
                    self.resolve_overloads(&candidates, &[], &arguments, CallKind::Operator);
                let binding = match in_place.binding {
                    Binding::None => binding,
                    in_place => in_place,
                };
                self.record_augmented_pick(id, binding);
            }
        };
    }

    /// Records what `a op= b` at `id` does, as [`Self::record_pick`].
    pub(super) fn record_augmented_pick(&mut self, id: NodeId, binding: Binding<SymbolId>) {
        if self.copied_through.is_none() {
            self.pick_tables(self.recording)
                .insert(CallSite::OpAssignment(id), binding);
        }
    }

    /// Reports that overloads `tied` take `call` equally well.
    fn report_ambiguous_call(
        &mut self,
        call: &CallConstraint,
        arguments: &[StaticType],
        tied: &[SymbolId],
    ) {
        let arguments = arguments
            .iter()
            .map(|argument| self.vars.shown(argument))
            .collect::<Vec<_>>();
        let overloads = tied
            .iter()
            .map(|&symbol| {
                let typ = self.vars.shown(self.types.get_type(symbol));
                match typ {
                    StaticType::Function {
                        parameters: Some(parameters),
                        ..
                    } => format!(
                        "{}({})",
                        call.name,
                        parameters
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                    other => format!("{}: {other}", call.name),
                }
            })
            .collect::<Vec<_>>();
        self.emit(AnalysisError::ambiguous_call(
            &call.name, &arguments, &overloads, call.span,
        ));
    }

    /// Reports that no overload accepts `call`. Writing `xs[i] += 1` reads
    /// and writes `xs[i]`, and when the read fails, the write fails the same
    /// way, which is reported once.
    fn report_failed_call(&mut self, call: &CallConstraint, arguments: &[StaticType]) {
        let arguments = &arguments
            .iter()
            .map(|argument| self.vars.shown(argument))
            .collect::<Vec<_>>();
        let error = match call.site {
            CallSite::Call(_) | CallSite::Getter(_) | CallSite::OpAssignment(_) => {
                if let CallSite::Getter(id) = call.site {
                    self.failed_reads.insert(id);
                }
                // No overload takes as many type arguments as the call
                // gives, which is what's wrong whatever the arguments are.
                // An intrinsic the call can't mean still takes as many as it
                // does: `cast` takes one.
                let given = call.type_arguments.len();
                let counts = self.type_argument_counts(self.site_references(call.site));
                if given > 0 && !counts.contains(&given) {
                    AnalysisError::wrong_type_argument_count(&call.name, &counts, given, call.span)
                } else {
                    self.no_match(
                        &call.candidates,
                        &call.type_arguments,
                        &call.name,
                        arguments,
                        call.span,
                    )
                }
            }
            CallSite::Setter(id) | CallSite::FieldSetter(id) if self.failed_reads.contains(&id) => {
                return;
            }
            CallSite::FieldSetter(_) => {
                AnalysisError::function_not_found(&call.name, arguments, call.span)
            }
            CallSite::Setter(_) => match arguments.as_slice() {
                [container, ..]
                    if container.index_element_type().is_none() && !container.is_unknown() =>
                {
                    AnalysisError::unable_to_index_into(container, call.span)
                }
                // The container and index fit, but the value doesn't fit
                // what the container holds there: an element, or a list of
                // them for a slice like `xs[0..2]`.
                [container, index, value]
                    if let Some(element) = container.index_element_type()
                        && let expected = if Self::index_type_is_slice(index) {
                            StaticType::List(Box::new(element))
                        } else {
                            element
                        }
                        && self.call_fits(
                            call,
                            &[container.clone(), index.clone(), expected.clone()],
                        ) =>
                {
                    AnalysisError::mismatched_types(value, &expected, call.span)
                }
                _ => AnalysisError::function_not_found(&call.name, arguments, call.span),
            },
        };
        self.emit(error);
    }
}
