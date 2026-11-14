//! Calls and other uses whose types aren't known yet wait here until they
//! are. In `xs.map(fn(x) => x + 1)`, the `+` waits for `x`'s type, which
//! picking `map` settles from the type of `xs`.

use super::Analyser;
use super::function_values::{Choice, FunctionValue};
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall, is_strictly_more_specific};
use crate::types::TypeVars;
use ahash::{AHashMap, AHashSet};
use itertools::Itertools;
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType, TypeVar};
use ndc_lexer::Span;
use ndc_resolver::{Candidates, SymbolId};
use std::collections::BTreeSet;
use std::rc::Rc;

/// Whether a waiting call can still take its arguments.
enum Fit {
    /// Its overload is known and takes them.
    Picked,
    /// Some overload might.
    Maybe,
    /// None can.
    No,
}

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
    /// wholly unknown: the branches' types and then the result's.
    Join { types: [StaticType; 3], span: Span },
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

    /// The constraint with `replace` applied to each of its types.
    pub(super) fn map_types(&self, replace: &mut impl FnMut(&StaticType) -> StaticType) -> Self {
        match self {
            Self::Call(call) => Self::Call(CallConstraint {
                arguments: call.arguments.iter().map(&mut *replace).collect(),
                result: replace(&call.result),
                ..call.clone()
            }),
            Self::FunctionValue(value) => {
                Self::FunctionValue(value.with_expected(replace(value.expected())))
            }
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
            Self::Join { types, span } => Self::Join {
                types: types.clone().map(|typ| replace(&typ)),
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
        candidates: Candidates,
        arguments: Vec<StaticType>,
        kind: CallKind,
        name: String,
        binding: Binding<SymbolId>,
    },
    FunctionValue {
        value: FunctionValue,
        picked: SymbolId,
    },
}

#[derive(Debug, Clone)]
pub(super) struct CallConstraint {
    site: CallSite,
    candidates: Candidates,
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
    pub(super) fn site(&self) -> CallSite {
        self.site
    }

    pub(super) fn candidates(&self) -> &Candidates {
        &self.candidates
    }

    pub(super) fn arguments(&self) -> &[StaticType] {
        &self.arguments
    }

    pub(super) fn result(&self) -> &StaticType {
        &self.result
    }

    pub(super) fn kind(&self) -> CallKind {
        self.kind
    }

    /// Whether `other` calls what this calls, the same way, with arguments
    /// of the same types, so it returns the same type.
    pub(super) fn is_same_call(&self, other: &Self, vars: &TypeVars) -> bool {
        std::mem::discriminant(&self.site) == std::mem::discriminant(&other.site)
            && self.kind == other.kind
            && self.candidates == other.candidates
            && self.tuple_index == other.tuple_index
            && self.arguments.len() == other.arguments.len()
            && self
                .arguments
                .iter()
                .zip(&other.arguments)
                .all(|(a, b)| vars.resolve(a) == vars.resolve(b))
    }
}

impl Analyser {
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
        self.constrain_index_call(site, arguments, kind, name, span, None)
    }

    /// [`Self::constrain_call`], for `[]` with the index `tuple_index` when
    /// it's a number written in the code, which picks a tuple's element.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn constrain_index_call(
        &mut self,
        site: CallSite,
        arguments: Vec<StaticType>,
        kind: CallKind,
        name: &str,
        span: Span,
        tuple_index: Option<i64>,
    ) -> StaticType {
        let mut call = CallConstraint {
            candidates: self.site_candidates(site).clone(),
            site,
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
        call.result = self.vars.fresh(StaticType::Any);
        let result = call.result.clone();
        self.wait(Constraint::Call(call));
        result
    }

    /// What iterating over a value of type `sequence` gives, which waits
    /// until the kind of sequence is known.
    pub(super) fn constrain_iteration(&mut self, sequence: StaticType, span: Span) -> StaticType {
        if self.vars.waits_to_iterate(&sequence) {
            let element = self.vars.fresh(StaticType::Any);
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

    /// The types `let (a, b, …) = value` gives each name when the kind of
    /// value isn't known yet. They're settled once it is.
    pub(super) fn constrain_unpacking(
        &mut self,
        value: StaticType,
        count: usize,
        span: Span,
    ) -> Vec<StaticType> {
        let parts = (0..count)
            .map(|_| self.vars.fresh(StaticType::Any))
            .collect::<Vec<_>>();
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
    /// `List<Int>`. Types that can't be made the same are held as a
    /// literal's values are, so `if c { 1 } else { "a" }` is an `Any` where
    /// one is expected, and an error where not.
    /// A branch nothing is known about yet waits, so a lambda's parameter
    /// isn't fixed by the branch next to it: in `fn(x) => if c { x } else
    /// { 0 }`, `x` can still be a `String`.
    pub(super) fn join(&mut self, a: &StaticType, b: &StaticType, span: Span) -> StaticType {
        let (a, b) = (self.vars.resolve(a), self.vars.resolve(b));
        let unknown = |this: &Self, typ: &StaticType| matches!(typ, StaticType::Var(var) if !this.vars.is_literal(*var));
        match (&a, &b) {
            (StaticType::Never, _) => b,
            (_, StaticType::Never) => a,
            _ if a == b => a,
            _ if unknown(self, &a) || unknown(self, &b) => {
                let result = self.vars.fresh(StaticType::Any);
                self.wait(Constraint::Join {
                    types: [a, b, result.clone()],
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
        let snapshot = self.vars.snapshot();
        if self.vars.unify(a, b) {
            return self.vars.resolve(a);
        }
        self.vars.rollback(snapshot);
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
        let binding = Binding::Resolved(Candidate::Scalar(picked));
        if cfg!(debug_assertions) {
            self.early_picks.push(EarlyPick::Call {
                candidates: call.candidates.clone(),
                arguments: call.arguments.clone(),
                kind: call.kind,
                name: call.name.clone(),
                binding: binding.clone(),
            });
        }
        self.record_pick(call.site, binding, &arguments);
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
                EarlyPick::Call {
                    candidates,
                    arguments,
                    kind,
                    name,
                    binding: picked,
                } => {
                    let arguments = arguments
                        .iter()
                        .map(|argument| self.vars.resolve(argument))
                        .collect::<Vec<_>>();
                    // A call passed a value whose check failed later picks
                    // nothing, and the program doesn't run.
                    if arguments.iter().any(StaticType::has_errors) {
                        continue;
                    }
                    let binding = self
                        .resolve_overloads(&candidates, &arguments, kind)
                        .binding;
                    let binding = self.call_of_the_only_one(binding);
                    // A call that nothing takes any more is one an error
                    // was reported for, as an argument whose overload
                    // couldn't be told then got the fallback `Any`, and a
                    // program with errors doesn't run.
                    if binding == Binding::None && !self.result.errors.is_empty() {
                        continue;
                    }
                    assert!(
                        binding == picked,
                        "internal error: `{name}` committed to {picked:?} before its arguments \
                         were known, but with ({}) it means {binding:?}",
                        arguments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", "),
                    );
                }
                EarlyPick::FunctionValue { value, picked } => {
                    let expected = self.vars.resolve(value.expected());
                    if expected.has_errors() {
                        continue;
                    }
                    let choice = self.choose_function_value(&value, &expected);
                    // As for a call: a value nothing fits any more got the
                    // fallback `Any` from a failed use, which was reported.
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
                let value = self.vars.fresh(StaticType::Any);
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

    /// Makes `constraint` wait, in the order it started waiting, until one
    /// of the variables it waits on is settled.
    pub(super) fn wait(&mut self, constraint: Constraint) {
        let origin = self.copied_through.clone();
        self.wait_as(Waiting { constraint, origin });
    }

    /// Makes `waiting` wait, as [`Self::wait`].
    pub(super) fn wait_as(&mut self, waiting: Waiting) {
        let serial = self.next_serial;
        self.next_serial += 1;
        self.wait_again(serial, waiting);
    }

    fn wait_again(&mut self, serial: u64, waiting: Waiting) {
        for var in waiting
            .constraint
            .inputs()
            .iter()
            .flat_map(|input| self.vars.unsettled(input))
        {
            self.watchers.entry(var).or_default().push(serial);
        }
        self.waiting.insert(serial, waiting);
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
    /// defaults first. Then what a default rule can decide is decided by it:
    /// a call commits to its narrowest overload, `a op= b` keeps the type of
    /// `b`, and a branch of an `if` takes the other branch's type. What's
    /// left gets its fallbacks, oldest constraint first. Constraints that wait only on
    /// older variables keep waiting.
    pub(super) fn settle_waiting(&mut self, first: usize) {
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
            while let Some(serial) = unchecked.pop_first() {
                let vars = self.waits_on_vars_since(serial, first);
                if !vars.is_empty()
                    && (self.default_to_narrowest(serial)
                        || self.default_to_operand(serial)
                        || self.default_to_branch(serial, &vars))
                {
                    unchecked.extend(self.solve_waiting_and_report());
                }
            }
            let next = self
                .waiting
                .range(fallen_back..)
                .map(|(&serial, _)| serial)
                .find_map(|serial| {
                    let vars = self.waits_on_vars_since(serial, first);
                    (!vars.is_empty()).then_some((serial, vars))
                });
            let Some((serial, vars)) = next else {
                break;
            };
            fallen_back = serial;
            self.vars.settle_with_fallbacks(&vars);
            unchecked.extend(self.solve_waiting_and_report());
        }
        self.vars.settle_from(first);
        self.solve_waiting();
        // A call can still wait when its arguments are known: a variable's
        // type might have settled since, which nothing watches.
        let serials = self.waiting.keys().copied().collect::<Vec<_>>();
        for serial in serials {
            self.retry(serial);
        }
        self.solve_waiting();
    }

    /// Settles the types of the literals that `belongs` says to that nothing
    /// settled, and returns whether it settled any. Each takes the first of
    /// its defaults that every waiting call it's in can still take: in
    /// `let total = 0; total += 2.5`, `+` with a Float can't give an Int
    /// back to `total`, so `0` is a Float.
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
                let fits = defaults.iter().position(|default| {
                    let snapshot = self.vars.snapshot();
                    let fits = self.literal_fits_as(var, default, &holders);
                    self.vars.rollback(snapshot);
                    fits
                });
                // When none fits, the first one says best what's wrong.
                let default = &defaults[fits.unwrap_or(0)];
                let holders_of_var = holders
                    .get(&var)
                    .into_iter()
                    .flatten()
                    .copied()
                    .filter(|&holder| self.vars.is_literal(holder))
                    .collect::<Vec<_>>();
                let snapshot = self.vars.snapshot();
                // Values that share no type are an error, which its fellows
                // aren't part of.
                if *default == StaticType::Error {
                    self.vars.settle_default(var, default);
                } else if self
                    .settle_with_fellows(var, default, &holders_of_var)
                    .is_none()
                {
                    self.vars.rollback(snapshot);
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
            let snapshot = self.vars.snapshot();
            let fits = self.vars.bind_argument(default, &held);
            self.vars.rollback(snapshot);
            if !fits {
                let error = AnalysisError::mismatched_types(&self.vars.shown(&held), default, span);
                self.emit(error);
            }
        }
        self.vars.settle_literal(var, default);
    }

    /// Settles the literal `var` as `default`, and returns whether the
    /// waiting calls on it still fit, and those on the literals that hold
    /// it, `holders` says which, with each settled as one of its own
    /// defaults: in `let x = n[0]; x = 0.5` on `n = [1]`, `n`'s element type
    /// holds the `1`, and `[]` gives a Float only if it's a Float. The
    /// other values those hold take the same type, as the values of a
    /// literal have one type when they can: `[1, 2]` holds Ints or Floats,
    /// not one of each.
    fn literal_fits_as(
        &mut self,
        var: TypeVar,
        default: &StaticType,
        holders: &AHashMap<TypeVar, Vec<TypeVar>>,
    ) -> bool {
        let holders_of_var = holders
            .get(&var)
            .into_iter()
            .flatten()
            .copied()
            .filter(|&holder| self.vars.is_literal(holder))
            .collect::<Vec<_>>();
        let Some(settled) = self.settle_with_fellows(var, default, &holders_of_var) else {
            return false;
        };
        if !self.calls_still_fit(&settled) {
            return false;
        }
        holders_of_var.into_iter().all(|holder| {
            if !self.vars.is_literal(holder) {
                return true;
            }
            // One that still waits for another literal is checked with it.
            let Some(defaults) = self.vars.literal_defaults(holder) else {
                return true;
            };
            defaults.iter().any(|default| {
                let snapshot = self.vars.snapshot();
                let fits = self.literal_fits_as(holder, default, holders);
                if !fits {
                    self.vars.rollback(snapshot);
                }
                fits
            })
        })
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
                let snapshot = self.vars.snapshot();
                if self.vars.settle_literal(other, default) {
                    settled.push(other);
                } else {
                    self.vars.rollback(snapshot);
                }
            }
        }
        Some(settled)
    }

    /// Whether the waiting calls that wait on `vars` can still take their
    /// arguments, and give back what's expected of them, together: in
    /// `let x = xs[0] + 0.5; xs.push(x)` on `xs = [1]`, `+` gives a Float
    /// when the `1` is an Int, which `push` then can't take. A call whose
    /// overload is known says what it gives back, so the calls that wait
    /// on that are checked too, and the others again.
    fn calls_still_fit(&mut self, vars: &[TypeVar]) -> bool {
        let mut serials = Vec::new();
        let mut seen = AHashSet::new();
        for var in vars {
            for &watcher in self.watchers.get(var).into_iter().flatten() {
                if seen.insert(watcher) {
                    serials.push(watcher);
                }
            }
        }
        let mut picked = AHashSet::new();
        loop {
            let mut learned = false;
            let mut next = 0;
            while let Some(&serial) = serials.get(next) {
                next += 1;
                if picked.contains(&serial) {
                    continue;
                }
                let Some(Waiting {
                    constraint: Constraint::Call(call),
                    ..
                }) = self.waiting.get(&serial).cloned()
                else {
                    continue;
                };
                let reached = std::iter::once(&call.result)
                    .chain(&call.arguments)
                    .flat_map(|typ| self.vars.unsettled(typ))
                    .collect::<Vec<_>>();
                match self.call_still_fits(&call) {
                    Fit::No => return false,
                    Fit::Maybe => {}
                    Fit::Picked => {
                        picked.insert(serial);
                        learned = true;
                        for var in reached {
                            for &watcher in self.watchers.get(&var).into_iter().flatten() {
                                if seen.insert(watcher) {
                                    serials.push(watcher);
                                }
                            }
                        }
                    }
                }
            }
            if !learned {
                return true;
            }
        }
    }

    /// Whether some overload of `call` takes its arguments, as far as
    /// they're known, and returns what fits its result. Once they're
    /// known, the call gets the overload that resolution picks, scope by
    /// scope, so only that one counts, and what it binds is kept:
    /// `[](Any, Any)` takes `xs[0]` but loses to `[](List<T>, Int) -> T`.
    fn call_still_fits(&mut self, call: &CallConstraint) -> Fit {
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.vars.resolve(argument))
            .collect::<Vec<_>>();
        if Self::operand_could_be_a_tuple(call.kind, &arguments) {
            return Fit::Maybe;
        }
        if arguments.iter().all(|argument| !argument.has_vars())
            && let Binding::Resolved(Candidate::Scalar(symbol)) = self
                .overloads()
                .resolve_call(&call.candidates, &arguments, call.kind)
                .binding
        {
            if self.takes_a_number_as_a_fallback(symbol, &call.candidates, &arguments) {
                return Fit::No;
            }
            let snapshot = self.vars.snapshot();
            if self.overload_fits(symbol, &arguments, &call.result) {
                return Fit::Picked;
            }
            self.vars.rollback(snapshot);
            return Fit::No;
        }
        let reachable = self.overloads().reachable_symbols(&call.candidates);
        let fits = reachable.into_iter().any(|symbol| {
            let snapshot = self.vars.snapshot();
            let fits = self.overload_fits(symbol, &arguments, &call.result);
            self.vars.rollback(snapshot);
            fits
        });
        if fits { Fit::Maybe } else { Fit::No }
    }

    /// Whether `symbol` takes a number among `arguments` through an `Any`
    /// parameter where another of `candidates` in its scope takes a number
    /// type or a type parameter, which makes `symbol` a fallback that a
    /// literal doesn't take a type to reach: `[](List<T>, Any)` takes the
    /// Float index `0.0`, but `[](List<T>, Int)` is what takes an index.
    /// Where nothing else in that scope takes a number, as for `print`, the
    /// `Any` says nothing about what the literal is.
    fn takes_a_number_as_a_fallback(
        &self,
        symbol: SymbolId,
        candidates: &Candidates,
        arguments: &[StaticType],
    ) -> bool {
        let parameters_of = |symbol: SymbolId| {
            self.overloads()
                .parameters_of(symbol)
                .filter(|parameters| parameters.len() == arguments.len())
        };
        let is_number = |typ: &StaticType| {
            matches!(
                typ,
                StaticType::Int | StaticType::Float | StaticType::Number
            )
        };
        let Some(parameters) = parameters_of(symbol) else {
            return false;
        };
        let fallbacks = parameters
            .iter()
            .zip(arguments)
            .enumerate()
            .filter(|(_, (parameter, argument))| {
                **parameter == StaticType::Any && is_number(argument)
            })
            .map(|(position, _)| position)
            .collect::<Vec<_>>();
        !fallbacks.is_empty()
            && self
                .overloads()
                .same_scope_symbols(candidates, symbol)
                .into_iter()
                .filter(|&other| other != symbol)
                .filter_map(parameters_of)
                .any(|parameters| {
                    fallbacks.iter().any(|&position| {
                        is_number(&parameters[position])
                            || matches!(parameters[position], StaticType::Generic(_))
                    })
                })
    }

    /// Whether `symbol` takes `arguments` and returns what fits `result`,
    /// binding the types on the way. One whose parameters aren't known
    /// takes anything.
    fn overload_fits(
        &mut self,
        symbol: SymbolId,
        arguments: &[StaticType],
        result: &StaticType,
    ) -> bool {
        let function = self.vars.resolve(self.types.get_type(symbol));
        let Some((parameters, return_type)) = self.vars.instantiate_signature(&function) else {
            return true;
        };
        self.vars.bind_arguments(&parameters, arguments)
            && self.vars.bind_argument(result, &return_type)
    }

    /// Tries the waiting constraint `serial` again, if it's still waiting.
    /// A call can wait although its arguments are known: a variable's type
    /// might have settled since, which nothing watches.
    pub(super) fn retry(&mut self, serial: u64) {
        let Some(mut waiting) = self.waiting.remove(&serial) else {
            return;
        };
        if !self.try_waiting(&mut waiting) {
            self.wait_again(serial, waiting);
        }
    }

    /// The unsettled variables made since `first` that the waiting
    /// constraint `serial` waits on, if it's still waiting.
    fn waits_on_vars_since(&self, serial: u64, first: usize) -> Vec<TypeVar> {
        self.waiting.get(&serial).map_or_else(Vec::new, |waiting| {
            waiting
                .constraint
                .inputs()
                .iter()
                .flat_map(|input| self.vars.unsettled(input))
                .filter(|var| var.0 as usize >= first)
                .collect()
        })
    }

    /// What nothing else settled in the waiting `a op= b` keeps the type it
    /// already had, so it's taken from `b`: in `m["k"] += 3` on an empty
    /// map, the values are Ints. Returns whether that settled anything.
    pub(super) fn default_to_operand(&mut self, serial: u64) -> bool {
        let Some(Waiting {
            constraint: Constraint::Call(call),
            ..
        }) = self.waiting.get(&serial)
        else {
            return false;
        };
        let (CallSite::OpAssignment(_), [target, operand]) = (call.site, &call.arguments[..])
        else {
            return false;
        };
        let (target, operand) = (target.clone(), operand.clone());
        let before = self.vars.settled_count();
        self.vars.try_unify(&target, &operand) && self.vars.settled_count() > before
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
                        self.emit_rejected_write(&typ, &result, call.span);
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
                let found = match &value {
                    StaticType::Tuple(elements) if elements.len() != parts.len() => {
                        self.emit(AnalysisError::tuple_arity_mismatch(
                            parts.len(),
                            elements.len(),
                            *span,
                        ));
                        None
                    }
                    value => {
                        let found = value
                            .unpack()
                            .map(|found| found.take(parts.len()).cloned().collect::<Vec<_>>());
                        if found.is_none() {
                            self.emit(AnalysisError::unable_to_unpack_type(value, *span));
                        }
                        found
                    }
                };
                let found = found.unwrap_or_else(|| vec![StaticType::Error; parts.len()]);
                for (part, found) in parts.iter().zip(found) {
                    self.check_write(part, &found, *span);
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
            } => {
                let (a, b, result) = (a.clone(), b.clone(), result.clone());
                // A result known from outside is what's expected of each
                // branch: in `let f: Fn(Int) -> Int = if c { abs } else
                // { floor }`, it says which `abs` and `floor` are meant.
                if !matches!(self.vars.resolve(&result), StaticType::Var(_)) {
                    for side in [&a, &b] {
                        if self.vars.resolve(side).has_vars() {
                            let snapshot = self.vars.snapshot();
                            if !self.vars.bind_argument(&result, side) {
                                self.vars.rollback(snapshot);
                            }
                        }
                    }
                }
                let (a, b) = (self.vars.resolve(&a), self.vars.resolve(&b));
                let unknown = |typ: &StaticType| matches!(typ, StaticType::Var(var) if !self.vars.is_literal(*var));
                if unknown(&a) || unknown(&b) {
                    return false;
                }
                let joined = match (&a, &b) {
                    (StaticType::Never, other) | (other, StaticType::Never) => other.clone(),
                    _ => self.join_known(&a, &b, *span),
                };
                self.check_write(&result, &joined, *span);
                true
            }
        }
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
            self.resolve_call_now(call, &indexed);
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

        let reachable = self.overloads().reachable_symbols(&call.candidates);

        // A generic function that calls itself with an argument made from
        // the parameter it's passed for, as `nest([x], n - 1)` in `fn
        // nest(x, n)`, would need a version of itself for `x`'s type, one
        // for lists of it, one for lists of those, without end.
        // Another overload may take the call, as `f(xs: List<Any>)` does
        // `f([x])`, so it's only this function's when no other can.
        if let Some(function) = self.declaring
            && self.copied_through.is_none()
            && self.recording.is_none()
            && reachable.contains(&function)
            && reachable
                .iter()
                .all(|&symbol| symbol == function || !self.could_accept(symbol, &arguments))
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
        // `f([1, "a"])` with `f(xs: List<Any>)`.
        if let Some(as_any) = self.unshared_literals_as_any(&arguments)
            && let ResolvedCall {
                binding: Binding::Resolved(Candidate::Scalar(picked)),
                ..
            } = self.resolve_overloads(&call.candidates, &as_any, call.kind)
        {
            // An overload picked through a type parameter that the `Any`
            // fits, as `push(List<T>, T)` with a Float for `[1, "a"]`, may
            // not take the literal itself, which then isn't an `Any`.
            let snapshot = self.vars.snapshot();
            if let Some(typ) = self.commit_early(call, &arguments, picked) {
                return Some(typ);
            }
            self.vars.rollback(snapshot);
        }

        match self.early_overload(&call.candidates, &arguments, call.kind) {
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

    /// Skips a call that's passed a value whose check failed, or that calls
    /// one, as that was reported where it failed: the call picks nothing,
    /// settles what only it could have said about its arguments as failed
    /// too, and gives a value whose check failed. In `missing().len()`, only
    /// `missing` is reported.
    fn skip_failed_call(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> StaticType {
        for argument in arguments {
            self.vars.settle_unknowns_as_errors(argument);
        }
        self.record_pick(call.site, Binding::None, arguments);
        StaticType::Error
    }

    /// Whether an operator might still apply to each element of a tuple: an
    /// operand that isn't known yet can be one, but an `Any` takes a cast
    /// to be one.
    fn operand_could_be_a_tuple(kind: CallKind, arguments: &[StaticType]) -> bool {
        kind == CallKind::Operator
            && arguments
                .iter()
                .any(|argument| matches!(argument, StaticType::Var(_)))
    }

    /// The overload a call of `candidates` with arguments of these types
    /// commits to while the arguments aren't fully known: the only one that
    /// can take them, once nothing learned later could make another one the
    /// pick. One that takes them only through an `Any` parameter would lose
    /// to an exact type once they're known, and an operator may also apply
    /// to each element of a tuple, which an unknown or `Any` operand can be:
    /// `fn(x) => x ++ "!"` can take `("a", "b")`.
    /// Function values are picked by the same rule, as calls with the
    /// expected parameter types.
    pub(super) fn early_overload(
        &mut self,
        candidates: &Candidates,
        arguments: &[StaticType],
        kind: CallKind,
    ) -> EarlyOverload {
        let reachable = self.overloads().reachable_symbols(candidates);
        let literals = arguments
            .iter()
            .flat_map(|argument| self.vars.unsettled(argument))
            .filter(|&var| self.vars.is_literal(var))
            .collect::<Vec<_>>();
        // The nearest scope with an overload that takes the call hides those
        // further out. When that scope has one overload, whose parameters
        // take whatever the literals turn out to be, as a nested `fn d(x)`
        // does, the call is its, though a literal isn't decided yet.
        if !literals.is_empty()
            && !Self::operand_could_be_a_tuple(kind, arguments)
            && let Some(nearest) = self
                .overloads()
                .reachable_groups(candidates)
                .into_iter()
                .find(|group| {
                    group
                        .iter()
                        .any(|&symbol| self.could_accept(symbol, arguments))
                })
            && let [only] = nearest[..]
            && self.takes_any_literal(only, arguments)
        {
            return EarlyOverload::Only(only);
        }
        let viable = reachable
            .iter()
            .copied()
            .filter(|&symbol| self.could_accept(symbol, arguments))
            .collect::<Vec<_>>();
        match viable[..] {
            // A literal may still become `Any`, which an overload that can't
            // take it now might take: `[1, "s"]` holds `Any`s, which
            // `p(x: String)` may get.
            [] if !literals.is_empty() && self.fits_once_any(&reachable, arguments, None) => {
                EarlyOverload::Undecided
            }
            [] => EarlyOverload::NoneFits,
            [only]
                if self.fits_without_any(only, arguments)
                    && !Self::operand_could_be_a_tuple(kind, arguments)
                    && !self.merges_literals(only, arguments) =>
            {
                // When the pick depends on what a literal is now, as
                // `p(x)` over `p(x: Int)` does for the Strings in
                // `["s"].map(p)`, the literal is what it is from here on:
                // the first use that depends on it decides it.
                // What they're decided as may still leave the pick open,
                // as `Any` would, so it's made again with them settled.
                if !literals.is_empty() && self.fits_once_any(&reachable, arguments, Some(only)) {
                    if !self.decide_literals(&literals, only, arguments) {
                        return EarlyOverload::Undecided;
                    }
                    let arguments = arguments
                        .iter()
                        .map(|argument| self.vars.resolve(argument))
                        .collect::<Vec<_>>();
                    return self.early_overload(candidates, &arguments, kind);
                }
                EarlyOverload::Only(only)
            }
            _ => EarlyOverload::Undecided,
        }
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
        Some(
            arguments
                .iter()
                .map(|argument| {
                    self.vars.resolve(argument).replace(&|part| match part {
                        StaticType::Var(var) if unshared.contains(var) => Some(StaticType::Any),
                        _ => None,
                    })
                })
                .collect(),
        )
    }

    /// Whether `symbol`'s parameter for each argument with a literal in it
    /// takes any type, as an unannotated one does, and the other arguments
    /// are known, so nothing still to come can change whether `symbol` takes
    /// the call.
    fn takes_any_literal(&self, symbol: SymbolId, arguments: &[StaticType]) -> bool {
        let Some(parameters) = self.overloads().parameters_of(symbol) else {
            return false;
        };
        parameters.len() == arguments.len()
            && parameters
                .iter()
                .zip(arguments)
                .all(|(parameter, argument)| {
                    let unsettled = self.vars.unsettled(argument);
                    if unsettled.iter().all(|&var| !self.vars.is_literal(var)) {
                        return unsettled.is_empty();
                    }
                    match self.vars.resolve(parameter) {
                        StaticType::Var(var) => !self.vars.is_literal(var),
                        StaticType::Generic(_) => true,
                        _ => false,
                    }
                })
    }

    /// Whether one of `reachable` other than `except` takes `arguments`
    /// once each literal among them is `Any`.
    fn fits_once_any(
        &mut self,
        reachable: &[SymbolId],
        arguments: &[StaticType],
        except: Option<SymbolId>,
    ) -> bool {
        let as_any = arguments
            .iter()
            .map(|argument| {
                self.vars.resolve(argument).replace(&|part| match part {
                    StaticType::Var(var) if self.vars.is_literal(*var) => Some(StaticType::Any),
                    _ => None,
                })
            })
            .collect::<Vec<_>>();
        reachable
            .iter()
            .filter(|&&symbol| Some(symbol) != except)
            .any(|&symbol| self.could_accept(symbol, &as_any))
    }

    /// Settles each of `literals` as the first of its defaults with which
    /// `only` still takes `arguments`, and returns whether it could.
    fn decide_literals(
        &mut self,
        literals: &[TypeVar],
        only: SymbolId,
        arguments: &[StaticType],
    ) -> bool {
        for &var in literals {
            // A list of numbers is what the numbers are.
            while let Some(held) = self.vars.held_literal(var) {
                if !self.decide_literals(&[held], only, arguments) {
                    return false;
                }
            }
            if !self.vars.is_literal(var) {
                continue;
            }
            let Some(defaults) = self.vars.literal_defaults(var) else {
                return false;
            };
            let decided = defaults.iter().any(|default| {
                let snapshot = self.vars.snapshot();
                let fits =
                    self.vars.settle_default(var, default) && self.could_accept(only, arguments);
                if !fits {
                    self.vars.rollback(snapshot);
                }
                fits
            });
            if !decided {
                return false;
            }
        }
        true
    }

    /// Whether passing these arguments to `symbol` would make the types of
    /// two literals one, as `++(List<T>, List<T>)` does with `[1]` and
    /// `["a"]`. Each literal first gets its own type, which says better
    /// whether they're the same.
    fn merges_literals(&mut self, symbol: SymbolId, arguments: &[StaticType]) -> bool {
        // A literal in two arguments, as the element of `[0]` is in
        // `[0].flat_map(fn(x) => …)` once `x` has its type, is one literal.
        let literals = arguments
            .iter()
            .flat_map(|argument| self.vars.unsettled(argument))
            .filter(|&var| self.vars.is_literal(var))
            .unique()
            .collect::<Vec<_>>();
        if literals.len() < 2 {
            return false;
        }
        let function = self.vars.resolve(self.types.get_type(symbol));
        let snapshot = self.vars.snapshot();
        let Some((parameters, _)) = self.vars.instantiate_signature(&function) else {
            return false;
        };
        for (parameter, argument) in parameters.iter().zip(arguments) {
            self.vars.bind_argument(parameter, argument);
        }
        let mut became = Vec::new();
        let merged = literals.iter().any(|&var| {
            let StaticType::Var(now) = self.vars.resolve(&StaticType::Var(var)) else {
                return false;
            };
            let merged = became.contains(&now);
            became.push(now);
            merged
        });
        self.vars.rollback(snapshot);
        merged
    }

    /// `binding`, except that a pick left to the program among one function
    /// whose parameters are known is a call of that function. A pick among
    /// one candidate whose parameters aren't known, as a variadic function
    /// or a variable that may not hold a function at all, stays the
    /// program's.
    fn call_of_the_only_one(&self, binding: Binding<SymbolId>) -> Binding<SymbolId> {
        match binding {
            Binding::Dynamic(only)
                if let [Candidate::Scalar(only)] = only[..]
                    && let StaticType::Function {
                        parameters: Some(_),
                        ..
                    } = self.vars.resolve(self.types.get_type(only)) =>
            {
                Binding::Resolved(Candidate::Scalar(only))
            }
            binding => binding,
        }
    }

    /// [`Overloads::resolve_call`] over the types as they're known now.
    pub(super) fn resolve_overloads(
        &mut self,
        candidates: &Candidates,
        arguments: &[StaticType],
        kind: CallKind,
    ) -> ResolvedCall {
        self.overloads().resolve_call(candidates, arguments, kind)
    }

    /// Resolves `call` with the arguments as they are, reporting it when no
    /// overload accepts them.
    fn resolve_call_now(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> StaticType {
        let ResolvedCall {
            binding,
            return_type,
        } = self.resolve_overloads(&call.candidates, arguments, call.kind);
        let binding = self.call_of_the_only_one(binding);
        // `a op= b` with no operator that takes the operands calls an in-place
        // overload if one does, as a set has `-=` but no `-`, which keeps `a`.
        if matches!(binding, Binding::None)
            && let CallSite::OpAssignment(id) = call.site
        {
            let in_place = self.resolver.augmented_candidates(id).in_place.clone();
            let in_place = self
                .resolve_overloads(&in_place, arguments, call.kind)
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
        let typ = self.commit_call(&binding, arguments, return_type);
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
    /// it picked, and returns the call's type. The picked signature gets new
    /// variables for its type parameters, and each argument is passed where
    /// its parameter is expected: `push(xs, 1)` with `push<T>(List<T>, T)`
    /// settles `xs` as a `List<Int>`, and `push(xs, "a")` then doesn't fit.
    /// When the pick is left to the program as it runs, any function may get
    /// the arguments, so what's unknown in them becomes `Any`. Returns `None`
    /// when an argument doesn't fit the picked overload after all, which
    /// settles nothing.
    fn commit_call(
        &mut self,
        binding: &Binding<SymbolId>,
        arguments: &[StaticType],
        return_type: StaticType,
    ) -> Option<StaticType> {
        let picked = match binding {
            Binding::Resolved(Candidate::Scalar(symbol)) => Some(*symbol),
            // An operator on tuples applies one overload to each position,
            // with a scalar operand used at every position.
            Binding::Resolved(Candidate::Vec(symbol)) => {
                let Some(length) = arguments.iter().find_map(|argument| match argument {
                    StaticType::Tuple(elements) => Some(elements.len()),
                    _ => None,
                }) else {
                    return Some(return_type);
                };
                let scalar = Binding::Resolved(Candidate::Scalar(*symbol));
                let mut returns = Vec::with_capacity(length);
                for position in 0..length {
                    let arguments = arguments
                        .iter()
                        .map(|argument| match argument {
                            StaticType::Tuple(elements) => elements[position].clone(),
                            scalar => scalar.clone(),
                        })
                        .collect::<Vec<_>>();
                    returns.push(self.commit_call(&scalar, &arguments, StaticType::Any)?);
                }
                return Some(StaticType::Tuple(returns));
            }
            Binding::Dynamic(_) | Binding::None => None,
        };
        let pending = self.pending.len();
        let signature = picked.map(|symbol| self.instantiate_function(symbol));
        let Some(
            signature @ StaticType::Function {
                parameters: Some(_),
                ..
            },
        ) = signature
        else {
            // A function that takes anything, as `print` does, takes each
            // value as the type it is, as a type parameter would, so it
            // settles nothing. What's passed to an overload the program
            // picks isn't known, but a literal says what it is.
            let variadic = matches!(
                signature,
                Some(StaticType::Function {
                    parameters: None,
                    ..
                })
            );
            if !variadic {
                for argument in arguments {
                    self.vars
                        .settle_unknowns_but_literals(argument, &StaticType::Any);
                }
            }
            return Some(return_type);
        };
        let Some((typ, parameters)) = self.bind_signature(&signature, arguments) else {
            self.pending.truncate(pending);
            return None;
        };
        let iterations = self.iterations_waited_for(&parameters, arguments);
        self.pending.iterations.extend(iterations);
        Some(typ)
    }

    /// `symbol`'s type with new variables for its type parameters. For a
    /// generic function declared in the program, its constraints get the
    /// same variables, and wait in [`Self::pending`] until
    /// [`Self::flush_copies`] knows which call they're for.
    pub(super) fn instantiate_function(&mut self, symbol: SymbolId) -> StaticType {
        let declared = self.vars.resolve(self.types.get_type(symbol));
        let mut made = Vec::new();
        let typ = self.vars.instantiate_with(&declared, &mut made);
        // A copy of a function's own constraint that calls the function
        // again, as a recursive call its body couldn't decide does, is
        // checked by the copies the outer call already made: recursion
        // doesn't change the types.
        let recursive = self
            .copied_through
            .iter()
            .flat_map(|frames| frames.iter())
            .any(|frame| frame.function == symbol);
        let Some(scheme) = self.schemes.get(&symbol).cloned() else {
            return typ;
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
        typ
    }

    /// Adds the constraints [`Self::instantiate_function`] copied for a call
    /// at `call`, and solves the ones it can. A call at `site` gets the
    /// instance its types need; a function's name used as a value, with no
    /// `site`, stays the function itself, so two values of it are equal.
    pub(super) fn flush_copies(&mut self, site: Option<CallSite>, call: Span) {
        match site {
            Some(site) => self.record_uses(site, call),
            None => self.pending.uses.clear(),
        }
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
    /// returns, and its parameters. Returns `None` and settles nothing when
    /// an argument doesn't fit, or the number of arguments is wrong.
    pub(super) fn bind_signature(
        &mut self,
        signature: &StaticType,
        arguments: &[StaticType],
    ) -> Option<(StaticType, Vec<StaticType>)> {
        let StaticType::Function {
            parameters: Some(parameters),
            return_type,
        } = signature
        else {
            unreachable!("only a function with known parameters has a signature to bind");
        };
        let snapshot = self.vars.snapshot();
        if !self.vars.bind_arguments(parameters, arguments) {
            self.vars.rollback(snapshot);
            return None;
        }
        Some((self.vars.resolve(return_type), parameters.clone()))
    }

    /// Each argument nothing is known about yet that was passed for an
    /// `Iterable<T>`, or that a function passed for one returns, with the
    /// `T` that iterating over it gives once it's known: in `fn total(xs)
    /// => xs.sum()`, what `xs` is waits for a call, and so does `xs` in
    /// `fn flatten(xs) => [0].flat_map(fn(_) => xs)`.
    pub(super) fn iterations_waited_for(
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
        let returns = self.vars.fresh(StaticType::Any);
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
        let snapshot = self.vars.snapshot();
        let ResolvedCall {
            binding,
            return_type,
        } = self.resolve_overloads(&call.candidates, arguments, call.kind);
        let fits = !matches!(binding, Binding::None)
            && self.commit_call(&binding, arguments, return_type).is_some();
        self.vars.rollback(snapshot);
        self.pending.clear();
        fits
    }

    /// Commits the waiting call `serial` to the narrowest overload that
    /// fits it, once nothing else can settle its arguments, and returns
    /// whether it did. That's the overload the call would pick if the
    /// arguments' unknown parts were whatever that overload needs: in
    /// `m["k"]` on an empty map, `[](Map<K, V>, K)` is narrower than
    /// `[](Any, Any)`, so the map's keys are Strings. It only commits when
    /// every overload that fits is in the same scope, as a nearer one would
    /// otherwise win, and when each unknown is inside an argument whose kind
    /// is known: an argument nothing at all is known about is `Any`, which
    /// the overloads taking `Any` take.
    pub(super) fn default_to_narrowest(&mut self, serial: u64) -> bool {
        let Some(Waiting {
            constraint: Constraint::Call(call),
            ..
        }) = self.waiting.get(&serial)
        else {
            return false;
        };
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.vars.resolve(argument))
            .collect::<Vec<_>>();
        if arguments
            .iter()
            .any(|argument| matches!(argument, StaticType::Var(_)))
            || Self::operand_could_be_a_tuple(call.kind, &arguments)
        {
            return false;
        }
        let groups = self.overloads().reachable_groups(&call.candidates);
        let mut fitting = Vec::new();
        for group in groups {
            let fits = group
                .into_iter()
                .filter(|&symbol| self.could_accept(symbol, &arguments))
                .collect::<Vec<_>>();
            if !fits.is_empty() {
                fitting.push(fits);
            }
        }
        let [group] = &fitting[..] else {
            return false;
        };
        let Some(narrowest) = self
            .most_specific(group)
            .filter(|&narrowest| self.fits_without_any(narrowest, &arguments))
        else {
            return false;
        };
        let Some(Waiting {
            constraint: Constraint::Call(call),
            origin,
        }) = self.waiting.remove(&serial)
        else {
            unreachable!("the call was waiting a moment ago");
        };
        let outer = std::mem::replace(&mut self.copied_through, origin.clone());
        let committed = self.commit_early(&call, &arguments, narrowest);
        if let Some(typ) = &committed {
            let result = call.result.clone();
            self.check_write(&result, typ, call.span);
        }
        self.copied_through = outer;
        if committed.is_none() {
            self.wait_again(
                serial,
                Waiting {
                    constraint: Constraint::Call(call),
                    origin,
                },
            );
            return false;
        }
        true
    }

    /// The one of `symbols` whose parameters are narrower than every other
    /// one's, as `[](Map<K, V>, K)` is than `[](Any, Any)`, if there is one.
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

    /// Whether `symbol` is a function whose parameters take these arguments
    /// as the types they are. An `Any` parameter standing in for an argument
    /// of another type is a fallback that only wins over the overloads
    /// taking exact types once the arguments are known, so a waiting call
    /// doesn't commit to it: `xs[0] = []` on a `List<Int>` leaves only
    /// `[]=(List<T>, Any, Any)` taking the empty list, but it's still an
    /// error once the list's type is known. An `Iterable<T>` parameter takes
    /// any kind of sequence, so it can't tell what an argument nothing is
    /// known about is, and the call waits for it as `for x in xs` does.
    fn fits_without_any(&self, symbol: SymbolId, arguments: &[StaticType]) -> bool {
        self.overloads()
            .parameters_of(symbol)
            .is_some_and(|parameters| {
                parameters
                    .iter()
                    .zip(arguments)
                    .all(|(parameter, argument)| match (parameter, argument) {
                        (StaticType::Any, argument) => *argument == StaticType::Any,
                        (StaticType::Iterable(_), argument) => {
                            !self.vars.waits_to_iterate(argument)
                        }
                        _ => true,
                    })
            })
    }

    /// Whether `symbol` can accept arguments of these types, where each part
    /// not known yet fits anything.
    fn could_accept(&mut self, symbol: SymbolId, arguments: &[StaticType]) -> bool {
        let function = self.vars.resolve(self.types.get_type(symbol));
        let snapshot = self.vars.snapshot();
        let Some((parameters, _)) = self.vars.instantiate_signature(&function) else {
            return matches!(function, StaticType::Function { .. }) || function.is_unknown();
        };
        let fits = self.vars.bind_arguments(&parameters, arguments);
        self.vars.rollback(snapshot);
        fits
    }

    fn site_candidates(&self, site: CallSite) -> &Candidates {
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
                let in_place = self.overloads().resolve_call(
                    &self.resolver.augmented_candidates(id).in_place,
                    &arguments,
                    CallKind::Operator,
                );
                let compatible = Self::augmented_rhs_is_compatible(&arguments[0], &arguments[1]);
                let binding = match (in_place.binding, binding) {
                    (Binding::Resolved(candidate), _) => Binding::Resolved(candidate),
                    // Which one applies is left to the program as it runs,
                    // which mutates in place where it can.
                    (Binding::Dynamic(in_place), operators) if compatible => {
                        Self::in_place_then_operators(in_place, operators)
                    }
                    (_, binding) => binding,
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
                self.no_match(&call.candidates, &call.name, arguments, call.span)
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
