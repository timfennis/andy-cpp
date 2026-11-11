//! Calls and other uses whose types aren't known yet wait here until they
//! are. In `xs.map(fn(x) => x + 1)`, the `+` waits for `x`'s type, which
//! picking `map` settles from the type of `xs`.

use super::Analyser;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall, is_strictly_more_specific};
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_resolver::{Candidates, SymbolId};
use std::collections::BTreeSet;

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
#[derive(Debug)]
pub(super) enum Constraint {
    /// A call whose overload can't be picked yet.
    Call(CallConstraint),
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
    /// The one type the two branches of an `if` have, once both are known.
    Join {
        sides: [StaticType; 2],
        result: StaticType,
        span: Span,
    },
}

impl Constraint {
    /// The types whose variables the constraint waits on.
    fn inputs(&self) -> &[StaticType] {
        match self {
            Self::Call(call) => &call.arguments,
            Self::Iterate { sequence, .. } => std::slice::from_ref(sequence),
            Self::Unpack { value, .. } => std::slice::from_ref(value),
            Self::Join { sides, .. } => sides,
        }
    }
}

#[derive(Debug)]
pub(super) struct CallConstraint {
    site: CallSite,
    candidates: Candidates,
    arguments: Vec<StaticType>,
    /// The call's type, which the picked overload's return type settles.
    result: StaticType,
    kind: CallKind,
    name: String,
    span: Span,
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
        let mut call = CallConstraint {
            candidates: self.site_candidates(site).clone(),
            site,
            arguments,
            result: StaticType::Never,
            kind,
            name: name.to_string(),
            span,
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
        match self.vars.resolve(&sequence) {
            StaticType::Var(_) => {
                let element = self.vars.fresh(StaticType::Any);
                self.wait(Constraint::Iterate {
                    sequence,
                    element: element.clone(),
                    span,
                });
                element
            }
            known => known.sequence_element_type().unwrap_or(StaticType::Any),
        }
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
    /// branches of an `if`: their least upper bound. Where a part of either
    /// isn't known yet, that part waits until it is, so a lambda's parameter
    /// isn't fixed by the branch next to it: in `fn(x) => if c { x } else
    /// { 0 }`, `x` can still be a `Float`. Parts both sides share are joined
    /// part by part, so `if c { [] } else { [1] }` is a list either way.
    pub(super) fn join(&mut self, a: &StaticType, b: &StaticType, span: Span) -> StaticType {
        let (a, b) = (self.vars.resolve(a), self.vars.resolve(b));
        match (&a, &b) {
            (StaticType::Never, _) => b,
            (_, StaticType::Never) => a,
            _ if a == b => a,
            _ if !a.has_vars() && !b.has_vars() => a.lub(&b),
            (StaticType::Option(x), StaticType::Option(y)) => {
                StaticType::Option(Box::new(self.join(x, y, span)))
            }
            (StaticType::List(x), StaticType::List(y)) => {
                StaticType::List(Box::new(self.join(x, y, span)))
            }
            (StaticType::Iterator(x), StaticType::Iterator(y)) => {
                StaticType::Iterator(Box::new(self.join(x, y, span)))
            }
            (StaticType::Deque(x), StaticType::Deque(y)) => {
                StaticType::Deque(Box::new(self.join(x, y, span)))
            }
            (StaticType::MinHeap(x), StaticType::MinHeap(y)) => {
                StaticType::MinHeap(Box::new(self.join(x, y, span)))
            }
            (StaticType::MaxHeap(x), StaticType::MaxHeap(y)) => {
                StaticType::MaxHeap(Box::new(self.join(x, y, span)))
            }
            (StaticType::Map { key: k1, value: v1 }, StaticType::Map { key: k2, value: v2 }) => {
                StaticType::Map {
                    key: Box::new(self.join(k1, k2, span)),
                    value: Box::new(self.join(v1, v2, span)),
                }
            }
            (StaticType::Tuple(xs), StaticType::Tuple(ys)) if xs.len() == ys.len() => {
                StaticType::Tuple(
                    xs.iter()
                        .zip(ys)
                        .map(|(x, y)| self.join(x, y, span))
                        .collect(),
                )
            }
            _ => {
                let result = self.vars.fresh(StaticType::Any);
                self.wait(Constraint::Join {
                    sides: [a, b],
                    result: result.clone(),
                    span,
                });
                result
            }
        }
    }

    /// Makes `constraint` wait, in the order it started waiting, until one
    /// of the variables it waits on is settled.
    fn wait(&mut self, constraint: Constraint) {
        let serial = self.next_serial;
        self.next_serial += 1;
        self.wait_again(serial, constraint);
    }

    fn wait_again(&mut self, serial: u64, constraint: Constraint) {
        for var in constraint
            .inputs()
            .iter()
            .flat_map(|input| self.vars.unsettled(input))
        {
            self.watchers.entry(var).or_default().push(serial);
        }
        self.waiting.insert(serial, constraint);
    }

    /// Tries again each waiting constraint that waits on a variable settled
    /// since this last ran, oldest first, until that settles nothing more.
    pub(super) fn solve_waiting(&mut self) {
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
                let Some(mut constraint) = self.waiting.remove(&serial) else {
                    continue;
                };
                if !self.try_constraint(&mut constraint) {
                    self.wait_again(serial, constraint);
                }
            }
        }
        self.watched_settlings = self.vars.settled_count();
    }

    /// Solves what waits on the variables made since `first`, which nothing
    /// outside the code they were made in can settle: each one still
    /// unsettled gets its fallback, oldest constraint first, and the rest are
    /// settled the same way. Constraints that wait only on older variables
    /// keep waiting.
    pub(super) fn settle_waiting(&mut self, first: usize) {
        // Settling only ever makes more known, so a constraint found to wait
        // on no variable made since `first` never needs looking at again.
        let mut checked = 0;
        loop {
            self.solve_waiting();
            let next = self
                .waiting
                .range(checked..)
                .find_map(|(&serial, constraint)| {
                    let vars = constraint
                        .inputs()
                        .iter()
                        .flat_map(|input| self.vars.unsettled(input))
                        .filter(|var| var.0 as usize >= first)
                        .collect::<Vec<_>>();
                    (!vars.is_empty()).then_some((serial, vars))
                });
            let Some((serial, vars)) = next else {
                break;
            };
            checked = serial;
            // What nothing else settled in `a op= b` keeps the type it
            // already had, so it's taken from `b`: in `m["k"] += 3` on an
            // empty map, the values are Ints.
            if let Some(Constraint::Call(call)) = self.waiting.get(&serial)
                && let CallSite::OpAssignment(_) = call.site
                && let [target, operand] = &call.arguments[..]
            {
                let (target, operand) = (target.clone(), operand.clone());
                self.vars.try_unify(&target, &operand);
            }
            self.vars.settle_with_fallbacks(&vars);
        }
        self.vars.settle_from(first);
        self.solve_waiting();
        // A call can still wait when its arguments are known: a variable's
        // type might have settled since, which nothing watches.
        let serials = self.waiting.keys().copied().collect::<Vec<_>>();
        for serial in serials {
            let Some(mut constraint) = self.waiting.remove(&serial) else {
                continue;
            };
            if !self.try_constraint(&mut constraint) {
                self.wait_again(serial, constraint);
            }
        }
        self.solve_waiting();
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
                if !self.settle_waited_for(&result, &typ, call.span)
                    && let CallSite::Getter(id) = call.site
                {
                    self.failed_reads.insert(id);
                }
                true
            }
            Constraint::Iterate {
                sequence,
                element,
                span,
            } => {
                let sequence = self.vars.resolve(sequence);
                if matches!(sequence, StaticType::Var(_)) {
                    return false;
                }
                let found = sequence.sequence_element_type().unwrap_or(StaticType::Any);
                let element = element.clone();
                self.settle_waited_for(&element, &found, *span);
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
                let found = found.unwrap_or_else(|| vec![StaticType::Any; parts.len()]);
                for (part, found) in parts.iter().zip(found) {
                    self.settle_waited_for(part, &found, *span);
                }
                true
            }
            Constraint::Join {
                sides: [a, b],
                result,
                span,
            } => {
                let (a, b) = (self.vars.resolve(a), self.vars.resolve(b));
                if a.has_vars() || b.has_vars() {
                    return false;
                }
                let result = result.clone();
                self.settle_waited_for(&result, &a.lub(&b), *span);
                true
            }
        }
    }

    /// Settles `waited`, the type a constraint gave out while it waited, as
    /// `found`, the type it turned out to be. Code analysed meanwhile may
    /// have settled `waited` already, and then `found` has to fit it.
    /// Returns whether it does.
    fn settle_waited_for(&mut self, waited: &StaticType, found: &StaticType, span: Span) -> bool {
        let fits = self.accepts_write(waited, found);
        if !fits {
            self.emit_rejected_write(waited, found, span);
        }
        fits
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
        let reachable = self.overloads().reachable_symbols(&call.candidates);

        // A variable whose type isn't known yet, like the parameter `f` in
        // `fn(f) => f(1)`, holds a function that takes these arguments.
        if let [only] = reachable[..]
            && let callee @ StaticType::Var(_) = self.vars.resolve(self.types.get_type(only))
        {
            let typ = self.call_unknown_function(&callee, arguments);
            self.record_pick(
                call.site,
                Binding::Resolved(Candidate::Scalar(only)),
                &call.arguments,
            );
            return Some(typ);
        }

        // A write waits for the value it stores, which is analysed after the
        // target: only then can it tell whether the value is what's wrong.
        if let (CallSite::Setter(_) | CallSite::FieldSetter(_), Some(StaticType::Var(_))) =
            (call.site, arguments.last())
        {
            return None;
        }

        if arguments.iter().all(|argument| !argument.has_vars()) {
            return Some(self.resolve_call_now(call, &arguments));
        }

        let viable = reachable
            .iter()
            .copied()
            .filter(|&symbol| self.could_accept(symbol, &arguments))
            .collect::<Vec<_>>();
        // An overload taking `Any` where another takes something narrower
        // loses to it, as it would once the arguments are known.
        let viable = match self.most_specific(&viable) {
            Some(symbol) => vec![symbol],
            None => viable,
        };
        match viable[..] {
            [] => Some(self.resolve_call_now(call, &arguments)),
            [only]
                if matches!(
                    self.vars.resolve(self.types.get_type(only)),
                    StaticType::Function {
                        parameters: Some(_),
                        ..
                    }
                ) =>
            {
                let binding = Binding::Resolved(Candidate::Scalar(only));
                let typ = self.commit_call(&binding, &arguments, StaticType::Any);
                self.record_pick(call.site, binding, &arguments);
                Some(typ.unwrap_or_else(|| {
                    self.report_failed_call(call, &arguments);
                    StaticType::Any
                }))
            }
            _ => None,
        }
    }

    /// Resolves `call` with the arguments as they are, reporting it when no
    /// overload accepts them.
    fn resolve_call_now(&mut self, call: &CallConstraint, arguments: &[StaticType]) -> StaticType {
        let ResolvedCall {
            binding,
            return_type,
        } = self
            .overloads()
            .resolve_call(&call.candidates, arguments, call.kind);
        if matches!(binding, Binding::None) {
            self.report_failed_call(call, arguments);
            self.record_pick(call.site, binding, arguments);
            return StaticType::Any;
        }
        let typ = self.commit_call(&binding, arguments, return_type);
        self.record_pick(call.site, binding, arguments);
        typ.unwrap_or_else(|| {
            self.report_failed_call(call, arguments);
            StaticType::Any
        })
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
        let signature = picked.map(|symbol| {
            let declared = self.vars.resolve(self.types.get_type(symbol));
            self.vars.instantiate(&declared)
        });
        let Some(StaticType::Function {
            parameters: Some(parameters),
            return_type: declared_return,
        }) = signature
        else {
            for argument in arguments {
                self.vars.settle_unknowns(argument, &StaticType::Any);
            }
            return Some(return_type);
        };

        self.bind_signature(
            &StaticType::Function {
                parameters: Some(parameters),
                return_type: declared_return,
            },
            arguments,
        )
    }

    /// Passes each argument where `signature` expects its parameter, which
    /// settles what's unknown in either, and returns what the signature
    /// returns. Returns `None` and settles nothing when an argument doesn't
    /// fit, or the number of arguments is wrong.
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
        let snapshot = self.vars.snapshot();
        let fits = parameters.len() == arguments.len()
            && parameters
                .iter()
                .zip(arguments)
                .all(|(parameter, argument)| self.vars.bind_argument(parameter, argument));
        if !fits {
            self.vars.rollback(snapshot);
            return None;
        }
        Some(self.vars.resolve(return_type))
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
        } = self
            .overloads()
            .resolve_call(&call.candidates, arguments, call.kind);
        let fits = !matches!(binding, Binding::None)
            && self.commit_call(&binding, arguments, return_type).is_some();
        self.vars.rollback(snapshot);
        fits
    }

    /// The one of `symbols` whose parameters are narrower than every other
    /// one's, as `[](Map<K, V>, K)` is than `[](Any, Any)`, if there is one.
    fn most_specific(&self, symbols: &[SymbolId]) -> Option<SymbolId> {
        let parameters = symbols
            .iter()
            .map(
                |&symbol| match self.vars.resolve(self.types.get_type(symbol)) {
                    StaticType::Function {
                        parameters: Some(parameters),
                        ..
                    } => Some(parameters),
                    _ => None,
                },
            )
            .collect::<Option<Vec<_>>>()?;
        let (narrowest, _) = parameters.iter().enumerate().find(|&(i, narrower)| {
            parameters
                .iter()
                .enumerate()
                .all(|(j, wider)| i == j || is_strictly_more_specific(narrower, wider))
        })?;
        (symbols.len() > 1).then(|| symbols[narrowest])
    }

    /// Whether `symbol` can accept arguments of these types, where each part
    /// not known yet fits anything.
    fn could_accept(&mut self, symbol: SymbolId, arguments: &[StaticType]) -> bool {
        match self.vars.resolve(self.types.get_type(symbol)) {
            StaticType::Function {
                parameters: None, ..
            } => true,
            function @ StaticType::Function {
                parameters: Some(_),
                ..
            } => {
                let snapshot = self.vars.snapshot();
                let StaticType::Function {
                    parameters: Some(parameters),
                    ..
                } = self.vars.instantiate(&function)
                else {
                    unreachable!("instantiating a function type gives a function type");
                };
                let fits = parameters.len() == arguments.len()
                    && parameters
                        .iter()
                        .zip(arguments)
                        .all(|(parameter, argument)| self.vars.bind_argument(parameter, argument));
                self.vars.rollback(snapshot);
                fits
            }
            other => other.is_unknown(),
        }
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

    fn record_pick(
        &mut self,
        site: CallSite,
        binding: Binding<SymbolId>,
        arguments: &[StaticType],
    ) {
        match site {
            CallSite::Call(id) => self.picks.bindings.insert(id, binding),
            CallSite::Getter(id) => self.picks.getters.insert(id, binding),
            CallSite::Setter(id) | CallSite::FieldSetter(id) => {
                self.picks.setters.insert(id, binding)
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
                let binding = match in_place.binding {
                    Binding::Resolved(candidate)
                        if Self::augmented_rhs_is_compatible(&arguments[0], &arguments[1]) =>
                    {
                        Binding::Resolved(candidate)
                    }
                    _ => binding,
                };
                self.picks.augmented_assignments.insert(id, binding)
            }
        };
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
