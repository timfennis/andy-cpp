//! Calls and other uses whose types aren't known yet wait here until they
//! are. In `xs.map(fn(x) => x + 1)`, the `+` waits for `x`'s type, which
//! picking `map` settles from the type of `xs`.

use super::Analyser;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall};
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_resolver::{Candidates, SymbolId};

/// Where a call's candidates come from and where its pick goes.
#[derive(Debug, Clone, Copy)]
pub(super) enum CallSite {
    /// A call or an operator: the overloads the callee's name can mean.
    Call(NodeId),
    /// `x.f`, or reading the old value in `xs[i] += 1`.
    Getter(NodeId),
    /// The write in `xs[i] = v`.
    Setter(NodeId),
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
    /// The arguments when the call last couldn't be picked, so it's tried
    /// again only once they've changed.
    tried: Option<Vec<StaticType>>,
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
            tried: None,
        };
        if let Some(typ) = self.try_call(&mut call) {
            return typ;
        }
        call.result = self.vars.fresh(StaticType::Any);
        let result = call.result.clone();
        self.waiting.push(Constraint::Call(call));
        result
    }

    /// What iterating over a value of type `sequence` gives, which waits
    /// until the kind of sequence is known.
    pub(super) fn constrain_iteration(&mut self, sequence: StaticType, span: Span) -> StaticType {
        match self.vars.resolve(&sequence) {
            StaticType::Var(_) => {
                let element = self.vars.fresh(StaticType::Any);
                self.waiting.push(Constraint::Iterate {
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
        self.waiting.push(Constraint::Unpack {
            value,
            parts: parts.clone(),
            span,
        });
        parts
    }

    /// Solves every waiting constraint that can be solved now, until solving
    /// one doesn't make another solvable.
    pub(super) fn solve_waiting(&mut self) {
        if self.vars.generation() == self.solved_generation {
            return;
        }
        loop {
            let mut progress = false;
            let mut index = 0;
            while index < self.waiting.len() {
                let mut constraint = self.waiting.remove(index);
                if self.try_constraint(&mut constraint) {
                    progress = true;
                } else {
                    self.waiting.insert(index, constraint);
                    index += 1;
                }
            }
            if !progress {
                break;
            }
        }
        self.solved_generation = self.vars.generation();
    }

    /// Solves what waits on the variables made since `first`, which nothing
    /// outside the code they were made in can settle: each one still
    /// unsettled gets its fallback, oldest constraint first, and the rest are
    /// settled the same way. Constraints that wait only on older variables
    /// keep waiting.
    pub(super) fn settle_waiting(&mut self, first: usize) {
        loop {
            self.solve_waiting();
            let next = self.waiting.iter().find_map(|constraint| {
                let inputs: &[StaticType] = match constraint {
                    Constraint::Call(call) => &call.arguments,
                    Constraint::Iterate { sequence, .. } => std::slice::from_ref(sequence),
                    Constraint::Unpack { value, .. } => std::slice::from_ref(value),
                };
                let vars = inputs
                    .iter()
                    .flat_map(|input| self.vars.unsettled(input))
                    .filter(|var| var.0 as usize >= first)
                    .collect::<Vec<_>>();
                (!vars.is_empty()).then_some(vars)
            });
            let Some(vars) = next else {
                break;
            };
            self.vars.settle_with_fallbacks(&vars);
        }
        self.vars.settle_from(first);
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
                self.settle_waited_for(&result, &typ, call.span);
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
        }
    }

    /// Settles `waited`, the type a constraint gave out while it waited, as
    /// `found`, the type it turned out to be. Code analysed meanwhile may
    /// have settled `waited` already, and then `found` has to fit it.
    fn settle_waited_for(&mut self, waited: &StaticType, found: &StaticType, span: Span) {
        if !self.accepts_write(waited, found) {
            self.emit_rejected_write(waited, found, span);
        }
    }

    /// Picks the overload `call` means, records it, and returns the call's
    /// type, or `None` when the arguments aren't known well enough yet. A
    /// call whose arguments are known is resolved the usual way. Otherwise
    /// the candidates that can't accept what's known are crossed off, and
    /// when one is left, the call commits to it, which settles the arguments
    /// from its parameters.
    fn try_call(&mut self, call: &mut CallConstraint) -> Option<StaticType> {
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.vars.resolve(argument))
            .collect::<Vec<_>>();
        if call.tried.as_ref() == Some(&arguments) {
            return None;
        }
        let reachable = self.overloads().reachable_symbols(&call.candidates);

        // A variable whose type isn't known yet, like the parameter `f` in
        // `fn(f) => f(1)`, holds a function that takes these arguments.
        if let [only] = reachable[..]
            && let callee @ StaticType::Var(_) = self.vars.resolve(self.types.get_type(only))
        {
            let returns = self.vars.fresh(StaticType::Any);
            self.vars.unify(
                &callee,
                &StaticType::Function {
                    parameters: Some(arguments),
                    return_type: Box::new(returns.clone()),
                },
            );
            self.record_pick(call.site, Binding::Resolved(Candidate::Scalar(only)));
            return Some(self.vars.resolve(&returns));
        }

        // A write waits for the value it stores, which is analysed after the
        // target: only then can it tell whether the value is what's wrong.
        if let (CallSite::Setter(_), Some(StaticType::Var(_))) = (call.site, arguments.last()) {
            call.tried = Some(arguments);
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
        match viable[..] {
            [] => Some(self.resolve_call_now(call, &arguments)),
            [only]
                if matches!(
                    self.types.get_type(only),
                    StaticType::Function {
                        parameters: Some(_),
                        ..
                    }
                ) =>
            {
                let binding = Binding::Resolved(Candidate::Scalar(only));
                let typ = self.commit_call(&binding, &arguments, StaticType::Any);
                self.record_pick(call.site, binding);
                Some(typ.unwrap_or_else(|| {
                    self.report_failed_call(call, &arguments);
                    StaticType::Any
                }))
            }
            _ => {
                call.tried = Some(arguments);
                None
            }
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
            self.record_pick(call.site, binding);
            return StaticType::Any;
        }
        let typ = self.commit_call(&binding, arguments, return_type);
        self.record_pick(call.site, binding);
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
            // Element-wise operators on tuples leave the types they have.
            Binding::Resolved(Candidate::Vec(_)) => return Some(return_type),
            Binding::Dynamic(_) | Binding::None => None,
        };
        let signature = picked.map(|symbol| {
            let declared = self.types.get_type(symbol).clone();
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

        let snapshot = self.vars.snapshot();
        let fits = parameters
            .iter()
            .zip(arguments)
            .all(|(parameter, argument)| self.vars.bind_argument(parameter, argument));
        if !fits {
            self.vars.rollback(snapshot);
            return None;
        }
        Some(self.vars.resolve(&declared_return))
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
            CallSite::Setter(id) => self.resolver.setter_candidates(id),
        }
    }

    fn record_pick(&mut self, site: CallSite, binding: Binding<SymbolId>) {
        match site {
            CallSite::Call(id) => self.picks.bindings.insert(id, binding),
            CallSite::Getter(id) => self.picks.getters.insert(id, binding),
            CallSite::Setter(id) => self.picks.setters.insert(id, binding),
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
            CallSite::Call(_) | CallSite::Getter(_) => {
                if let CallSite::Getter(id) = call.site {
                    self.failed_reads.insert(id);
                }
                self.no_match(&call.candidates, &call.name, arguments, call.span)
            }
            CallSite::Setter(id) if self.failed_reads.contains(&id) => return,
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
