use super::Analyser;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads, ResolvedCall};
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation};
use ndc_resolver::{Candidates, SymbolId};

impl Analyser {
    /// Why no candidate accepts a call of `name` with arguments of these types:
    /// a variable ended the search, a cast would make an overload fit, or
    /// nothing fits.
    fn no_match(
        &self,
        candidates: &Candidates,
        name: &str,
        types: &[StaticType],
        span: Span,
    ) -> AnalysisError {
        let overloads = self.overloads();
        if let Some(variable) = overloads.variable_ending_search(candidates) {
            let hides_functions = overloads.hides_functions(candidates);
            AnalysisError::variable_rejects_arguments(
                name,
                self.types.get_type(variable),
                types,
                hides_functions,
                span,
            )
        } else if overloads.a_cast_could_resolve(candidates, types) {
            AnalysisError::function_not_found_suggest_cast(name, types, span)
        } else {
            AnalysisError::function_not_found(name, types, span)
        }
    }

    /// Settles the types a call's arguments leave unknown from the overload
    /// it picked, and returns the call's type. The picked signature gets new
    /// variables for its type parameters, and each argument is passed where
    /// its parameter is expected: `push(xs, 1)` with `push<T>(List<T>, T)`
    /// settles `xs` as a `List<Int>`, and `push(xs, "a")` then doesn't fit.
    /// When the pick is left to the program as it runs, any function may get
    /// the arguments, so what's unknown in them becomes `Any`.
    pub(super) fn commit_call(
        &mut self,
        binding: &Binding<SymbolId>,
        arguments: &[StaticType],
        return_type: StaticType,
        name: &str,
        span: Span,
    ) -> StaticType {
        let picked = match binding {
            Binding::Resolved(Candidate::Scalar(symbol)) => Some(*symbol),
            // Element-wise operators on tuples leave the types they have.
            Binding::Resolved(Candidate::Vec(_)) => return return_type,
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
            return return_type;
        };

        let mut fits = true;
        for (parameter, argument) in parameters.iter().zip(arguments) {
            fits &= self.vars.bind_argument(parameter, argument);
        }
        if !fits {
            let arguments = arguments
                .iter()
                .map(|argument| self.vars.resolve(argument))
                .collect::<Vec<_>>();
            self.emit(AnalysisError::function_not_found(name, &arguments, span));
        }
        self.vars.resolve(&declared_return)
    }

    /// Overload resolution over the types declared so far.
    pub(super) fn overloads(&self) -> Overloads<'_> {
        Overloads::new(&self.types, &self.vars, self.resolver.symbols())
    }

    /// Analyses `receiver.member`, the call `member(receiver)`.
    pub(super) fn analyse_member_access(
        &mut self,
        id: NodeId,
        receiver: &ExpressionLocation,
        member: &str,
        member_span: Span,
    ) -> StaticType {
        // `xs.len` is the call `len(xs)`, among functions only.
        let receiver_type = self.analyse_expr(receiver);
        let getters = self
            .resolver
            .getter_candidates(id)
            .expect("internal error: member access without getter candidates");
        let ResolvedCall {
            binding,
            return_type,
        } = self.overloads().resolve_call(
            getters,
            std::slice::from_ref(&receiver_type),
            CallKind::Regular,
        );

        if matches!(binding, Binding::None) {
            let error = self.no_match(
                getters,
                member,
                std::slice::from_ref(&receiver_type),
                member_span,
            );
            self.emit(error);
        }

        let return_type = self.commit_call(
            &binding,
            std::slice::from_ref(&receiver_type),
            return_type,
            member,
            member_span,
        );
        self.picks.getters.insert(id, binding);
        return_type
    }

    /// Resolves a call (regular or operator-form) and returns its result type.
    /// Only operator-form calls are eligible for vec dispatch.
    pub(super) fn analyse_call(
        &mut self,
        function: &ExpressionLocation,
        arguments: &[ExpressionLocation],
        kind: CallKind,
        span: Span,
    ) -> StaticType {
        // Higher-order call shapes like `get_function()()` have a non-identifier
        // function position; in that case we just analyse the callee as a value
        // and trust the runtime to dispatch. The callee comes before the
        // arguments, in source order like the resolver, so declarations in it
        // get their slots first.
        let Expression::Identifier { name, .. } = &function.expression else {
            let callee_type = self.analyse_expr(function);
            let arguments = arguments
                .iter()
                .map(|arg| self.analyse_expr(arg))
                .collect::<Vec<_>>();
            return match callee_type {
                // The callee's type says what it takes, like a picked overload's.
                StaticType::Function {
                    parameters: Some(parameters),
                    return_type,
                } if parameters.len() == arguments.len() => {
                    let mut fits = true;
                    for (parameter, argument) in parameters.iter().zip(&arguments) {
                        fits &= self.vars.bind_argument(parameter, argument);
                    }
                    if !fits {
                        let arguments = arguments
                            .iter()
                            .map(|argument| self.vars.resolve(argument))
                            .collect::<Vec<_>>();
                        let callee_type = StaticType::Function {
                            parameters: Some(parameters),
                            return_type: return_type.clone(),
                        };
                        self.emit(AnalysisError::callee_rejects_arguments(
                            &self.vars.resolve(&callee_type),
                            &arguments,
                            span,
                        ));
                    }
                    self.vars.resolve(&return_type)
                }
                // Nothing tells what the callee does with its arguments.
                callee if callee.could_be_callable() => {
                    for argument in &arguments {
                        self.vars.settle_unknowns(argument, &StaticType::Any);
                    }
                    match callee {
                        StaticType::Function { return_type, .. } => *return_type,
                        _ => StaticType::Any,
                    }
                }
                other => {
                    self.emit(AnalysisError::not_callable(&other, span));
                    StaticType::Any
                }
            };
        };

        // A named callee declares nothing, so its overload can be picked after
        // the arguments' types are known.
        let mut type_sig = Vec::with_capacity(arguments.len());
        for arg in arguments.iter() {
            type_sig.push(self.analyse_expr(arg));
        }

        let ResolvedCall {
            binding,
            return_type,
        } = self.overloads().resolve_call(
            self.resolver.reference_candidates(function.id),
            &type_sig,
            kind,
        );

        if matches!(binding, Binding::None) {
            let candidates = self.resolver.reference_candidates(function.id);
            let error = self.no_match(candidates, name, &type_sig, span);
            self.emit(error);
            self.picks.bindings.insert(function.id, binding);
            return StaticType::Any;
        }

        let return_type = self.commit_call(&binding, &type_sig, return_type, name, span);
        self.picks.bindings.insert(function.id, binding);
        return_type
    }
}
