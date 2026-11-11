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
    ///
    /// `lambdas` are the arguments analysed only now, by their position,
    /// with [`Self::lambda_stand_in`] in `arguments` so far. Each is analysed
    /// after the other arguments are bound, with the function type its
    /// parameter expects: in `xs.map(fn(x) => x + 1)`, `x` gets the
    /// elements' type. Their types replace the stand-ins in `arguments`.
    pub(super) fn commit_call(
        &mut self,
        binding: &Binding<SymbolId>,
        arguments: &mut [StaticType],
        lambdas: &[(usize, &ExpressionLocation)],
        return_type: StaticType,
        name: &str,
        span: Span,
    ) -> StaticType {
        let picked = match binding {
            Binding::Resolved(Candidate::Scalar(symbol)) => Some(*symbol),
            Binding::Resolved(Candidate::Vec(_)) | Binding::Dynamic(_) | Binding::None => None,
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
            for &(position, lambda) in lambdas {
                arguments[position] = self.analyse_expr(lambda);
            }
            // Element-wise operators on tuples leave the types they have.
            if !matches!(binding, Binding::Resolved(Candidate::Vec(_))) {
                for argument in arguments.iter() {
                    self.vars.settle_unknowns(argument, &StaticType::Any);
                }
            }
            return return_type;
        };

        let mut fits = true;
        for (position, (parameter, argument)) in parameters.iter().zip(arguments.iter()).enumerate()
        {
            if !lambdas.iter().any(|&(lambda, _)| lambda == position) {
                fits &= self.vars.bind_argument(parameter, argument);
            }
        }
        for &(position, lambda) in lambdas {
            let expected = self.vars.resolve(&parameters[position]);
            let lambda_type = self.analyse_with_expected(lambda, &expected);
            fits &= self.vars.bind_argument(&parameters[position], &lambda_type);
            arguments[position] = lambda_type;
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

    /// What a lambda argument stands in as while its call picks an overload,
    /// before the lambda itself is analysed: a function of its arity that fits
    /// any function type, as it takes what its annotations say or anything,
    /// and returns `Never`. `None` for any other argument.
    fn lambda_stand_in(&mut self, argument: &ExpressionLocation) -> Option<StaticType> {
        let Expression::FunctionDeclaration {
            name: None,
            parameters,
            ..
        } = &argument.expression
        else {
            return None;
        };
        // The lambda's analysis reports a bad annotation, so this doesn't.
        let errors = self.result.errors.len();
        let parameters = parameters
            .iter()
            .map(|parameter| {
                self.lower_annotation(parameter.annotation.as_ref())
                    .unwrap_or(StaticType::Any)
            })
            .collect();
        self.result.errors.truncate(errors);
        Some(StaticType::Function {
            parameters: Some(parameters),
            return_type: Box::new(StaticType::Never),
        })
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
            &mut [receiver_type],
            &[],
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
            // The callee's type says what each argument is expected to be,
            // which tells a lambda argument what its parameters are.
            let expected = match &callee_type {
                StaticType::Function {
                    parameters: Some(parameters),
                    ..
                } if parameters.len() == arguments.len() => Some(parameters.clone()),
                _ => None,
            };
            let arguments = arguments
                .iter()
                .enumerate()
                .map(|(position, arg)| match &expected {
                    Some(expected) => self.analyse_with_expected(arg, &expected[position]),
                    None => self.analyse_expr(arg),
                })
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
        // Lambdas wait until the overload is picked, which tells them what
        // their parameters are.
        let mut type_sig = Vec::with_capacity(arguments.len());
        let mut lambdas = Vec::new();
        for (position, arg) in arguments.iter().enumerate() {
            if let Some(stand_in) = self.lambda_stand_in(arg) {
                lambdas.push((position, arg));
                type_sig.push(stand_in);
            } else {
                type_sig.push(self.analyse_expr(arg));
            }
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
            for &(position, lambda) in &lambdas {
                type_sig[position] = self.analyse_expr(lambda);
            }
            let candidates = self.resolver.reference_candidates(function.id);
            let error = self.no_match(candidates, name, &type_sig, span);
            self.emit(error);
            self.picks.bindings.insert(function.id, binding);
            return StaticType::Any;
        }

        let return_type =
            self.commit_call(&binding, &mut type_sig, &lambdas, return_type, name, span);
        self.picks.bindings.insert(function.id, binding);
        return_type
    }
}
