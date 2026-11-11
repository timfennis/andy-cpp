use super::Analyser;
use super::solver::CallSite;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation};
use ndc_resolver::Candidates;

impl Analyser {
    /// Why no candidate accepts a call of `name` with arguments of these types:
    /// a variable ended the search, a cast would make an overload fit, or
    /// nothing fits.
    pub(super) fn no_match(
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
        self.constrain_call(
            CallSite::Getter(id),
            vec![receiver_type],
            CallKind::Regular,
            member,
            member_span,
        )
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
        // function position, so the callee is analysed as a value. The callee
        // comes before the arguments, in source order like the resolver, so
        // declarations in it get their slots first.
        let Expression::Identifier { name, .. } = &function.expression else {
            let callee_type = self.analyse_expr(function);
            let arguments = arguments
                .iter()
                .map(|arg| self.analyse_expr(arg))
                .collect::<Vec<_>>();
            return match self.vars.resolve(&callee_type) {
                // The callee's type says what it takes, like a picked overload's.
                StaticType::Function {
                    parameters: Some(parameters),
                    return_type,
                } if parameters.len() == arguments.len() => {
                    let snapshot = self.vars.snapshot();
                    let mut fits = true;
                    for (parameter, argument) in parameters.iter().zip(&arguments) {
                        fits &= self.vars.bind_argument(parameter, argument);
                    }
                    if !fits {
                        self.vars.rollback(snapshot);
                        let arguments = arguments
                            .iter()
                            .map(|argument| self.vars.shown(argument))
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
                // A callee whose type isn't known yet is a function that
                // takes these arguments.
                callee @ StaticType::Var(_) => {
                    let returns = self.vars.fresh(StaticType::Any);
                    self.vars.unify(
                        &callee,
                        &StaticType::Function {
                            parameters: Some(arguments),
                            return_type: Box::new(returns.clone()),
                        },
                    );
                    self.vars.resolve(&returns)
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
        let arguments = arguments
            .iter()
            .map(|arg| self.analyse_expr(arg))
            .collect::<Vec<_>>();
        self.constrain_call(CallSite::Call(function.id), arguments, kind, name, span)
    }
}
