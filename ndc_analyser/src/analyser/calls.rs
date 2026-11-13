use super::Analyser;
use super::solver::{CallNodes, CallSite, Constraint};
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::{NumericLiteral, Span};
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
                &self.vars.shown(self.types.get_type(variable)),
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
            CallNodes {
                arguments: vec![Some(receiver.id)],
                result: Some(id),
            },
            CallKind::Regular,
            member,
            member_span,
        )
    }

    /// Resolves a call (regular or operator-form) and returns its result type.
    /// Only operator-form calls are eligible for vec dispatch.
    pub(super) fn analyse_call(
        &mut self,
        id: NodeId,
        function: &ExpressionLocation,
        arguments: &[ExpressionLocation],
        kind: CallKind,
        span: Span,
    ) -> StaticType {
        // Higher-order call shapes like `get_function()()` have a non-identifier
        // function position, so the callee is analysed as a value. The callee
        // comes before the arguments, in source order like the resolver, so
        // declarations in it get their slots first.
        let nodes = arguments.iter().map(|arg| Some(arg.id)).collect::<Vec<_>>();
        let Expression::Identifier { name, .. } = &function.expression else {
            let callee_type = self.analyse_expr(function);
            let arguments = arguments
                .iter()
                .map(|arg| self.analyse_expr(arg))
                .collect::<Vec<_>>();
            // A call needs to know what it calls now, so what a literal
            // holds, like the functions in `[f, g][0](1)`, is settled.
            if let StaticType::Var(var) = self.vars.resolve(&callee_type)
                && self.vars.is_literal(var)
            {
                self.vars.settle_with_fallbacks(&[var]);
                self.solve_waiting();
            }
            return match self.vars.resolve(&callee_type) {
                // The callee's type says what it takes, like a picked overload's.
                callee @ StaticType::Function {
                    parameters: Some(_),
                    ..
                } => {
                    if let Some((typ, parameters)) = self.bind_signature(&callee, &arguments) {
                        for (node, expected) in
                            self.expected_of_any(&parameters, &arguments, &nodes)
                        {
                            self.expect(node, expected);
                        }
                        for (sequence, element) in
                            self.iterations_waited_for(&parameters, &arguments)
                        {
                            self.wait(Constraint::Iterate {
                                sequence,
                                element,
                                span,
                            });
                        }
                        typ
                    } else {
                        let arguments = arguments
                            .iter()
                            .map(|argument| self.vars.shown(argument))
                            .collect::<Vec<_>>();
                        self.emit(AnalysisError::callee_rejects_arguments(
                            &self.vars.shown(&callee),
                            &arguments,
                            span,
                        ));
                        StaticType::Any
                    }
                }
                callee @ StaticType::Var(_) => self.call_unknown_function(&callee, arguments),
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
                    self.emit(AnalysisError::not_callable(&self.vars.shown(&other), span));
                    StaticType::Any
                }
            };
        };

        // A named callee declares nothing, so its overload can be picked after
        // the arguments' types are known.
        let tuple_index = match arguments {
            [_, index] if name == "[]" => literal_index(index),
            _ => None,
        };
        let arguments = arguments
            .iter()
            .map(|arg| self.analyse_expr(arg))
            .collect::<Vec<_>>();
        self.constrain_index_call(
            CallSite::Call(function.id),
            arguments,
            CallNodes {
                arguments: nodes,
                result: Some(id),
            },
            kind,
            name,
            span,
            tuple_index,
        )
    }
}

/// The value of an index that's an integer written in the code, as in
/// `t[1]` or `t[-1]`.
fn literal_index(index: &ExpressionLocation) -> Option<i64> {
    match &index.expression {
        Expression::NumericLiteral(NumericLiteral::Int64(value)) => Some(*value),
        Expression::Call {
            function,
            arguments,
        }
        | Expression::OperatorCall {
            function,
            arguments,
        } => match (&function.expression, &arguments[..]) {
            (Expression::Identifier { name, .. }, [operand]) if name == "-" => {
                literal_index(operand).and_then(i64::checked_neg)
            }
            _ => None,
        },
        _ => None,
    }
}
