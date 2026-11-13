use super::Analyser;
use super::solver::{CallNodes, CallSite, Constraint};
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads};
use itertools::Itertools;
use ndc_core::{NodeId, StaticType};
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{Expression, ExpressionLocation};
use ndc_resolver::{Candidates, SymbolId};

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
                .with_overloads(name, &self.what_the_overloads_take(candidates))
        }
    }

    /// What each overload `candidates` can reach takes, as `(Int, String)`,
    /// when there are few enough to list: why `put("a")` doesn't fit a `put`
    /// whose body made its parameter an Int.
    fn what_the_overloads_take(&self, candidates: &Candidates) -> Vec<String> {
        const MOST: usize = 3;
        let symbols = self.overloads().reachable_symbols(candidates);
        if symbols.len() > MOST {
            return Vec::new();
        }
        symbols
            .into_iter()
            .filter_map(
                |symbol| match self.vars.shown(self.types.get_type(symbol)) {
                    StaticType::Function {
                        parameters: Some(parameters),
                        ..
                    } => Some(format!("({})", parameters.iter().join(", "))),
                    _ => None,
                },
            )
            .collect()
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
        let arguments = self.analyse_arguments(function.id, arguments);
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

    /// Analyses the arguments of a call of the function named at `callee`
    /// and returns their types. An anonymous function among them is
    /// analysed after the others, with the parameter types the overloads
    /// that take those agree on, so its body is checked against what it
    /// will be passed: in `players.map(fn(p) => p.size)`, `p` is a
    /// `Player`, which has no `size`.
    fn analyse_arguments(
        &mut self,
        callee: NodeId,
        arguments: &[ExpressionLocation],
    ) -> Vec<StaticType> {
        let mut types = arguments
            .iter()
            .map(|argument| {
                (!takes_parameter_types_from_the_call(argument))
                    .then(|| self.analyse_expr(argument))
            })
            .collect::<Vec<_>>();
        for (position, argument) in arguments.iter().enumerate() {
            if types[position].is_some() {
                continue;
            }
            if let Some(expected) = self.parameters_the_overloads_agree_on(callee, &types, position)
                && expected.len() == parameter_count(argument)
            {
                self.expected_parameters.insert(argument.id, expected);
            }
            types[position] = Some(self.analyse_expr(argument));
        }
        types.into_iter().flatten().collect()
    }

    /// The types of the parameters of the function passed at `position`
    /// that every overload at `callee` that takes the `known` arguments
    /// agrees on, each `None` where they don't, or where it isn't known
    /// yet. `None` when no overload takes them, or one takes a function of
    /// any type there.
    fn parameters_the_overloads_agree_on(
        &mut self,
        callee: NodeId,
        known: &[Option<StaticType>],
        position: usize,
    ) -> Option<Vec<Option<StaticType>>> {
        let candidates = self.resolver.reference_candidates(callee).clone();
        let mut agreed: Option<Vec<Option<StaticType>>> = None;
        for symbol in self.overloads().reachable_symbols(&candidates) {
            let snapshot = self.vars.snapshot();
            let expected = self.parameters_one_overload_expects(symbol, known, position);
            self.vars.rollback(snapshot);
            let expected = match expected {
                Expects::Parameters(expected) => expected,
                Expects::Nothing => continue,
                Expects::Anything => return None,
            };
            agreed = Some(match agreed {
                None => expected,
                Some(agreed) => agreed
                    .into_iter()
                    .zip(expected)
                    .map(|(agreed, expected)| {
                        agreed.filter(|agreed| Some(agreed) == expected.as_ref())
                    })
                    .collect(),
            });
        }
        agreed
    }

    /// The parameter types `symbol` expects of the function passed at
    /// `position` if it takes the `known` arguments. Settles variables, so
    /// the caller rolls them back.
    fn parameters_one_overload_expects(
        &mut self,
        symbol: SymbolId,
        known: &[Option<StaticType>],
        position: usize,
    ) -> Expects {
        let function = self.vars.resolve(self.types.get_type(symbol));
        let first = self.vars.count();
        let Some((parameters, _)) = self.vars.instantiate_signature(&function) else {
            return Expects::Anything;
        };
        let takes_known = parameters.len() == known.len()
            && parameters.iter().zip(known).all(|(parameter, argument)| {
                argument
                    .as_ref()
                    .is_none_or(|argument| self.vars.bind_argument(parameter, argument))
            });
        if !takes_known {
            return Expects::Nothing;
        }
        match self.vars.resolve(&parameters[position]) {
            StaticType::Function {
                parameters: Some(expected),
                ..
            } => Expects::Parameters(
                expected
                    .iter()
                    .map(|typ| {
                        Some(self.vars.resolve(typ)).filter(|typ| is_known_before(typ, first))
                    })
                    .collect(),
            ),
            _ => Expects::Anything,
        }
    }
}

/// What an overload expects of a function passed to it.
enum Expects {
    /// It doesn't take the call's other arguments.
    Nothing,
    /// A function whose parameters have these types, where they're known.
    Parameters(Vec<Option<StaticType>>),
    /// It takes a function of any type, or anything at all.
    Anything,
}

/// Whether `argument` is an anonymous function with a parameter whose type
/// isn't written, which the call it's passed to can give: `fn(p) =>
/// p.score`, but not `fn(p: Player) => p.score`.
fn takes_parameter_types_from_the_call(argument: &ExpressionLocation) -> bool {
    matches!(
        &argument.expression,
        Expression::FunctionDeclaration { name: None, parameters, .. }
            if parameters.iter().any(|parameter| parameter.annotation.is_none())
    )
}

/// How many parameters the function declared by `argument` has.
fn parameter_count(argument: &ExpressionLocation) -> usize {
    match &argument.expression {
        Expression::FunctionDeclaration { parameters, .. } => parameters.len(),
        _ => 0,
    }
}

/// Whether `typ` is known as far as the code around the call knows it:
/// nothing in it is only known as the program runs, and its variables are
/// from before `first`, not ones an overload's signature made. A list
/// literal's elements are a variable until its uses settle them.
fn is_known_before(typ: &StaticType, first: usize) -> bool {
    !typ.contains(&|part| match part {
        StaticType::Var(var) => var.0 as usize >= first,
        StaticType::Any | StaticType::Generic(_) => true,
        _ => false,
    })
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
