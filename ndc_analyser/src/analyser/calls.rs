use super::Analyser;
use super::solver::CallSite;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads};
use itertools::Itertools;
use ndc_analysis::{Binding, Candidate};
use ndc_core::{Intrinsic, NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation};
use ndc_resolver::{Candidates, SymbolId};

impl Analyser {
    /// Why no candidate accepts a call of `name` with arguments of these types:
    /// a variable ended the search, a cast would make an overload fit, or
    /// nothing fits.
    pub(super) fn no_match(
        &self,
        candidates: &Candidates,
        type_arguments: &[StaticType],
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
        } else if crate::error::has_unknown(types) {
            AnalysisError::function_not_found_for_unknown(name, types, span)
        } else if types.contains(&StaticType::Any) {
            AnalysisError::function_not_found_for_any(name, types, span)
        } else if overloads.a_cast_could_resolve(candidates, types) {
            AnalysisError::function_not_found_suggest_cast(name, types, span)
        } else if name == "+"
            && let Some(help) = crate::error::adding_a_string(types)
        {
            AnalysisError::function_not_found(name, types, span).with_help(help)
        } else {
            AnalysisError::function_not_found(name, types, span).with_overloads(
                name,
                &self.what_the_overloads_take(candidates, type_arguments),
            )
        }
    }

    /// What each overload `candidates` can reach takes, as `(Int, String)`,
    /// when there are few enough to list: why `put("a")` doesn't fit a `put`
    /// whose body made its parameter an Int. With type arguments, only the
    /// overloads that take that many are listed, with them in place of
    /// their type parameters: `id::<String>` takes `(String)`.
    fn what_the_overloads_take(
        &self,
        candidates: &Candidates,
        type_arguments: &[StaticType],
    ) -> Vec<String> {
        const MOST: usize = 3;
        let symbols = self
            .overloads()
            .reachable_symbols(candidates)
            .into_iter()
            .filter_map(|symbol| {
                self.type_argument_bindings(symbol, type_arguments)
                    .map(|bound| (symbol, bound))
            })
            .collect::<Vec<_>>();
        if symbols.len() > MOST {
            return Vec::new();
        }
        symbols
            .into_iter()
            .filter_map(|(symbol, bound)| {
                let typ = self.vars.shown(self.types.get_type(symbol));
                let typ = typ.replace(&|part| match part {
                    StaticType::Generic(name) => bound
                        .iter()
                        .find(|(parameter, _)| parameter == name)
                        .map(|(_, argument)| self.vars.shown(argument)),
                    _ => None,
                });
                match typ {
                    StaticType::Function {
                        parameters: Some(parameters),
                        ..
                    } => Some(format!("({})", parameters.iter().join(", "))),
                    _ => None,
                }
            })
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
            CallKind::Regular,
            member,
            member_span,
        )
    }

    /// Resolves a call (regular or operator-form) and returns its result type.
    /// Only an operator other than a comparison can apply to each element of
    /// tuple arguments: see [`CallKind`].
    pub(super) fn analyse_call(
        &mut self,
        function: &ExpressionLocation,
        arguments: &[ExpressionLocation],
        kind: CallKind,
        span: Span,
    ) -> StaticType {
        // Higher-order call shapes like `get_function()()` have a non-identifier
        // function position, so the callee is analysed as a value. The callee
        // comes before the arguments, in source order like the resolver.
        let Expression::Identifier {
            name,
            type_arguments,
            ..
        } = &function.expression
        else {
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
                    if let Some(typ) = self.bind_signature(&callee, &arguments) {
                        self.flush_copies(None, span);
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
                        StaticType::Error
                    }
                }
                callee @ StaticType::Var(_) => self.call_unknown_function(&callee, arguments),
                // The callee's check failed, which was reported there.
                StaticType::Error => {
                    for argument in &arguments {
                        self.vars
                            .settle_unknowns_but_literals(argument, &StaticType::Error);
                    }
                    StaticType::Error
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
                    self.emit(AnalysisError::not_callable(&self.vars.shown(&other), span));
                    StaticType::Error
                }
            };
        };

        // A named callee declares nothing, so its overload can be picked after
        // the arguments' types are known.
        let site = CallSite::Call(function.id);
        let type_arguments = self.lower_type_arguments(type_arguments);
        let types = self.analyse_arguments(function.id, &type_arguments, arguments);
        if let Some((symbol, intrinsic)) =
            self.called_intrinsic(function.id, &type_arguments, &types)
        {
            return self.analyse_intrinsic_call(
                function.id,
                symbol,
                intrinsic,
                &type_arguments,
                &types,
                name,
                span,
            );
        }
        match arguments {
            _ if !type_arguments.is_empty() => {
                self.constrain_explicit_call(site, type_arguments, types, name, span)
            }
            [_, index] if name == "[]" => self.constrain_index_read(site, types, index, kind, span),
            _ => self.constrain_call(site, types, kind, name, span),
        }
    }

    /// The intrinsic a call of the name at `callee` with these type
    /// arguments and arguments means, if it means one: of the nearest scope
    /// with an overload that could take them, the only one. A function the
    /// program declares, as `fn cast<T>(x: T)`, is nearer, and takes the
    /// call if it can, as it would from a native. A call that only an
    /// intrinsic could mean, but that it can't take, as `cast::<Int>(1, 2)`,
    /// means it too, so its typing rule says what's wrong.
    fn called_intrinsic(
        &mut self,
        callee: NodeId,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
    ) -> Option<(SymbolId, Intrinsic)> {
        let candidates = self.resolver.reference_candidates(callee).clone();
        let reachable = self.overloads().reachable_symbols(&candidates);
        // Most calls can't mean an intrinsic, and need no overload tried.
        let intrinsic = reachable.iter().find_map(|&symbol| {
            self.resolver
                .intrinsic(symbol)
                .map(|intrinsic| (symbol, intrinsic))
        })?;
        if reachable.len() == 1 {
            return Some(intrinsic);
        }
        for group in self.overloads().reachable_groups(&candidates) {
            let viable = group
                .into_iter()
                .filter(|&symbol| self.could_accept(symbol, type_arguments, arguments))
                .collect::<Vec<_>>();
            match viable[..] {
                [] => {}
                [only] => {
                    return self
                        .resolver
                        .intrinsic(only)
                        .map(|intrinsic| (only, intrinsic));
                }
                _ => return None,
            }
        }
        None
    }

    /// Analyses the call at `callee` of the intrinsic `symbol`, which no
    /// signature says the type of: `cast::<T>(x)` is a cast of `x` to `T`.
    #[allow(clippy::too_many_arguments)]
    fn analyse_intrinsic_call(
        &mut self,
        callee: NodeId,
        symbol: SymbolId,
        intrinsic: Intrinsic,
        type_arguments: &[StaticType],
        arguments: &[StaticType],
        name: &str,
        span: Span,
    ) -> StaticType {
        self.record_pick(
            CallSite::Call(callee),
            Binding::Resolved(Candidate::Scalar(symbol)),
            arguments,
        );
        match intrinsic {
            Intrinsic::Cast | Intrinsic::TryCast => {
                let ([target], [found]) = (type_arguments, arguments) else {
                    for argument in arguments {
                        self.vars
                            .settle_unknowns_but_literals(argument, &StaticType::Error);
                    }
                    self.emit(if type_arguments.is_empty() && arguments.len() == 1 {
                        AnalysisError::cast_without_a_target(name, span)
                    } else {
                        AnalysisError::cast_takes_a_type_and_a_value(name, span)
                    });
                    return StaticType::Error;
                };
                self.analyse_cast(
                    callee,
                    found,
                    target.clone(),
                    intrinsic == Intrinsic::TryCast,
                    span,
                )
            }
            Intrinsic::TupleIndex => {
                unreachable!("internal error: no identifier names the tuple index")
            }
        }
    }

    /// [`Self::constrain_call`] for reading through `[]`: `arguments` are
    /// the container's type and that of `index`. An index that's a number
    /// written in the code picks a tuple's element, with its own type, as
    /// in `t[0]` and `t[0] += 1`.
    pub(super) fn constrain_index_read(
        &mut self,
        site: CallSite,
        arguments: Vec<StaticType>,
        index: &ExpressionLocation,
        kind: CallKind,
        span: Span,
    ) -> StaticType {
        self.constrain_index_call(
            site,
            Vec::new(),
            arguments,
            kind,
            "[]",
            span,
            index.integer_literal(),
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
        type_arguments: &[StaticType],
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
            if let Some(expected) =
                self.parameters_the_overloads_agree_on(callee, type_arguments, &types, position)
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
        type_arguments: &[StaticType],
        known: &[Option<StaticType>],
        position: usize,
    ) -> Option<Vec<Option<StaticType>>> {
        let candidates = self.resolver.reference_candidates(callee).clone();
        let mut agreed: Option<Vec<Option<StaticType>>> = None;
        for symbol in self.overloads().reachable_symbols(&candidates) {
            let expected = self.probe(|this| {
                this.parameters_one_overload_expects(symbol, type_arguments, known, position)
            });
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
        type_arguments: &[StaticType],
        known: &[Option<StaticType>],
        position: usize,
    ) -> Expects {
        let function = self.vars.resolve(self.types.get_type(symbol));
        let first = self.vars.count();
        let Some(mut made) = self.type_argument_bindings(symbol, type_arguments) else {
            return Expects::Nothing;
        };
        let Some(parameters) = self.vars.instantiate_parameters(&function, &mut made) else {
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
pub(super) fn is_known_before(typ: &StaticType, first: usize) -> bool {
    !typ.contains(&|part| match part {
        StaticType::Var(var) => var.0 as usize >= first,
        StaticType::Any | StaticType::Generic(_) => true,
        _ => false,
    })
}
