use super::generics::Generalizing;
use super::solver::Constraint;
use super::{Analyser, Returns};
use crate::error::AnalysisError;
use itertools::izip;
use ndc_core::r#struct::{StructId, StructInfo};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, ExpressionLocation, FunctionParameter, StructField,
    TypeExpr, TypeParameter,
};
use ndc_resolver::SymbolId;
use std::cell::RefCell;
use std::rc::Rc;

/// What a function declaration consists of, named like `fn f(x) { … }` or
/// anonymous like `fn(x) => …`.
#[derive(Clone, Copy)]
pub(super) struct FunctionDeclaration<'a> {
    pub id: NodeId,
    pub name: Option<&'a str>,
    /// The type parameters it declares, as `T` in `fn f<T>(x: T)`.
    pub type_parameters: &'a [TypeParameter],
    pub parameters: &'a [FunctionParameter],
    pub body: &'a ExpressionLocation,
    pub return_annotation: Option<&'a TypeExpr>,
    /// Whether it's a `pure fn`, whose results are cached.
    pub pure: bool,
    pub span: Span,
}

/// A `pure fn`, whose parameters are checked to take only data: what its
/// declaration says once the batch has settled their types, and what each
/// instance of a generic function it's in or is gives them.
#[derive(Debug)]
pub(super) struct PureFunction {
    /// `None` for a lambda.
    pub(super) name: Option<String>,
    /// Each parameter's name, type and span.
    pub(super) parameters: Vec<(String, StaticType, Span)>,
}

impl Analyser {
    /// Analyses a function declaration and returns its type. Its
    /// unannotated parameters are type variables that what the body does
    /// with them constrains. A lambda's, and a nested `fn`'s, are settled by
    /// how the code around it uses it: in `xs.map(fn(x) => x + 1)`, `x` gets
    /// the type of `xs`'s elements. A `fn` in the program scope is generic:
    /// each call settles them anew (see [`Self::generalize`]).
    pub(super) fn analyse_function_declaration(
        &mut self,
        function: FunctionDeclaration<'_>,
    ) -> StaticType {
        let FunctionDeclaration {
            id,
            name,
            type_parameters,
            parameters,
            body,
            return_annotation,
            pure,
            span,
        } = function;
        let declared = name.map(|name| (name, self.resolver.declared_symbol(id)));
        let generic = declared.is_some_and(|(_, symbol)| self.is_generic_function(symbol));
        if generic {
            self.vars.enter_level();
            self.declaring = declared.map(|(_, symbol)| symbol);
        }
        // Each type parameter it declares is a variable of the body's, which
        // ends up one of the function's type parameters. Only a generic
        // function has type parameters: one in a function or a block has
        // one type.
        if !type_parameters.is_empty() && !generic {
            let first = &type_parameters[0];
            self.emit(AnalysisError::type_parameters_of_a_local_function(
                name.unwrap_or("?"),
                first
                    .span
                    .merge(type_parameters[type_parameters.len() - 1].span),
            ));
        }
        let declared_type_parameters = type_parameters
            .iter()
            .map(|parameter| {
                let symbol = self.resolver.type_parameter_symbol(parameter.id);
                let var = if generic {
                    self.vars.fresh()
                } else {
                    StaticType::Error
                };
                self.type_parameter_vars.insert(symbol, var.clone());
                var
            })
            .collect::<Vec<_>>();
        if let Some((_, symbol)) = declared
            && generic
            && !declared_type_parameters.is_empty()
        {
            self.explicit_type_parameters
                .insert(symbol, declared_type_parameters.clone());
        }
        let first_use = self.uses.len();
        let first_cast = self.casts.len();
        let annotations = parameters
            .iter()
            .map(|param| self.lower_annotation(param.annotation.as_ref()))
            .collect::<Vec<_>>();
        let expected = self.expected_parameters.remove(&id).unwrap_or_default();
        let param_types = annotations
            .iter()
            .enumerate()
            .map(|(position, annotation)| {
                annotation
                    .clone()
                    .or_else(|| expected.get(position).cloned().flatten())
                    .unwrap_or_else(|| self.vars.fresh())
            })
            .collect::<Vec<_>>();
        let annotated_return = self.lower_annotation(return_annotation);
        if pure {
            self.pure_in_batch.push(id);
            self.pure_functions.insert(
                id,
                PureFunction {
                    name: name.map(str::to_string),
                    parameters: parameters
                        .iter()
                        .zip(&param_types)
                        .map(|(parameter, typ)| {
                            (parameter.name().to_string(), typ.clone(), parameter.span)
                        })
                        .collect(),
                },
            );
        }
        // A function's value takes what its parameters turn out to be. What
        // it returns is `Any` unless it's written, or, for a lambda, until
        // its body says.
        let runtime_parameters = Some(param_types.clone());
        self.result.analysis.function_types.insert(
            id,
            StaticType::Function {
                parameters: runtime_parameters,
                return_type: Box::new(annotated_return.clone().unwrap_or(StaticType::Any)),
            },
        );
        self.declared_functions.push(id);
        let first_function = self.declared_functions.len();

        // The function is declared before its body is analysed, so a
        // recursive call can call it. Its return type is what the body
        // returns, which the recursive calls share. A redefinition is
        // reported but still bound, since the resolver declared it.
        let recursive_return = match (&declared, &annotated_return) {
            (Some(_), None) => Some(self.vars.fresh()),
            _ => None,
        };
        if let Some((name, symbol)) = declared {
            self.declared_overloads
                .push((symbol, name.to_string(), span));
            let placeholder = StaticType::Function {
                parameters: Some(param_types.clone()),
                return_type: Box::new(
                    annotated_return
                        .clone()
                        .or_else(|| recursive_return.clone())
                        .expect("internal error: a declared function's return type is known"),
                ),
            };
            self.types.declare(symbol, placeholder);
        }

        self.return_type_stack.push(Returns {
            annotated: annotated_return.clone(),
            joined: None,
        });
        // A function body starts outside of any loop: `break` can't
        // leave the function to end a loop around its declaration.
        let outer_loops = std::mem::take(&mut self.loops);
        for (param, typ) in parameters.iter().zip(&param_types) {
            let symbol = self.resolver.declared_symbol(param.lvalue.id);
            self.types.declare(symbol, typ.clone());
            self.result
                .analysis
                .variable_types
                .insert(param.lvalue.id, typ.clone());
        }

        let implicit_return = self.analyse_expr(body);
        let mut returns = self.return_type_stack.pop().unwrap();
        self.loops = outer_loops;

        // The body's value is returned like the values of `return`. An
        // annotated return type is what each of them returns, which can
        // settle what the body left waiting: in `fn f() -> fn(String) ->
        // [String] { split }`, it picks `split(String)`.
        self.add_return(&mut returns, body, implicit_return);
        let inferred_return = returns.joined.unwrap_or(StaticType::Never);

        if let Some(returns) = &recursive_return
            && !self.vars.try_unify(returns, &inferred_return)
        {
            // A function that returns itself, like `fn f() { f }`, or a list
            // of what it returns, like `fn f(n) => [f(n - 1)]`, would have a
            // type that contains itself: only an `Any` can say what it
            // returns, which has to be written. What it returns is a failed
            // check, so what's built from it isn't reported again.
            if matches!(self.vars.resolve(returns), StaticType::Var(_)) {
                self.emit(AnalysisError::return_type_contains_itself(
                    name.unwrap_or("?"),
                    span,
                ));
                self.vars.unify(returns, &StaticType::Error);
            } else {
                self.emit_rejected_write(returns, &inferred_return, span, false);
            }
        }

        let signature = StaticType::Function {
            parameters: Some(param_types.clone()),
            return_type: Box::new(
                annotated_return
                    .clone()
                    .unwrap_or_else(|| inferred_return.clone()),
            ),
        };
        let function_type = match declared {
            Some((_, symbol)) if generic => {
                self.vars.leave_level();
                self.declaring = None;
                self.declared_in_batch.insert(symbol);
                let flags = Generalizing {
                    pure,
                    return_annotated: annotated_return.is_some(),
                };
                let declared = declared_type_parameters
                    .iter()
                    .zip(type_parameters)
                    .map(|(typ, parameter)| (typ, parameter.name.as_str()))
                    .collect::<Vec<_>>();
                let typ = self.generalize(
                    symbol,
                    &signature,
                    id,
                    flags,
                    &declared,
                    [first_use, first_function, first_cast],
                );
                self.check_type_parameters(symbol, type_parameters, &declared_type_parameters);
                self.within_size_limit(symbol, name, typ, span)
            }
            _ => signature,
        };
        let inferred_return = self.vars.resolve(&inferred_return);

        if annotated_return.is_none() {
            self.result
                .analysis
                .inferred_return_types
                .insert(id, inferred_return.clone());
            // A lambda's value says what it returns, which tells whether it's
            // a function type an `Any` is used as without checking its calls.
            if declared.is_none()
                && let Some(StaticType::Function { return_type, .. }) =
                    self.result.analysis.function_types.get_mut(&id)
            {
                **return_type = inferred_return.clone();
            }
        }

        // A named declaration stores the function under its name and, like
        // `let`, evaluates to unit. An anonymous one evaluates to the function.
        match declared {
            Some((_, symbol)) => {
                self.types.declare(symbol, function_type);
                self.picks.variables.insert(id, symbol);
                StaticType::unit()
            }
            None => function_type,
        }
    }

    /// Checks that each type parameter the generic function `symbol`
    /// declares, `declared` the variables its body had for them, ended up a
    /// type parameter of its own: one the body settled, as `fn f<T>(x: T)
    /// -> Int { x }` does, or made the same as another, isn't one, and is
    /// reported. The function then takes type arguments for its type
    /// parameters in the order they're declared.
    fn check_type_parameters(
        &mut self,
        symbol: SymbolId,
        type_parameters: &[TypeParameter],
        declared: &[StaticType],
    ) {
        if type_parameters.is_empty() {
            return;
        }
        let function = self.resolver.symbol_name(symbol).to_string();
        let mut generics = Vec::with_capacity(declared.len());
        for (index, (parameter, var)) in type_parameters.iter().zip(declared).enumerate() {
            let resolved = self.vars.resolve(var);
            match &resolved {
                StaticType::Generic(_) => {
                    if let Some(earlier) = generics
                        .iter()
                        .position(|earlier: &StaticType| *earlier == resolved)
                    {
                        self.emit(AnalysisError::type_parameters_are_the_same(
                            &function,
                            &type_parameters[earlier].name,
                            &parameter.name,
                            parameter.span,
                        ));
                    }
                }
                // A failed check was reported where it failed.
                StaticType::Error => {}
                _ => self.emit(AnalysisError::type_parameter_is_not_generic(
                    &function,
                    &parameter.name,
                    &self.vars.shown(&resolved),
                    parameter.span,
                )),
            }
            debug_assert_eq!(generics.len(), index);
            generics.push(resolved);
        }
        self.check_type_parameters_are_not_joined(&function, type_parameters, &generics, symbol);
        self.explicit_type_parameters.insert(symbol, generics);
    }

    /// Reports each type parameter `generics[i]`, declared as
    /// `type_parameters[i]`, that the generic function `symbol`'s body
    /// joins with another type, as `if c { x } else { 1 }` does with `x: T`:
    /// a call can only pass that type for it. Unlike a call's, which a
    /// type parameter can meet in many ways, a join's constraint makes the
    /// two types one.
    fn check_type_parameters_are_not_joined(
        &mut self,
        function: &str,
        type_parameters: &[TypeParameter],
        generics: &[StaticType],
        symbol: SymbolId,
    ) {
        let Some(scheme) = self.schemes.get(&symbol) else {
            return;
        };
        let joined = scheme
            .constraints
            .iter()
            .filter_map(|waiting| match &waiting.constraint {
                Constraint::Join {
                    types: [a, b, _], ..
                } => Some((self.vars.resolve(a), self.vars.resolve(b))),
                _ => None,
            })
            .collect::<Vec<_>>();
        for (a, b) in joined {
            for (position, (parameter, generic)) in type_parameters.iter().zip(generics).enumerate()
            {
                let other = if a == *generic {
                    &b
                } else if b == *generic {
                    &a
                } else {
                    continue;
                };
                if other == generic {
                    continue;
                }
                let error = match generics.iter().position(|declared| declared == other) {
                    // Reported once, at the later of the two.
                    Some(index) if index > position => continue,
                    Some(index) => AnalysisError::type_parameters_are_the_same(
                        function,
                        &type_parameters[index].name,
                        &parameter.name,
                        parameter.span,
                    ),
                    // One the function doesn't declare, as an unannotated
                    // parameter's, leaves this one free: it's whatever the
                    // call makes both.
                    None if matches!(other, StaticType::Generic(_)) => continue,
                    None => AnalysisError::type_parameter_is_not_generic(
                        function,
                        &parameter.name,
                        &self.vars.shown(other),
                        parameter.span,
                    ),
                };
                self.emit(error);
            }
        }
    }

    /// `typ`, the type of the generic function `symbol`, unless it has more
    /// parts than the analyser can work with, as when each function in a
    /// chain doubles what the previous one returns: `fn f1(x) => f0(f0(x))`
    /// with `fn f0(x) => (x, x)`. Then it's reported, and what the function
    /// takes and returns are failed checks, so what calls it isn't built
    /// from its type, and isn't reported again.
    fn within_size_limit(
        &mut self,
        symbol: SymbolId,
        name: Option<&str>,
        typ: StaticType,
        span: Span,
    ) -> StaticType {
        const LIMIT: usize = 10_000;
        let parts = std::cell::Cell::new(0_usize);
        let too_large = typ.contains(&|_| {
            parts.set(parts.get() + 1);
            parts.get() > LIMIT
        });
        if !too_large {
            return typ;
        }
        self.emit(AnalysisError::type_too_large(
            name.unwrap_or("?"),
            LIMIT,
            span,
        ));
        self.schemes.remove(&symbol);
        let StaticType::Function { parameters, .. } = typ else {
            return StaticType::Error;
        };
        StaticType::Function {
            parameters: parameters.map(|parameters| vec![StaticType::Error; parameters.len()]),
            return_type: Box::new(StaticType::Error),
        }
    }

    /// Declares a struct's type, constructor and field accessors.
    pub(super) fn analyse_struct_declaration(
        &mut self,
        id: NodeId,
        name: &str,
        fields: &[StructField],
    ) -> StaticType {
        let field_types: Vec<StaticType> = fields
            .iter()
            .map(|f| self.lower_value_type(&f.annotation))
            .collect();

        let struct_id = self.struct_registry.register(
            name,
            fields
                .iter()
                .zip(&field_types)
                .map(|(f, t)| (f.identifier.clone(), t.clone()))
                .collect(),
        );

        // Bind the constructor and per-field accessors with the same
        // types the runtime functions report (StructInfo is the source
        // of truth), so the analyser and the functions' values agree.
        let info = Rc::clone(&self.struct_registry[struct_id]);

        let symbols = self.resolver.struct_symbols(id).clone();
        self.struct_types
            .insert(symbols.type_symbol, Rc::clone(&info));
        self.types
            .declare(symbols.constructor, info.constructor_type());
        self.picks.variables.insert(id, symbols.constructor);
        for (index, &(getter, setter)) in symbols.accessors.iter().enumerate() {
            self.types.declare(getter, info.getter_type(index));
            self.types.declare(setter, info.setter_type(index));
        }
        self.picks.structs.insert(id, StructInfo::clone(&info));

        StaticType::unit()
    }

    /// Declares each name in a `let` or `for` pattern, checking
    /// `found_type` against the annotation and the pattern's shape.
    pub(super) fn declare_binding_pattern(
        &mut self,
        lvalue: &BindingPatternLocation,
        expected_type: Option<StaticType>,
        found_type: StaticType,
        span: Span,
    ) {
        match &lvalue.pattern {
            BindingPattern::Identifier { span, .. } => {
                // The value is checked like an assignment to the annotated
                // variable, so `let o: Option<Int> = None` settles `None`.
                if let Some(expected_type) = &expected_type {
                    self.check_write(expected_type, &found_type, *span);
                }

                // An annotation decides the type, or else the value does.
                let typ = expected_type.unwrap_or(found_type);
                let symbol = self.resolver.declared_symbol(lvalue.id);
                self.types.declare(symbol, typ.clone());
                self.picks.variables.insert(lvalue.id, symbol);
                self.result.analysis.variable_types.insert(lvalue.id, typ);
            }
            BindingPattern::Sequence(seq) => {
                // Each name gets its part of the value and, under an
                // annotation, its part of that: `let (a, b): (Int, Int) = x`.
                // An annotation that doesn't fit the names is the one error.
                let expected = match expected_type {
                    None => vec![None; seq.len()],
                    Some(annotated) => {
                        let Some(parts) = self.unpack_known(&annotated, seq.len(), span) else {
                            for sub_lvalue in seq {
                                self.declare_binding_pattern(
                                    sub_lvalue,
                                    None,
                                    StaticType::Error,
                                    span,
                                );
                            }
                            return;
                        };
                        parts.into_iter().map(Some).collect()
                    }
                };
                let found = self.unpack(&found_type, seq.len(), span);
                for (sub_lvalue, expected, found) in izip!(seq, expected, found) {
                    self.declare_binding_pattern(sub_lvalue, expected, found, span);
                }
            }
        }
    }

    /// The part of `typ` a `pure fn` can't take: a function, an iterator, a
    /// heap or an `Any`, in `typ` or in the fields of a struct in it. A
    /// `pure fn` gives a call the result of an earlier one whose arguments
    /// `==` says are equal, and it says that of functions, iterators and
    /// heaps only when they're the same one, while an `Any` may hold any of
    /// them.
    pub(super) fn not_data(&self, typ: &StaticType) -> Option<StaticType> {
        self.not_data_beyond(typ, &mut Vec::new())
    }

    /// [`Self::not_data`], past the structs in `seen`, whose fields are
    /// already being looked at.
    fn not_data_beyond(&self, typ: &StaticType, seen: &mut Vec<StructId>) -> Option<StaticType> {
        let found = RefCell::new(None);
        let structs = RefCell::new(Vec::new());
        typ.contains(&|part| match part {
            StaticType::Any
            | StaticType::Function { .. }
            | StaticType::Lazy(_)
            | StaticType::MinHeap(_)
            | StaticType::MaxHeap(_) => {
                *found.borrow_mut() = Some(part.clone());
                true
            }
            StaticType::Struct { id, .. } => {
                structs.borrow_mut().push(*id);
                false
            }
            _ => false,
        });
        if let Some(part) = found.into_inner() {
            return Some(part);
        }
        for id in structs.into_inner() {
            if seen.contains(&id) {
                continue;
            }
            seen.push(id);
            let info = Rc::clone(&self.struct_registry[id]);
            for (_, field) in &info.fields {
                if let Some(part) = self.not_data_beyond(field, seen) {
                    return Some(part);
                }
            }
        }
        None
    }

    /// Reports the `pure fn`s the batch declares whose parameters are said
    /// to be more than data. What a type parameter of a generic function
    /// stands for is checked in each instance, see
    /// [`Self::check_pure_instance`].
    pub(super) fn check_pure_functions(&mut self) {
        for declaration in std::mem::take(&mut self.pure_in_batch) {
            let function = &self.pure_functions[&declaration];
            let found = function
                .parameters
                .iter()
                .find_map(|(parameter, typ, span)| {
                    let typ = self.vars.resolve(typ);
                    self.not_data(&typ).map(|part| {
                        AnalysisError::pure_fn_parameter_takes_more_than_data(
                            function.name.as_deref(),
                            parameter,
                            &self.vars.shown(&typ),
                            &part,
                            *span,
                        )
                    })
                });
            if let Some(error) = found {
                self.emit(error);
            }
        }
    }
}
