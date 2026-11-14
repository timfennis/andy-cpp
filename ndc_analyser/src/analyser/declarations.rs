use super::{Analyser, Returns};
use crate::error::AnalysisError;
use itertools::izip;
use ndc_core::r#struct::StructInfo;
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, ExpressionLocation, FunctionParameter, StructField,
    TypeExpr,
};
use ndc_resolver::SymbolId;
use std::rc::Rc;

/// What a function declaration consists of, named like `fn f(x) { … }` or
/// anonymous like `fn(x) => …`.
#[derive(Clone, Copy)]
pub(super) struct FunctionDeclaration<'a> {
    pub id: NodeId,
    pub name: Option<&'a str>,
    pub parameters: &'a [FunctionParameter],
    pub body: &'a ExpressionLocation,
    pub return_annotation: Option<&'a TypeExpr>,
    /// Whether it's a `pure fn`, whose results are cached.
    pub pure: bool,
    pub span: Span,
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
        let first_use = self.uses.len();
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
                    .unwrap_or_else(|| self.vars.fresh(StaticType::Any))
            })
            .collect::<Vec<_>>();
        let annotated_return = self.lower_annotation(return_annotation);
        // A function's value takes what its parameters turn out to be.
        let runtime_parameters = Some(param_types.clone());
        self.result.analysis.function_types.insert(
            id,
            StaticType::Function {
                parameters: runtime_parameters,
                return_type: Box::new(annotated_return.clone().unwrap_or_default()),
            },
        );
        self.declared_functions.push(id);
        let first_function = self.declared_functions.len();

        // The function is declared before its body is analysed, so a
        // recursive call can call it. Its return type is what the body
        // returns, which the recursive calls share. A redefinition is
        // reported but still bound, since the resolver declared it.
        let recursive_return = match (&declared, &annotated_return) {
            (Some(_), None) => Some(self.vars.fresh(StaticType::Any)),
            _ => None,
        };
        if let Some((name, symbol)) = declared {
            self.check_overlap(symbol, name, Some(&param_types), span);
            let placeholder = StaticType::Function {
                parameters: Some(param_types.clone()),
                return_type: Box::new(
                    annotated_return
                        .clone()
                        .or_else(|| recursive_return.clone())
                        .unwrap_or_default(),
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
        // settle what the body left waiting: in `fn f() -> Fn(String) ->
        // List<String> { split }`, it picks `split(String)`.
        self.add_return(&mut returns, body, implicit_return);
        let inferred_return = returns.joined.unwrap_or(StaticType::Never);

        if let Some(returns) = &recursive_return
            && !self.vars.try_unify(returns, &inferred_return)
        {
            // A function that returns itself, like `fn f() { f }`, would
            // have a type that contains itself, so it returns `Any`.
            if matches!(self.vars.resolve(returns), StaticType::Var(_)) {
                self.vars.unify(returns, &StaticType::Any);
            } else {
                self.emit_rejected_write(returns, &inferred_return, span);
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
                let typ =
                    self.generalize(symbol, &signature, id, pure, [first_use, first_function]);
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
        // of truth), so static bindings and runtime dispatch agree.
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
                // What a value whose type isn't known yet unpacks into is
                // settled once its type is.
                let found_type = self.vars.resolve(&found_type);
                if expected_type.is_none() && matches!(found_type, StaticType::Var(_)) {
                    let parts = self.constrain_unpacking(found_type, seq.len(), span);
                    for (sub_lvalue, part) in seq.iter().zip(parts) {
                        self.declare_binding_pattern(sub_lvalue, None, part, span);
                    }
                    return;
                }
                // If the type is a fixed-length Tuple whose arity doesn't match
                // the number of bindings, bind each name as a failed check. This
                // can happen when a variable is declared with one type (e.g. ())
                // and later reassigned to a tuple of a different arity — the
                // analyser doesn't track reassignment types.
                let is_annotated = expected_type.is_some();
                let resolved_type = expected_type.unwrap_or(found_type.clone());

                let sub_types: Box<dyn Iterator<Item = &StaticType>> =
                    if let StaticType::Tuple(elems) = &resolved_type {
                        if elems.len() != seq.len() {
                            self.emit(AnalysisError::tuple_arity_mismatch(
                                seq.len(),
                                elems.len(),
                                span,
                            ));
                            self.bind_patterns_as_errors(seq, span);
                            return;
                        } else {
                            Box::new(elems.iter())
                        }
                    } else if let Some(iter) = resolved_type.unpack() {
                        iter
                    } else {
                        self.emit(AnalysisError::unable_to_unpack_type(&resolved_type, span));
                        self.bind_patterns_as_errors(seq, span);
                        return;
                    };

                let found_types = found_type
                    .unpack()
                    .unwrap_or_else(|| Box::new(std::iter::repeat(&StaticType::Any)));

                let desired_length = seq.len();
                let mut actual_len = 0;

                for (sub_lvalue, sub_type, found_type) in izip!(seq.iter(), sub_types, found_types)
                {
                    let sub_expected = if is_annotated {
                        Some(sub_type.clone())
                    } else {
                        None
                    };
                    self.declare_binding_pattern(
                        sub_lvalue,
                        sub_expected,
                        found_type.clone(),
                        span,
                    );

                    actual_len += 1;
                }

                if desired_length != actual_len {
                    self.emit(AnalysisError::unable_to_unpack_type(&found_type, span));
                    self.bind_patterns_as_errors(&seq[actual_len..], span);
                }
            }
        }
    }

    /// Binds every name in `patterns` as a failed check, so a pattern that
    /// failed to type-check still declares its names. In `let (a, b, c) = (1, 2)`, `c`
    /// is bound and a later `print(c)` reports nothing further.
    fn bind_patterns_as_errors(&mut self, patterns: &[BindingPatternLocation], span: Span) {
        for pattern in patterns {
            self.declare_binding_pattern(pattern, None, StaticType::Error, span);
        }
    }
}
