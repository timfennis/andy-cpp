use super::solver::Constraint;
use super::{Analyser, Returns};
use crate::error::AnalysisError;
use itertools::{Itertools, izip};
use ndc_core::r#struct::StructInfo;
use ndc_core::{NodeId, StaticType, TypeSignature};
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, ExpressionLocation, FunctionParameter, StructField,
    TypeExpr,
};
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
            span,
        } = function;
        let declared = name.map(|name| (name, self.resolver.declared_symbol(id)));
        let generic = declared.is_some_and(|(_, symbol)| self.is_generic_function(symbol));
        if generic {
            self.vars.enter_level();
            self.declaring = declared.map(|(_, symbol)| symbol);
        }
        let first_use = self.uses.len();
        let (annotations, iterates): (Vec<_>, Vec<_>) = parameters
            .iter()
            .map(|param| self.lower_parameter_annotation(param.annotation.as_ref()))
            .unzip();
        let type_signature = TypeSignature::from_annotated_bindings(
            parameters
                .iter()
                .zip(&annotations)
                .map(|(param, annotation)| {
                    let typ = annotation
                        .clone()
                        .unwrap_or_else(|| self.vars.fresh(StaticType::Any));
                    (param.name().to_string(), Some(typ))
                })
                .collect(),
        );
        let annotated_return = self.lower_annotation(return_annotation);
        // Until the body says more, a named function's value takes what its
        // annotations say, as its body is compiled once for every type it's
        // called with.
        let runtime_parameters = match declared {
            Some(_) => Some(
                annotations
                    .iter()
                    .zip(&iterates)
                    .map(|(annotation, iterates)| match (annotation, iterates) {
                        (Some(annotation), _) => annotation.clone(),
                        (None, Some(_)) => StaticType::Iterable(Box::new(StaticType::Any)),
                        (None, None) => StaticType::Any,
                    })
                    .collect(),
            ),
            None => type_signature.types(),
        };
        self.result.analysis.function_types.insert(
            id,
            StaticType::Function {
                parameters: runtime_parameters,
                return_type: Box::new(annotated_return.clone().unwrap_or_default()),
            },
        );

        // The function is declared before its body is analysed, so a
        // recursive call can call it. Its return type is what the body
        // returns, which the recursive calls share. A redefinition is
        // reported but still bound, since the resolver declared it.
        let recursive_return = match (&declared, &annotated_return) {
            (Some(_), None) => Some(self.vars.fresh(StaticType::Any)),
            _ => None,
        };
        if let Some((name, symbol)) = declared {
            let parameters = type_signature.types();
            // An `Iterable<T>` parameter takes what each call settles, but
            // only sequences of `T`s, which tells it from its other overloads.
            let shape = iterates
                .iter()
                .map(|element| {
                    element
                        .as_ref()
                        .map(|element| StaticType::Iterable(Box::new(element.clone())))
                })
                .collect::<Vec<_>>();
            let shaped = parameters.as_ref().map(|parameters| {
                parameters
                    .iter()
                    .zip(&shape)
                    .map(|(parameter, shape)| shape.clone().unwrap_or_else(|| parameter.clone()))
                    .collect::<Vec<_>>()
            });
            self.types.declare_shape(symbol, shape);
            self.check_overlap(symbol, name, shaped.as_deref(), span);
            let placeholder = StaticType::Function {
                parameters,
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
        let outer_loop_depth = std::mem::take(&mut self.loop_depth);
        let parameter_nodes = parameters.iter().map(|p| p.lvalue.id).collect_vec();
        let param_types = self.declare_parameters(&type_signature, &parameter_nodes);

        for (param, typ) in parameters.iter().zip(&param_types) {
            self.result
                .analysis
                .variable_types
                .insert(param.lvalue.id, typ.clone());
        }
        // An `Iterable<T>` parameter is what each call passes, which has to
        // iterate as `T`: `fn total(xs: Iterable<Int>)` takes a list or a
        // tuple of Ints.
        for ((param, typ), element) in parameters.iter().zip(&param_types).zip(&iterates) {
            if let Some(element) = element {
                let span = param.annotation.as_ref().map_or(span, TypeExpr::span);
                self.wait(Constraint::Iterate {
                    sequence: typ.clone(),
                    element: element.clone(),
                    span,
                });
            }
        }

        let implicit_return = self.analyse_expr(body);
        let mut returns = self.return_type_stack.pop().unwrap();
        self.loop_depth = outer_loop_depth;

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
                self.emit(AnalysisError::mismatched_types(
                    &self.vars.shown(&inferred_return),
                    &self.vars.shown(returns),
                    span,
                ));
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
                self.generalize(symbol, &signature, id, first_use)
            }
            _ => signature,
        };
        let inferred_return = self.vars.resolve(&inferred_return);

        if annotated_return.is_none() {
            self.result
                .analysis
                .inferred_return_types
                .insert(id, inferred_return.clone());
        }

        // A named function's value is checked as the program runs against
        // what it takes, as far as that can be checked: its body may have
        // made an unannotated parameter an Int.
        if declared.is_some()
            && let StaticType::Function {
                parameters: Some(parameters),
                ..
            } = self.vars.resolve(&function_type)
            && let Some(StaticType::Function {
                parameters: Some(runtime),
                ..
            }) = self.result.analysis.function_types.get_mut(&id)
        {
            for (((runtime, parameter), annotation), iterates) in runtime
                .iter_mut()
                .zip(parameters)
                .zip(&annotations)
                .zip(&iterates)
            {
                if annotation.is_none() && iterates.is_none() {
                    *runtime = checkable_as_the_program_runs(&parameter);
                }
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

    /// Declares a function's parameters and returns their types.
    fn declare_parameters(
        &mut self,
        type_signature: &TypeSignature,
        parameter_nodes: &[NodeId],
    ) -> Vec<StaticType> {
        let TypeSignature::Exact(parameters) = type_signature else {
            return vec![];
        };

        for (param, &node) in parameters.iter().zip(parameter_nodes) {
            let symbol = self.resolver.declared_symbol(node);
            self.types.declare(symbol, param.type_name.clone());
        }

        parameters
            .iter()
            .map(|param| param.type_name.clone())
            .collect()
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
                if let Some(expected_type) = &expected_type
                    && !self.accepts_write(expected_type, &found_type, Some(lvalue.id))
                {
                    self.emit_rejected_write(expected_type, &found_type, *span);
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
                // the number of bindings, fall back to Any for each element. This
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
                            self.bind_patterns_as_any(seq, span);
                            return;
                        } else {
                            Box::new(elems.iter())
                        }
                    } else if let Some(iter) = resolved_type.unpack() {
                        iter
                    } else {
                        self.emit(AnalysisError::unable_to_unpack_type(&resolved_type, span));
                        self.bind_patterns_as_any(seq, span);
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
                    self.bind_patterns_as_any(&seq[actual_len..], span);
                }
            }
        }
    }

    /// Binds every name in `patterns` as `Any`, so a pattern that failed to
    /// type-check still declares its names. In `let (a, b, c) = (1, 2)`, `c`
    /// is bound and a later `print(c)` reports nothing further.
    fn bind_patterns_as_any(&mut self, patterns: &[BindingPatternLocation], span: Span) {
        for pattern in patterns {
            self.declare_binding_pattern(pattern, None, StaticType::Any, span);
        }
    }
}

/// What the program can check about a value of type `typ` as it runs: the
/// kind of value, but not what a container holds, and nothing about a type
/// parameter, which takes anything. `List<Int>` is checked as a list.
fn checkable_as_the_program_runs(typ: &StaticType) -> StaticType {
    let any = || Box::new(StaticType::Any);
    match typ {
        StaticType::Var(_) | StaticType::Generic(_) | StaticType::Tuple(_) => StaticType::Any,
        StaticType::List(_) => StaticType::List(any()),
        StaticType::Deque(_) => StaticType::Deque(any()),
        StaticType::Iterable(_) => StaticType::Iterable(any()),
        StaticType::Iterator(_) => StaticType::Iterator(any()),
        StaticType::MinHeap(_) => StaticType::MinHeap(any()),
        StaticType::MaxHeap(_) => StaticType::MaxHeap(any()),
        StaticType::Option(_) => StaticType::Option(any()),
        StaticType::Map { .. } => StaticType::Map {
            key: any(),
            value: any(),
        },
        StaticType::Function { parameters, .. } => StaticType::Function {
            parameters: parameters
                .as_ref()
                .map(|parameters| vec![StaticType::Any; parameters.len()]),
            return_type: any(),
        },
        other => other.clone(),
    }
}
