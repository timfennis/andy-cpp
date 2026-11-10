use super::Analyser;
use crate::error::AnalysisError;
use crate::types::TypeBinding;
use itertools::{Itertools, izip};
use ndc_analysis::{DeclaredStruct, FieldAccessors};
use ndc_core::r#struct::StructInfo;
use ndc_core::{NodeId, StaticType, TypeSignature};
use ndc_layout::Layout;
use ndc_lexer::Span;
use ndc_parser::{
    BindingPattern, BindingPatternLocation, ExpressionLocation, FunctionParameter, StructField,
    TypeExpr,
};
use ndc_resolver::SymbolId;
use std::rc::Rc;

impl Analyser {
    /// Analyses a function declaration and returns its type.
    pub(super) fn analyse_function_declaration(
        &mut self,
        id: NodeId,
        name: Option<&str>,
        parameters: &[FunctionParameter],
        body: &ExpressionLocation,
        return_annotation: Option<&TypeExpr>,
        span: Span,
    ) -> StaticType {
        let type_signature = TypeSignature::from_annotated_bindings(
            parameters
                .iter()
                .map(|param| {
                    let annotation = self.lower_annotation(param.annotation.as_ref());
                    (param.name().to_string(), annotation)
                })
                .collect(),
        );
        let annotated_return = self.lower_annotation(return_annotation);
        self.result.analysis.function_types.insert(
            id,
            StaticType::Function {
                parameters: type_signature.types(),
                return_type: Box::new(annotated_return.clone().unwrap_or_default()),
            },
        );

        // Pre-register the function before analyzing its body so recursive calls can
        // resolve the name. The return type is unknown at this point so we use Any.
        // A redefinition is reported but still bound, since the resolver declared it.
        let declared = if let Some(name) = name {
            let symbol = self.resolver.declared_symbol(id);
            let parameters = type_signature.types();
            if self.redefines_function(symbol, parameters.as_deref()) {
                self.emit(AnalysisError::function_redefinition(
                    name,
                    parameters.as_deref(),
                    span,
                ));
            }
            let placeholder = StaticType::Function {
                parameters: type_signature.types(),
                return_type: Box::new(annotated_return.clone().unwrap_or_default()),
            };
            let slot = self.declare(symbol, TypeBinding::Inferred(placeholder));
            Some((symbol, slot))
        } else {
            None
        };

        self.functions.push(id);
        self.return_type_stack.push(None);
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

        let implicit_return = self.analyse_expr(body);
        let explicit_return = self.return_type_stack.pop().unwrap();
        self.loop_depth = outer_loop_depth;
        self.functions.pop();
        let captures = Layout::new(&self.resolver).captures(id);
        self.result.analysis.captures.insert(id, captures);
        self.result
            .analysis
            .frame_sizes
            .insert(id, Layout::new(&self.resolver).frame_size(Some(id)));

        // Combine explicit `return` types with the block's implicit return type.
        let inferred_return = match explicit_return {
            Some(ret) => ret.lub(&implicit_return),
            None => implicit_return,
        };

        // If there is an annotated return type, validate it;
        // otherwise record the inferred type in the analysis.
        if let Some(annotated) = &annotated_return {
            if !inferred_return.is_subtype(annotated) {
                self.emit(AnalysisError::mismatched_types(
                    &inferred_return,
                    annotated,
                    span,
                ));
            }
        } else {
            self.result
                .analysis
                .inferred_return_types
                .insert(id, inferred_return.clone());
        }

        let effective_return = annotated_return.unwrap_or(inferred_return);

        let function_type = StaticType::Function {
            parameters: Some(param_types.clone()),
            return_type: Box::new(effective_return),
        };

        if let Some((symbol, slot)) = declared {
            let _ = self
                .types
                .update_binding_type(symbol, function_type.clone());
            self.result.analysis.variables.insert(id, slot);
        }

        function_type
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
            .map(|f| self.lower_type_expr(&f.annotation))
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
        let constructor = self.declare(
            symbols.constructor,
            TypeBinding::Annotated(info.constructor_type()),
        );
        self.result.analysis.variables.insert(id, constructor);

        let fields = symbols
            .accessors
            .iter()
            .enumerate()
            .map(|(index, &(getter, setter))| FieldAccessors {
                getter: self.declare(getter, TypeBinding::Annotated(info.getter_type(index))),
                setter: self.declare(setter, TypeBinding::Annotated(info.setter_type(index))),
            })
            .collect();
        self.result.analysis.structs.insert(
            id,
            DeclaredStruct {
                info: StructInfo::clone(&info),
                fields,
            },
        );

        StaticType::unit()
    }

    /// Whether a function declared earlier in the same scope as `symbol` has
    /// its name and the same parameter types, which makes `symbol` an illegal
    /// redefinition. `fn f(x: Int)` and `fn f(x: String)` are two overloads,
    /// but `fn f(x)` and `fn f(y)` both take `Any`. `None` is variadic.
    fn redefines_function(&self, symbol: SymbolId, parameters: Option<&[StaticType]>) -> bool {
        let name = self.resolver.symbol_name(symbol);
        let scope = self.resolver.scope(self.resolver.scope_of(symbol));
        scope.bindings()[..self.resolver.binding_position(symbol)]
            .iter()
            .filter(|&&earlier| self.resolver.symbol_name(earlier) == name)
            .any(|&earlier| match self.types.get_type(earlier) {
                StaticType::Function {
                    parameters: earlier,
                    ..
                } => earlier.as_deref() == parameters,
                _ => false,
            })
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
            let has_annotation = param.type_name != StaticType::Any;
            let binding = if has_annotation {
                TypeBinding::Annotated(param.type_name.clone())
            } else {
                TypeBinding::Inferred(StaticType::Any)
            };

            let symbol = self.resolver.declared_symbol(node);
            self.declare(symbol, binding);
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
                // If there is a type annotation and the given type is not a subtype of the annotated type we emit an error
                if let Some(expected_type) = &expected_type
                    && !found_type.is_subtype(expected_type)
                {
                    self.emit(AnalysisError::mismatched_types(
                        &found_type,
                        expected_type,
                        *span,
                    ));
                }

                let type_binding = match expected_type {
                    Some(annotated) => TypeBinding::Annotated(annotated),
                    None => TypeBinding::Inferred(found_type),
                };

                let symbol = self.resolver.declared_symbol(lvalue.id);
                let variable = self.declare(symbol, type_binding.clone());
                self.result.analysis.variables.insert(lvalue.id, variable);
                self.result
                    .analysis
                    .variable_types
                    .insert(lvalue.id, type_binding.typ().clone());
            }
            BindingPattern::Sequence(seq) => {
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
