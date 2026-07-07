use crate::scope::{CallKind, ResolvedCall, ScopeTree, TypeBinding};
use itertools::{Itertools, izip};
use ndc_core::r#struct::{StructInfo, StructRegistry};
use ndc_core::{StaticType, TypeSignature};
use ndc_lexer::Span;
use ndc_parser::{
    Binding, Candidate, Expression, ExpressionLocation, ForBody, ForIteration, FunctionParameter,
    Lvalue, NodeId,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

/// Side table holding semantic information keyed by AST node identity.
/// Keeps tooling-specific data (like per-expression types) out of the AST.
#[derive(Debug, Default)]
pub struct AnalysisResult {
    /// Maps each expression node to its inferred result type.
    pub expr_types: HashMap<NodeId, StaticType>,
    /// Inferred return types for functions without explicit annotations.
    /// Keyed by the FunctionDeclaration's `NodeId`.
    pub inferred_return_types: HashMap<NodeId, StaticType>,
    /// Errors accumulated during analysis. Non-empty when the analyser
    /// encountered problems but was able to continue with fallback types.
    pub errors: Vec<AnalysisError>,
}

#[derive(Debug)]
pub struct Analyser {
    struct_registry: Rc<RefCell<StructRegistry>>,
    scope_tree: ScopeTree,
    /// Stack of explicit `return` types for each enclosing function scope.
    /// Pushed on function entry, popped on exit. The value accumulates the
    /// lub of all `return <expr>` types seen so far.
    return_type_stack: Vec<Option<StaticType>>,
    /// Side table populated during analysis.
    result: AnalysisResult,
    /// Non-fatal errors accumulated during the current analysis pass.
    errors: Vec<AnalysisError>,
}

impl Analyser {
    pub fn from_scope_tree(
        scope_tree: ScopeTree,
        struct_registry: Rc<RefCell<StructRegistry>>,
    ) -> Self {
        Self {
            scope_tree,
            struct_registry,
            return_type_stack: Vec::new(),
            result: AnalysisResult::default(),
            errors: Vec::new(),
        }
    }

    pub fn checkpoint(&self) -> ScopeTree {
        self.scope_tree.clone()
    }

    pub fn restore(&mut self, checkpoint: ScopeTree) {
        self.scope_tree = checkpoint;
    }

    /// Take the accumulated analysis result (including any errors),
    /// resetting it for the next analysis.
    pub fn take_result(&mut self) -> AnalysisResult {
        let mut result = std::mem::take(&mut self.result);
        result.errors = std::mem::take(&mut self.errors);
        result
    }

    /// Record a non-fatal analysis error. The analyser continues with a
    /// fallback type (usually `Any`) so that subsequent code is still checked.
    fn emit(&mut self, err: AnalysisError) {
        self.errors.push(err);
    }

    /// Record an error from outside the analyser (e.g. a hard error caught by the caller).
    pub fn emit_external(&mut self, err: AnalysisError) {
        self.errors.push(err);
    }

    /// Returns `true` if any errors have been recorded during the current
    /// analysis pass.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn analyse(
        &mut self,
        expr_loc: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        let typ = self.analyse_inner(expr_loc)?;
        self.result.expr_types.insert(expr_loc.id, typ.clone());
        Ok(typ)
    }

    /// Like [`analyse`], but on error emits the error and returns `Any`
    /// so that analysis can continue.
    fn analyse_or_any(&mut self, expr_loc: &mut ExpressionLocation) -> StaticType {
        match self.analyse(expr_loc) {
            Ok(t) => t,
            Err(e) => {
                self.emit(e);
                StaticType::Any
            }
        }
    }

    fn analyse_inner(
        &mut self,
        ExpressionLocation {
            expression,
            span,
            id,
        }: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        match expression {
            Expression::BoolLiteral(_) => Ok(StaticType::Bool),
            Expression::StringLiteral(_) => Ok(StaticType::String),
            Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => Ok(StaticType::Int),
            Expression::Float64Literal(_) => Ok(StaticType::Float),
            Expression::ComplexLiteral(_) => Ok(StaticType::Complex),
            Expression::Continue | Expression::Break => Ok(StaticType::Never),
            Expression::Identifier {
                name: ident,
                resolved,
            } => {
                if ident == "None" {
                    return Ok(StaticType::Option(Box::new(StaticType::Any)));
                }
                let Some(binding) = self.scope_tree.get_binding_any(ident) else {
                    self.emit(AnalysisError::identifier_not_previously_declared(
                        ident, *span,
                    ));
                    return Ok(StaticType::Any);
                };

                *resolved = Binding::Resolved(Candidate::Scalar(binding));

                Ok(self.scope_tree.get_type(binding).clone())
            }
            Expression::Statement(inner) => {
                let typ = self.analyse_or_any(inner);
                // Diverging statements (return/break/continue) propagate Never
                // so that blocks can see that control doesn't fall through.
                if typ == StaticType::Never {
                    Ok(StaticType::Never)
                } else {
                    Ok(StaticType::unit())
                }
            }
            Expression::Logical { left, right, .. } => {
                self.analyse_or_any(left);
                self.analyse_or_any(right);
                Ok(StaticType::Bool)
            }
            Expression::Grouping(expr) => self.analyse(expr),
            Expression::VariableDeclaration {
                l_value,
                annotated_type,
                value,
            } => {
                let value_span = value.span;
                let found_type = self.analyse_or_any(value);

                self.resolve_lvalue_declarative(
                    l_value,
                    annotated_type.to_owned(),
                    found_type.clone(),
                    value_span,
                );
                Ok(StaticType::unit())
            }
            Expression::Assignment { l_value, r_value } => {
                let old_type = self.resolve_lvalue_or_any(l_value, *span);
                let new_type = self.analyse_or_any(r_value);

                if let Lvalue::Identifier {
                    resolved: Some(target),
                    ..
                } = l_value
                {
                    let widened = old_type.lub(&new_type);
                    if widened != old_type
                        && let Err(annotated_type) =
                            self.scope_tree.update_binding_type(*target, widened)
                        && !new_type.is_subtype(&annotated_type)
                    {
                        self.emit(AnalysisError::mismatched_types(
                            &new_type,
                            &annotated_type,
                            *span,
                        ));
                    }
                }

                Ok(StaticType::unit())
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
                resolved_assign_operation,
                resolved_operation,
            } => {
                let left_type = self.resolve_single_lvalue(l_value, *span)?;
                let right_type = self.analyse_or_any(r_value);
                let arg_types = vec![left_type, right_type];

                // Resolve both `op=` and `op` so we can widen the lvalue
                // by the result type of whichever one actually fires.
                let ResolvedCall {
                    binding: assign_binding,
                    ..
                } = self.scope_tree.resolve_call(
                    &format!("{operation}="),
                    &arg_types,
                    CallKind::Operator,
                );
                let ResolvedCall {
                    binding: op_binding,
                    return_type: op_return,
                } = self
                    .scope_tree
                    .resolve_call(operation, &arg_types, CallKind::Operator);

                *resolved_assign_operation = assign_binding;
                *resolved_operation = op_binding;

                // Either form satisfies the call: `op=` mutates in place;
                // `op` falls back through `a = a op b`. Only error when both
                // are missing — e.g. `Map -= Map` is fine via `-=` even when
                // `-` itself has no Map overload.
                if matches!(resolved_assign_operation, Binding::None)
                    && matches!(resolved_operation, Binding::None)
                {
                    self.emit(AnalysisError::function_not_found(
                        operation, &arg_types, *span,
                    ));
                }

                if !matches!(resolved_operation, Binding::None) {
                    let result_type = op_return;
                    match l_value {
                        Lvalue::Identifier {
                            resolved: Some(target),
                            ..
                        } => {
                            let widened = arg_types[0].lub(&result_type);
                            if widened != arg_types[0]
                                && let Err(annotated_type) =
                                    self.scope_tree.update_binding_type(*target, widened)
                                && !result_type.is_subtype(&annotated_type)
                            {
                                self.emit(AnalysisError::mismatched_types(
                                    &result_type,
                                    &annotated_type,
                                    *span,
                                ));
                            }
                        }
                        Lvalue::Index { value, .. } => {
                            if let Expression::Identifier {
                                resolved: Binding::Resolved(Candidate::Scalar(target)),
                                ..
                            } = &value.expression
                            {
                                let container_type = self.scope_tree.get_type(*target).clone();
                                if let Some(elem_type) = container_type.index_element_type() {
                                    let widened_elem = elem_type.lub(&result_type);
                                    if widened_elem != elem_type {
                                        let new_container =
                                            container_type.with_element_type(widened_elem);
                                        let _ = self
                                            .scope_tree
                                            .update_binding_type(*target, new_container);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }

                Ok(StaticType::unit())
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                parameters,
                body,
                return_type: return_type_slot,
                captures,
                ..
            } => {
                let type_signature = FunctionParameter::from_params(parameters);

                // Pre-register the function before analyzing its body so recursive calls can
                // resolve the name. The return type is unknown at this point so we use Any.
                let pre_slot =
                    if let Some(name) = name {
                        let arity = type_signature.types().map(|t| t.len());
                        if self.scope_tree.has_function_in_current_scope(name, arity) {
                            self.emit(AnalysisError::function_redefinition(name, arity, *span));
                            // Skip re-registering but still analyse the body below.
                            None
                        } else {
                            let placeholder = StaticType::Function {
                                parameters: type_signature.types(),
                                return_type: Box::new(
                                    return_type_slot.clone().unwrap_or(StaticType::Any),
                                ),
                            };
                            Some(self.scope_tree.create_local_binding(
                                name.clone(),
                                TypeBinding::Inferred(placeholder),
                            ))
                        }
                    } else {
                        None
                    };

                self.scope_tree.new_function_scope();
                self.return_type_stack.push(None);
                let param_types = self.resolve_parameters_declarative(&type_signature, *span);

                // Fill inferred_type on parameter Lvalues for LSP hints.
                for (p, typ) in parameters.iter_mut().zip(&param_types) {
                    if let Lvalue::Identifier { inferred_type, .. } = &mut p.lvalue {
                        *inferred_type = Some(typ.clone());
                    }
                }

                let implicit_return = self.analyse_or_any(body);
                let explicit_return = self.return_type_stack.pop().unwrap();
                *captures = self.scope_tree.current_scope_captures();
                self.scope_tree.destroy_scope();

                // Combine explicit `return` types with the block's implicit return type.
                let inferred_return = match explicit_return {
                    Some(ret) => ret.lub(&implicit_return),
                    None => implicit_return,
                };

                // If there is an annotated return type, validate it;
                // otherwise record the inferred type in the side table.
                if let Some(annotated) = return_type_slot {
                    if !inferred_return.is_subtype(annotated) {
                        self.emit(AnalysisError::mismatched_types(
                            &inferred_return,
                            annotated,
                            *span,
                        ));
                    }
                } else {
                    self.result
                        .inferred_return_types
                        .insert(*id, inferred_return.clone());
                }

                let effective_return = return_type_slot.clone().unwrap_or(inferred_return);

                let function_type = StaticType::Function {
                    parameters: Some(param_types.clone()),
                    return_type: Box::new(effective_return),
                };

                if let Some(slot) = pre_slot {
                    let _ = self
                        .scope_tree
                        .update_binding_type(slot, function_type.clone());
                    *resolved_name = Some(slot);
                }

                Ok(function_type)
            }
            Expression::Block { statements } => {
                self.scope_tree.new_block_scope();
                let mut last = None;
                for s in statements {
                    last = Some(self.analyse_or_any(s));
                }
                self.scope_tree.destroy_scope();

                Ok(last.unwrap_or_else(StaticType::unit))
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.analyse_or_any(condition);
                let true_type = self.analyse_or_any(on_true);
                let false_type = if let Some(on_false) = on_false {
                    self.analyse_or_any(on_false)
                } else {
                    StaticType::unit()
                };

                Ok(true_type.lub(&false_type))
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                self.analyse_or_any(expression);
                self.analyse_or_any(loop_body);
                Ok(StaticType::unit())
            }
            Expression::For { iterations, body } => {
                let return_type = self.resolve_for_iterations(iterations, body, *span);
                Ok(return_type)
            }
            Expression::Call {
                function,
                arguments,
            } => self.analyse_call(function, arguments, CallKind::Regular, *span),
            Expression::OperatorCall {
                function,
                arguments,
            } => self.analyse_call(function, arguments, CallKind::Operator, *span),
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse_or_any(v));
                }

                Ok(StaticType::Tuple(types))
            }
            Expression::List { values } => {
                let element_type = self.analyse_multiple_expression_with_same_type(values);

                Ok(StaticType::List(Box::new(
                    element_type.unwrap_or(StaticType::Any),
                )))
            }
            Expression::Map { values, default } => {
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in values {
                    Self::fold_lub(&mut key_type, self.analyse_or_any(key));
                    if let Some(value) = value {
                        Self::fold_lub(&mut value_type, self.analyse_or_any(value));
                    }
                }

                if let Some(default) = default {
                    self.analyse_or_any(default);
                }

                Ok(StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or_else(StaticType::unit)),
                })
            }
            Expression::Return { value } => {
                let typ = self.analyse_or_any(value);
                if let Some(slot) = self.return_type_stack.last_mut() {
                    Self::fold_lub(slot, typ);
                }
                Ok(StaticType::Never)
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse_or_any(start);
                }
                if let Some(end) = end {
                    self.analyse_or_any(end);
                }

                Ok(StaticType::Iterator(Box::new(StaticType::Int)))
            }
            Expression::StructDeclaration {
                name,
                fields,
                resolved,
                resolved_name,
            } => {
                let struct_id = self.struct_registry.borrow_mut().register(
                    &*name,
                    fields
                        .iter()
                        .cloned()
                        .map(|f| (f.identifier, f.annotation))
                        .collect(),
                );
                *resolved = Some(struct_id);

                // Create a constructor
                *resolved_name = Some(
                    self.scope_tree.create_local_binding(
                        name.clone(),
                        TypeBinding::Annotated(StaticType::Function {
                            parameters: Some(
                                fields
                                    .iter()
                                    .map(|thing| thing.annotation.clone())
                                    .collect(),
                            ),
                            return_type: Box::new(StaticType::Struct {
                                id: struct_id,
                                name: Box::from(name.as_str()),
                            }),
                        }),
                    ),
                );

                for field in fields {
                    // Getter
                    field.resolved_getter = Some(self.scope_tree.create_local_binding(
                        field.identifier.clone(),
                        TypeBinding::Annotated(StaticType::Function {
                            parameters: Some(vec![
                                self.struct_registry.borrow()[struct_id].static_type(),
                            ]),
                            return_type: Box::new(field.annotation.clone()),
                        }),
                    ));

                    field.resolved_setter = Some(self.scope_tree.create_local_binding(
                        format!("{}=", field.identifier),
                        TypeBinding::Annotated(StaticType::Function {
                            parameters: Some(vec![
                                self.struct_registry.borrow()[struct_id].static_type(),
                                field.annotation.clone(),
                            ]),
                            return_type: Box::new(StaticType::unit()),
                        }),
                    ));
                }

                Ok(StaticType::unit())
            }
        }
    }

    /// Resolves a call (regular or operator-form) and returns its result type.
    /// Only operator-form calls are eligible for vec dispatch.
    fn analyse_call(
        &mut self,
        function: &mut ExpressionLocation,
        arguments: &mut [ExpressionLocation],
        kind: CallKind,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        let mut type_sig = Vec::with_capacity(arguments.len());
        for arg in arguments {
            type_sig.push(self.analyse_or_any(arg));
        }

        // Higher-order call shapes like `get_function()()` have a non-identifier
        // function position; in that case we just analyse the callee as a value
        // and trust the runtime to dispatch.
        let Expression::Identifier { name, resolved } = &mut function.expression else {
            let callee_type = self.analyse_or_any(function);
            return Ok(match callee_type {
                StaticType::Function { return_type, .. } => *return_type,
                StaticType::Any => StaticType::Any,
                other => {
                    self.emit(AnalysisError::not_callable(&other, span));
                    StaticType::Any
                }
            });
        };

        let ResolvedCall {
            binding,
            return_type,
        } = self.scope_tree.resolve_call(name, &type_sig, kind);

        if matches!(binding, Binding::None) {
            self.emit(AnalysisError::function_not_found(name, &type_sig, span));
            *resolved = binding;
            return Ok(StaticType::Any);
        }

        *resolved = binding;
        Ok(return_type)
    }

    fn resolve_for_iterations(
        &mut self,
        iterations: &mut [ForIteration],
        body: &mut ForBody,
        span: Span,
    ) -> StaticType {
        let Some((iteration, tail)) = iterations.split_first_mut() else {
            unreachable!("because this function is never called with an empty slice");
        };

        let mut do_destroy = false;
        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                let sequence_span = sequence.span;
                let sequence_type = self.analyse_or_any(sequence);

                self.scope_tree.new_iteration_scope();

                let found_type = sequence_type
                    .sequence_element_type()
                    .unwrap_or(StaticType::Any);

                // TOOD: get this from the AST when the parser adds it
                let expected_type = None;

                self.resolve_lvalue_declarative(l_value, expected_type, found_type, sequence_span);
                do_destroy = true;
            }
            ForIteration::Guard(expr) => {
                self.analyse_or_any(expr);
            }
        }

        let out_type = if !tail.is_empty() {
            self.resolve_for_iterations(tail, body, span)
        } else {
            match body {
                ForBody::Block(block) => {
                    self.analyse_or_any(block);
                    StaticType::unit()
                }
                ForBody::List {
                    expr,
                    accumulator_slot,
                    ..
                } => {
                    // Reserve the accumulator slot BEFORE analysing the body so
                    // that nested for-comprehensions receive strictly higher slot
                    // numbers and cannot collide with this accumulator.
                    *accumulator_slot = Some(self.scope_tree.reserve_anonymous_slot());
                    StaticType::List(Box::new(self.analyse_or_any(expr)))
                }
                ForBody::Map {
                    key,
                    value,
                    default,
                    accumulator_slot,
                    ..
                } => {
                    *accumulator_slot = Some(self.scope_tree.reserve_anonymous_slot());
                    let key_type = self.analyse_or_any(key);
                    let value_type = if let Some(value) = value {
                        self.analyse_or_any(value)
                    } else {
                        StaticType::unit()
                    };

                    if let Some(default) = default {
                        self.analyse_or_any(default);
                    }

                    StaticType::Map {
                        key: Box::new(key_type),
                        value: Box::new(value_type),
                    }
                }
            }
        };

        if do_destroy {
            self.scope_tree.destroy_scope();
        }

        out_type
    }

    fn resolve_single_lvalue(
        &mut self,
        lvalue: &mut Lvalue,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        if matches!(lvalue, Lvalue::Sequence(_)) {
            return Err(AnalysisError::lvalue_required_to_be_single_identifier(span));
        }
        self.resolve_lvalue(lvalue, span)
    }

    fn resolve_lvalue(
        &mut self,
        lvalue: &mut Lvalue,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
                ..
            } => {
                let Some(target) = self.scope_tree.get_binding_any(identifier) else {
                    return Err(AnalysisError::identifier_not_previously_declared(
                        identifier, span,
                    ));
                };

                *resolved = Some(target);
                Ok(self.scope_tree.get_type(target).clone())
            }
            Lvalue::Index {
                index,
                value,
                resolved_set,
                resolved_get,
            } => {
                let index_type = self.analyse_or_any(index);
                let type_of_index_target = self.analyse_or_any(value);

                let get_args = [type_of_index_target.clone(), index_type.clone()];
                let set_args = [type_of_index_target.clone(), index_type, StaticType::Any];

                // Indexing isn't operator-form for vec purposes: there's no
                // natural broadcast story for `(list_a, list_b)[i]`.
                *resolved_get = Some(
                    self.scope_tree
                        .resolve_call("[]", &get_args, CallKind::Regular)
                        .binding,
                );
                *resolved_set = Some(
                    self.scope_tree
                        .resolve_call("[]=", &set_args, CallKind::Regular)
                        .binding,
                );

                if let Some(t) = type_of_index_target.index_element_type() {
                    Ok(t)
                } else {
                    self.emit(AnalysisError::unable_to_index_into(
                        &type_of_index_target,
                        span,
                    ));
                    Ok(StaticType::Any)
                }
            }
            Lvalue::Sequence(seq) => {
                for sub_lvalue in seq {
                    self.resolve_lvalue_or_any(sub_lvalue, span);
                }
                Ok(StaticType::unit())
            }
        }
    }

    fn resolve_lvalue_or_any(&mut self, lvalue: &mut Lvalue, span: Span) -> StaticType {
        match self.resolve_lvalue(lvalue, span) {
            Ok(t) => t,
            Err(e) => {
                self.emit(e);
                StaticType::Any
            }
        }
    }

    /// Resolve expressions as arguments to a function and return the function arity
    fn resolve_parameters_declarative(
        &mut self,
        type_signature: &TypeSignature,
        span: Span,
    ) -> Vec<StaticType> {
        let TypeSignature::Exact(parameters) = type_signature else {
            return vec![];
        };

        let mut types: Vec<StaticType> = Vec::new();
        let mut seen_names: Vec<&str> = Vec::new();

        for param in parameters {
            let has_annotation = param.type_name != StaticType::Any;
            let binding = if has_annotation {
                TypeBinding::Annotated(param.type_name.clone())
            } else {
                TypeBinding::Inferred(StaticType::Any)
            };

            types.push(param.type_name.clone());
            if seen_names.contains(&param.name.as_str()) {
                self.emit(AnalysisError::parameter_redefined(&param.name, span));
                continue;
            }
            seen_names.push(&param.name);

            self.scope_tree
                .create_local_binding(param.name.clone(), binding);
        }

        types
    }
    fn resolve_lvalue_declarative(
        &mut self,
        lvalue: &mut Lvalue,
        expected_type: Option<StaticType>,
        found_type: StaticType,
        span: Span,
    ) {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
                inferred_type,
                span,
            } => {
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

                *resolved = Some(
                    self.scope_tree
                        .create_local_binding(identifier.clone(), type_binding.clone()),
                );

                *inferred_type = Some(type_binding.typ().clone())
            }
            Lvalue::Index { index, value, .. } => {
                self.analyse_or_any(index);
                self.analyse_or_any(value);
            }
            Lvalue::Sequence(seq) => {
                // If the type is a fixed-length Tuple whose arity doesn't match
                // the number of lvalues, fall back to Any for each element. This
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
                            return;
                        } else {
                            Box::new(elems.iter())
                        }
                    } else if let Some(iter) = resolved_type.unpack() {
                        iter
                    } else {
                        self.emit(AnalysisError::unable_to_unpack_type(&resolved_type, span));
                        return;
                    };

                let found_types = found_type
                    .unpack()
                    .unwrap_or_else(|| Box::new(std::iter::repeat(&StaticType::Any)));

                let desired_length = seq.len();
                let mut actual_len = 0;

                for (sub_lvalue, sub_type, found_type) in
                    izip!(seq.iter_mut(), sub_types, found_types)
                {
                    let sub_expected = if is_annotated {
                        Some(sub_type.clone())
                    } else {
                        None
                    };
                    self.resolve_lvalue_declarative(
                        sub_lvalue,
                        sub_expected,
                        found_type.clone(),
                        span,
                    );

                    actual_len += 1;
                }

                if desired_length != actual_len {
                    self.emit(AnalysisError::unable_to_unpack_type(&found_type, span));
                }
            }
        }
    }
    fn analyse_multiple_expression_with_same_type(
        &mut self,
        expressions: &mut Vec<ExpressionLocation>,
    ) -> Option<StaticType> {
        let mut element_type: Option<StaticType> = None;
        for expression in expressions {
            Self::fold_lub(&mut element_type, self.analyse_or_any(expression));
        }
        element_type
    }

    /// Fold a new type into an accumulator via least-upper-bound.
    fn fold_lub(acc: &mut Option<StaticType>, new_type: StaticType) {
        match acc {
            Some(prev) => *prev = prev.lub(&new_type),
            None => *acc = Some(new_type),
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct AnalysisError {
    text: String,
    span: Span,
}

impl AnalysisError {
    pub fn span(&self) -> Span {
        self.span
    }

    fn tuple_arity_mismatch(ident_len: usize, annotation_len: usize, span: Span) -> Self {
        Self {
            text: format!(
                "mismatched tuple arity: found a len={ident_len} identifier and a len={annotation_len} annotation."
            ),
            span,
        }
    }

    fn mismatched_types(found: &StaticType, expected: &StaticType, span: Span) -> Self {
        Self {
            text: format!("mismatched types: found {found} but expected {expected}"),
            span,
        }
    }

    fn function_redefinition(name: &str, arity: Option<usize>, span: Span) -> Self {
        let arity_desc = match arity {
            Some(n) => format!("{n} parameter{}", if n == 1 { "" } else { "s" }),
            None => "variadic parameters".to_string(),
        };
        Self {
            text: format!(
                "Illegal redefinition of function '{name}' with {arity_desc} in the same scope"
            ),
            span,
        }
    }

    fn parameter_redefined(param: &str, span: Span) -> Self {
        Self {
            text: format!("Illegal redefinition of parameter {param}"),
            span,
        }
    }
    fn unable_to_index_into(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Unable to index into {typ}"),
            span,
        }
    }
    fn unable_to_unpack_type(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Invalid unpacking of {typ}"),
            span,
        }
    }
    fn lvalue_required_to_be_single_identifier(span: Span) -> Self {
        Self {
            text: "This lvalue is required to be a single identifier".to_string(),
            span,
        }
    }

    fn function_not_found(ident: &str, types: &[StaticType], span: Span) -> Self {
        Self {
            text: format!(
                "No function called '{ident}' found that matches the arguments '{}'",
                types.iter().join(", ")
            ),
            span,
        }
    }

    fn not_callable(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Unable to invoke {typ} as a function."),
            span,
        }
    }

    fn identifier_not_previously_declared(ident: &str, span: Span) -> Self {
        Self {
            text: format!("Identifier {ident} has not previously been declared"),
            span,
        }
    }
}
