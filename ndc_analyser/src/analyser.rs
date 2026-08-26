use crate::scope::{CallKind, ResolvedCall, ScopeTree, TypeBinding};
use itertools::{Itertools, izip};
use ndc_core::static_type::StaticTypeConstructionError;
use ndc_core::r#struct::StructRegistry;
use ndc_core::{StaticType, TypeSignature};
use ndc_lexer::Span;
use ndc_parser::{
    AugmentedAssignmentPlan, Binding, Candidate, Expression, ExpressionLocation, ForBody,
    ForIteration, FunctionParameter, Lvalue, NodeId, TypeExpr,
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

/// A snapshot of the analyser's persistent state: the scope tree and the
/// number of registered structs at the time of the snapshot. Restoring it
/// discards every binding and struct declared since.
#[derive(Debug, Clone)]
pub struct Checkpoint {
    scope_tree: ScopeTree,
    struct_count: usize,
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

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            scope_tree: self.scope_tree.clone(),
            struct_count: self.struct_registry.borrow().len(),
        }
    }

    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.scope_tree = checkpoint.scope_tree;
        self.struct_registry
            .borrow_mut()
            .truncate(checkpoint.struct_count);
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

    /// Resolves a syntactic type annotation to a `StaticType`: `Int` becomes
    /// `StaticType::Int`, `Map<String, Int>` becomes `StaticType::Map { .. }`,
    /// and a declared struct name becomes its `StaticType::Struct`. Built-in
    /// names take precedence over structs. An unknown name or a wrong
    /// generic-argument count is reported as an analysis error at the
    /// annotation's span, and the annotation falls back to `Any` so the rest
    /// of the program is still checked.
    fn lower_type_expr(&mut self, expr: &TypeExpr) -> StaticType {
        match expr {
            TypeExpr::Tuple { elements, .. } => {
                StaticType::Tuple(elements.iter().map(|e| self.lower_type_expr(e)).collect())
            }
            TypeExpr::Name { name, args, span } => {
                let has_args = !args.is_empty();
                let args = args.iter().map(|a| self.lower_type_expr(a)).collect();
                match StaticType::from_name_and_args(name, args) {
                    Ok(typ) => typ,
                    Err(err) => {
                        let info = self.struct_registry.borrow().find_by_name(name).cloned();
                        match info {
                            Some(info) if !has_args => info.static_type(),
                            Some(_) => {
                                self.emit(AnalysisError::type_does_not_take_generic_args(
                                    name, *span,
                                ));
                                StaticType::Any
                            }
                            None => {
                                self.emit(AnalysisError::invalid_type_annotation(&err, *span));
                                StaticType::Any
                            }
                        }
                    }
                }
            }
        }
    }

    fn lower_annotation(&mut self, annotation: Option<&TypeExpr>) -> Option<StaticType> {
        annotation.map(|expr| self.lower_type_expr(expr))
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

    /// Like [`analyse_or_any`], but with an expected type from an annotation
    /// or an annotated target: `let x: List<Int> = []` types the literal as
    /// `List<Int>` instead of `List<Any>`. A container literal with no
    /// elements has nothing to infer from, so it adopts the expected element
    /// types; non-empty literals recurse so nested empties (`[(1, [])]`)
    /// adopt too. Everything else falls back to plain analysis, and the
    /// caller still subtype-checks the result against the expectation.
    ///
    /// This is a temporary measure until the type system supports type
    /// parameters and proper unification, at which point empty literals can
    /// be typed with fresh type variables instead of adopting the annotation.
    fn analyse_with_expected(
        &mut self,
        expr_loc: &mut ExpressionLocation,
        expected: &StaticType,
    ) -> StaticType {
        let typ = match (&mut expr_loc.expression, expected) {
            (
                Expression::List { values },
                StaticType::List(element) | StaticType::Sequence(element),
            ) => {
                let mut element_type: Option<StaticType> = None;
                for value in values {
                    let found = self.analyse_with_expected(value, element);
                    Self::fold_lub(&mut element_type, found);
                }
                StaticType::List(Box::new(
                    element_type.unwrap_or_else(|| element.as_ref().clone()),
                ))
            }
            (Expression::Map { values, default }, StaticType::Map { key, value }) => {
                let is_empty = values.is_empty();
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (entry_key, entry_value) in values {
                    let found = self.analyse_with_expected(entry_key, key);
                    Self::fold_lub(&mut key_type, found);
                    if let Some(entry_value) = entry_value {
                        let found = self.analyse_with_expected(entry_value, value);
                        Self::fold_lub(&mut value_type, found);
                    }
                }
                if let Some(default) = default {
                    let found = self.analyse_with_expected(default, value);
                    Self::fold_lub(&mut value_type, found);
                }
                StaticType::Map {
                    key: Box::new(key_type.unwrap_or_else(|| key.as_ref().clone())),
                    value: Box::new(value_type.unwrap_or_else(|| {
                        if is_empty {
                            value.as_ref().clone()
                        } else {
                            // A set literal like `%{1, 2}` has unit values.
                            StaticType::unit()
                        }
                    })),
                }
            }
            (Expression::Tuple { values }, StaticType::Tuple(elements))
                if values.len() == elements.len() =>
            {
                let types = values
                    .iter_mut()
                    .zip(elements)
                    .map(|(value, element)| self.analyse_with_expected(value, element))
                    .collect();
                StaticType::Tuple(types)
            }
            _ => return self.analyse_or_any(expr_loc),
        };
        self.result.expr_types.insert(expr_loc.id, typ.clone());
        typ
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
                let annotated_type = self.lower_annotation(annotated_type.as_ref());
                let value_span = value.span;
                let found_type = match &annotated_type {
                    Some(expected) => self.analyse_with_expected(value, expected),
                    None => self.analyse_or_any(value),
                };

                self.resolve_lvalue_declarative(
                    l_value,
                    annotated_type,
                    found_type.clone(),
                    value_span,
                );
                Ok(StaticType::unit())
            }
            Expression::Assignment { l_value, r_value } => {
                let old_type = self.resolve_lvalue_or_any(l_value, *span);
                let new_type = self.analyse_with_expected(r_value, &old_type);
                self.validate_lvalue_write(l_value, &old_type, &new_type, *span);

                Ok(StaticType::unit())
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
                plan,
            } => {
                let left_type = self.resolve_single_lvalue(l_value, *span)?;
                let right_type = self.analyse_or_any(r_value);
                let arg_types = vec![left_type.clone(), right_type.clone()];

                let ResolvedCall {
                    binding: assign_binding,
                    return_type: assign_return,
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
                let has_op_binding = !matches!(op_binding, Binding::None);
                let assign_is_eligible = Self::augmented_rhs_is_compatible(&left_type, &right_type);

                let writeback_type = match assign_binding {
                    Binding::Resolved(candidate) if assign_is_eligible => {
                        *plan = AugmentedAssignmentPlan::Resolved(Binding::Resolved(candidate));
                        None
                    }
                    Binding::Dynamic(mut assign_candidates) if assign_is_eligible => {
                        match op_binding.clone() {
                            Binding::Resolved(candidate) => {
                                if !assign_candidates.contains(&candidate) {
                                    assign_candidates.push(candidate);
                                }
                            }
                            Binding::Dynamic(op_candidates) => {
                                for candidate in op_candidates {
                                    if !assign_candidates.contains(&candidate) {
                                        assign_candidates.push(candidate);
                                    }
                                }
                            }
                            Binding::None => {}
                        }

                        let result_type = if has_op_binding {
                            assign_return.lub(&op_return)
                        } else {
                            assign_return
                        };
                        *plan =
                            AugmentedAssignmentPlan::Resolved(Binding::Dynamic(assign_candidates));
                        Some(result_type)
                    }
                    Binding::Resolved(_) | Binding::Dynamic(_) => {
                        // A specialized mutation exists, but using it would
                        // change the concrete left type. Reject it here rather
                        // than falling through to an ordinary operation whose
                        // erased return type could widen the same target.
                        self.emit(AnalysisError::mismatched_types(
                            &right_type,
                            &left_type,
                            *span,
                        ));
                        *plan = AugmentedAssignmentPlan::Unresolved;
                        None
                    }
                    Binding::None if has_op_binding => {
                        *plan = AugmentedAssignmentPlan::Resolved(op_binding);
                        Some(op_return)
                    }
                    Binding::None => {
                        self.emit(AnalysisError::function_not_found(
                            operation, &arg_types, *span,
                        ));
                        *plan = AugmentedAssignmentPlan::Unresolved;
                        None
                    }
                };

                if let Some(result_type) = writeback_type {
                    self.validate_lvalue_write(l_value, &left_type, &result_type, *span);
                }

                Ok(StaticType::unit())
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                parameters,
                body,
                return_annotation,
                resolved_return_type,
                captures,
                ..
            } => {
                for param in parameters.iter_mut() {
                    param.resolved_type = self.lower_annotation(param.annotation.as_ref());
                }
                *resolved_return_type = self.lower_annotation(return_annotation.as_ref());
                let return_type_slot: &Option<StaticType> = resolved_return_type;

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
            Expression::MemberAccess {
                receiver,
                member,
                member_span,
                resolved_getter,
            } => {
                let receiver_type = self.analyse_or_any(receiver);
                let ResolvedCall {
                    binding,
                    return_type,
                } = self.scope_tree.resolve_call(
                    member,
                    std::slice::from_ref(&receiver_type),
                    CallKind::Regular,
                );

                if matches!(binding, Binding::None) {
                    self.emit(AnalysisError::function_not_found(
                        member,
                        &[receiver_type],
                        *member_span,
                    ));
                }

                *resolved_getter = binding;
                Ok(return_type)
            }
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

                // Reads can produce the default value, so its type is part of
                // the map's value type.
                if let Some(default) = default {
                    let default_type = self.analyse_or_any(default);
                    Self::fold_lub(&mut value_type, default_type);
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
                let field_types: Vec<StaticType> = fields
                    .iter()
                    .map(|f| self.lower_type_expr(&f.annotation))
                    .collect();

                if self.struct_registry.borrow().find_by_name(name).is_some() {
                    self.emit(AnalysisError::struct_redefinition(name, *span));
                    return Ok(StaticType::unit());
                }

                let duplicate_fields = fields
                    .iter()
                    .duplicates_by(|field| &field.identifier)
                    .collect_vec();
                if !duplicate_fields.is_empty() {
                    for field in duplicate_fields {
                        self.emit(AnalysisError::field_redefinition(
                            &field.identifier,
                            name,
                            field.span,
                        ));
                    }
                    return Ok(StaticType::unit());
                }

                let struct_id = self.struct_registry.borrow_mut().register(
                    &*name,
                    fields
                        .iter()
                        .zip(&field_types)
                        .map(|(f, t)| (f.identifier.clone(), t.clone()))
                        .collect(),
                );
                *resolved = Some(struct_id);

                // Create a constructor
                *resolved_name = Some(self.scope_tree.create_local_binding(
                    name.clone(),
                    TypeBinding::Annotated(StaticType::Function {
                        parameters: Some(field_types.clone()),
                        return_type: Box::new(StaticType::Struct {
                            id: struct_id,
                            name: Box::from(name.as_str()),
                        }),
                    }),
                ));

                for (field, field_type) in fields.iter_mut().zip(&field_types) {
                    // Getter
                    field.resolved_getter = Some(self.scope_tree.create_local_binding(
                        field.identifier.clone(),
                        TypeBinding::Annotated(StaticType::Function {
                            parameters: Some(vec![
                                self.struct_registry.borrow()[struct_id].static_type(),
                            ]),
                            return_type: Box::new(field_type.clone()),
                        }),
                    ));

                    field.resolved_setter = Some(self.scope_tree.create_local_binding(
                        format!("{}=", field.identifier),
                        TypeBinding::Annotated(StaticType::Function {
                            parameters: Some(vec![
                                self.struct_registry.borrow()[struct_id].static_type(),
                                field_type.clone(),
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
                ForBody::List { expr } => StaticType::List(Box::new(self.analyse_or_any(expr))),
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
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
                let set_args = [
                    type_of_index_target.clone(),
                    index_type.clone(),
                    StaticType::Any,
                ];

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

                if Self::index_type_is_slice(&index_type)
                    && let StaticType::List(element) = &type_of_index_target
                {
                    return Ok(StaticType::List(element.clone()));
                }

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
            Lvalue::Member {
                receiver,
                member,
                member_span,
                resolved_getter,
                resolved_setter,
            } => {
                let receiver_type = self.analyse_or_any(receiver);
                let getter_args = [receiver_type.clone()];
                let setter_args = [receiver_type, StaticType::Any];
                let getter = self
                    .scope_tree
                    .resolve_call(member, &getter_args, CallKind::Regular);
                let setter_name = format!("{member}=");
                let setter =
                    self.scope_tree
                        .resolve_call(&setter_name, &setter_args, CallKind::Regular);
                let getter_missing = matches!(getter.binding, Binding::None);
                let setter_missing = matches!(setter.binding, Binding::None);

                *resolved_getter = Some(getter.binding);
                *resolved_setter = Some(setter.binding);

                if getter_missing {
                    return Err(AnalysisError::function_not_found(
                        member,
                        &getter_args,
                        *member_span,
                    ));
                }
                if setter_missing {
                    return Err(AnalysisError::function_not_found(
                        &setter_name,
                        &setter_args,
                        *member_span,
                    ));
                }

                Ok(getter.return_type)
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

    /// An index expression typed as an integer sequence (e.g. `0..2`) selects
    /// a slice of the container rather than a single element.
    fn index_type_is_slice(index_type: &StaticType) -> bool {
        index_type.is_subtype(&StaticType::Iterator(Box::new(StaticType::Int)))
    }

    /// Specialized `op=` implementations preserve the concrete left type.
    /// A tuple left-hand side represents vector dispatch, so a scalar right
    /// operand must be compatible with every concrete tuple element.
    fn augmented_rhs_is_compatible(left_type: &StaticType, right_type: &StaticType) -> bool {
        match (left_type, right_type) {
            (StaticType::Tuple(left), StaticType::Tuple(right)) => {
                left.len() == right.len()
                    && right
                        .iter()
                        .zip(left)
                        .all(|(right, left)| right.is_subtype(left))
            }
            (StaticType::Tuple(left), right) => left.iter().all(|left| right.is_subtype(left)),
            (left, right) => right.is_subtype(left),
        }
    }

    /// Validate a value that will be stored through an lvalue, widening an
    /// inferred binding when the location has a stable variable to update.
    fn validate_lvalue_write(
        &mut self,
        lvalue: &Lvalue,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
    ) {
        match lvalue {
            Lvalue::Identifier {
                resolved: Some(target),
                ..
            } => {
                let widened = stored_type.lub(value_type);
                if widened != *stored_type
                    && let Err(annotated_type) =
                        self.scope_tree.update_binding_type(*target, widened)
                    && !value_type.is_subtype(&annotated_type)
                {
                    self.emit(AnalysisError::mismatched_types(
                        value_type,
                        &annotated_type,
                        span,
                    ));
                }
            }
            Lvalue::Member { .. } => {
                if !value_type.is_subtype(stored_type) {
                    self.emit(AnalysisError::mismatched_types(
                        value_type,
                        stored_type,
                        span,
                    ));
                }
            }
            Lvalue::Index { value, index, .. } => {
                if value_type.is_subtype(stored_type) {
                    return;
                }

                let is_slice = self
                    .result
                    .expr_types
                    .get(&index.id)
                    .is_some_and(Self::index_type_is_slice);
                let (stored_element_type, value_element_type) = if is_slice {
                    let Some(stored_element_type) = stored_type.index_element_type() else {
                        self.emit(AnalysisError::mismatched_types(
                            value_type,
                            stored_type,
                            span,
                        ));
                        return;
                    };
                    let Some(value_element_type) = value_type.sequence_element_type() else {
                        self.emit(AnalysisError::mismatched_types(
                            value_type,
                            stored_type,
                            span,
                        ));
                        return;
                    };
                    (stored_element_type, value_element_type)
                } else {
                    (stored_type.clone(), value_type.clone())
                };

                if let Expression::Identifier {
                    resolved: Binding::Resolved(Candidate::Scalar(target)),
                    ..
                } = &value.expression
                {
                    let container_type = self.scope_tree.get_type(*target).clone();
                    let widened_container = container_type
                        .with_element_type(stored_element_type.lub(&value_element_type));
                    if widened_container != container_type
                        && self
                            .scope_tree
                            .update_binding_type(*target, widened_container)
                            .is_ok()
                    {
                        return;
                    }
                }

                self.emit(AnalysisError::mismatched_types(
                    value_type,
                    stored_type,
                    span,
                ));
            }
            Lvalue::Identifier { resolved: None, .. } | Lvalue::Sequence(_) => {}
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

        for param in parameters.iter().duplicates_by(|param| &param.name) {
            self.emit(AnalysisError::parameter_redefined(&param.name, span));
        }

        for param in parameters.iter().unique_by(|param| &param.name) {
            let has_annotation = param.type_name != StaticType::Any;
            let binding = if has_annotation {
                TypeBinding::Annotated(param.type_name.clone())
            } else {
                TypeBinding::Inferred(StaticType::Any)
            };

            self.scope_tree
                .create_local_binding(param.name.clone(), binding);
        }

        parameters
            .iter()
            .map(|param| param.type_name.clone())
            .collect()
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
            Lvalue::Member { receiver, .. } => {
                self.analyse_or_any(receiver);
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

    fn invalid_type_annotation(err: &StaticTypeConstructionError, span: Span) -> Self {
        Self {
            text: format!("{err}. {}", err.help_text()),
            span,
        }
    }

    fn type_does_not_take_generic_args(name: &str, span: Span) -> Self {
        Self {
            text: format!("type `{name}` does not take generic arguments"),
            span,
        }
    }

    fn struct_redefinition(name: &str, span: Span) -> Self {
        Self {
            text: format!("Illegal redefinition of struct '{name}'"),
            span,
        }
    }

    fn field_redefinition(field: &str, struct_name: &str, span: Span) -> Self {
        Self {
            text: format!("Illegal redefinition of field '{field}' in struct '{struct_name}'"),
            span,
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_lexer::{Lexer, SourceId};
    use ndc_parser::Parser;

    fn analyse_with_globals(
        source: &str,
        globals: Vec<(String, StaticType)>,
    ) -> (StaticType, AnalysisResult) {
        let tokens = Lexer::new(source, SourceId::SYNTHETIC)
            .collect::<Result<Vec<_>, _>>()
            .expect("lex failed");
        let mut expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
        let mut analyser =
            Analyser::from_scope_tree(ScopeTree::from_global_scope(globals), Default::default());
        let mut last_type = StaticType::unit();
        for expression in &mut expressions {
            last_type = analyser.analyse(expression).expect("analysis failed");
        }
        (last_type, analyser.take_result())
    }

    fn analyse_last_type_with_globals(
        source: &str,
        globals: Vec<(String, StaticType)>,
    ) -> StaticType {
        let (last_type, result) = analyse_with_globals(source, globals);
        assert!(
            result.errors.is_empty(),
            "analysis errors: {:?}",
            result.errors
        );
        last_type
    }

    fn analyse_last_type(source: &str) -> StaticType {
        analyse_last_type_with_globals(source, vec![])
    }

    fn assert_analysis_error(source: &str, globals: Vec<(String, StaticType)>, expected: &str) {
        let (_, result) = analyse_with_globals(source, globals);
        assert!(
            result
                .errors
                .iter()
                .any(|error| error.to_string().contains(expected)),
            "expected an error containing {expected:?}, got {:?}",
            result.errors,
        );
    }

    #[test]
    fn inferred_list_index_assignment_widens_element_type() {
        assert_eq!(
            analyse_last_type("let values = [1]; values[0] = \"two\"; values"),
            StaticType::List(Box::new(StaticType::Any)),
        );
    }

    #[test]
    fn inferred_list_slice_assignment_widens_element_type() {
        assert_eq!(
            analyse_last_type("let values = [1]; values[0..1] = [\"two\"]; values"),
            StaticType::List(Box::new(StaticType::Any)),
        );
    }

    #[test]
    fn inferred_map_index_assignment_widens_value_and_preserves_key_type() {
        assert_eq!(
            analyse_last_type("let values = %{\"one\": 1}; values[\"two\"] = \"two\"; values"),
            StaticType::Map {
                key: Box::new(StaticType::String),
                value: Box::new(StaticType::Any),
            },
        );
    }

    #[test]
    fn inferred_index_augmented_assignment_widens_element_type() {
        let add = StaticType::Function {
            parameters: Some(vec![StaticType::Int, StaticType::Float]),
            return_type: Box::new(StaticType::Number),
        };
        assert_eq!(
            analyse_last_type_with_globals(
                "let values = [1]; values[0] += 0.5; values",
                vec![("+".to_string(), add)],
            ),
            StaticType::List(Box::new(StaticType::Number)),
        );
    }

    #[test]
    fn compatible_specialized_assignment_preserves_left_type() {
        let list_any = StaticType::List(Box::new(StaticType::Any));
        let append = StaticType::Function {
            parameters: Some(vec![list_any.clone(), list_any.clone()]),
            return_type: Box::new(list_any),
        };

        assert_eq!(
            analyse_last_type_with_globals(
                "let values = [1]; values ++= [2]; values",
                vec![("++=".to_string(), append)],
            ),
            StaticType::List(Box::new(StaticType::Int)),
        );
    }

    #[test]
    fn incompatible_specialized_assignment_is_rejected() {
        let list_any = StaticType::List(Box::new(StaticType::Any));
        let concat = StaticType::Function {
            parameters: Some(vec![list_any.clone(), list_any.clone()]),
            return_type: Box::new(list_any),
        };

        assert_analysis_error(
            "let values = [1]; values ++= [\"two\"];",
            vec![
                ("++=".to_string(), concat.clone()),
                ("++".to_string(), concat.clone()),
            ],
            "mismatched types: found List<String> but expected List<Int>",
        );
        assert_analysis_error(
            "let values: List<Int> = [1]; values ++= [\"two\"];",
            vec![
                ("++=".to_string(), concat.clone()),
                ("++".to_string(), concat),
            ],
            "mismatched types: found List<String> but expected List<Int>",
        );
    }

    #[test]
    fn annotated_declaration_types_empty_container_literals() {
        assert_eq!(
            analyse_last_type("let x: Map<Int, Int> = %{}; x"),
            StaticType::Map {
                key: Box::new(StaticType::Int),
                value: Box::new(StaticType::Int),
            },
        );
        assert_eq!(
            analyse_last_type("let x: List<Int> = []; x"),
            StaticType::List(Box::new(StaticType::Int)),
        );
        assert_eq!(
            analyse_last_type("let x: List<List<Int>> = [[]]; x"),
            StaticType::List(Box::new(StaticType::List(Box::new(StaticType::Int)))),
        );
    }

    #[test]
    fn annotated_declaration_still_rejects_mismatched_literals() {
        assert_analysis_error(
            "let x: Map<Int, Int> = %{\"a\": 1};",
            vec![],
            "mismatched types: found Map<String, Int> but expected Map<Int, Int>",
        );
    }

    #[test]
    fn constructor_arity_mismatch_is_rejected() {
        assert_analysis_error(
            "struct Point { x: Int, y: Int }\nPoint(1)",
            vec![],
            "No function called 'Point' found that matches the arguments 'Int'",
        );
    }

    #[test]
    fn constructor_argument_type_mismatch_is_rejected() {
        assert_analysis_error(
            "struct Point { x: Int, y: Int }\nPoint(\"x\", 2)",
            vec![],
            "No function called 'Point' found that matches the arguments 'String, Int'",
        );
    }

    #[test]
    fn any_typed_callee_still_dispatches_dynamically() {
        let (_, result) =
            analyse_with_globals("f(\"x\")", vec![("f".to_string(), StaticType::Any)]);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn map_default_value_contributes_to_value_type() {
        assert_eq!(
            analyse_last_type("let m = %{:0}; m"),
            StaticType::Map {
                key: Box::new(StaticType::Any),
                value: Box::new(StaticType::Int),
            },
        );
    }

    #[test]
    fn duplicate_struct_field_is_rejected() {
        assert_analysis_error(
            "struct Dup { a: Int, a: String, a: Bool }",
            vec![],
            "Illegal redefinition of field 'a' in struct 'Dup'",
        );
    }

    #[test]
    fn struct_with_duplicate_fields_does_not_claim_its_name() {
        let (_, result) = analyse_with_globals(
            "struct Dup { a: Int, a: String }\nstruct Dup { a: Int }",
            vec![],
        );
        assert_eq!(
            result.errors.iter().map(ToString::to_string).collect_vec(),
            vec!["Illegal redefinition of field 'a' in struct 'Dup'"],
        );
    }
}
