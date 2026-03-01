use crate::interpreter::function::StaticType;
use itertools::Itertools;
use ndc_lexer::Span;
use ndc_parser::{
    Binding, Expression, ExpressionPool, ExpressionRef, ForBody, ForIteration,
    Lvalue, ResolvedVar,
};
use std::fmt::{Debug, Formatter};

pub struct Analyser {
    scope_tree: ScopeTree,
}

impl Analyser {
    pub fn from_scope_tree(scope_tree: ScopeTree) -> Self {
        Self { scope_tree }
    }

    pub fn checkpoint(&self) -> ScopeTree {
        self.scope_tree.clone()
    }

    pub fn restore(&mut self, checkpoint: ScopeTree) {
        self.scope_tree = checkpoint;
    }

    pub fn analyse(
        &mut self,
        expr_ref: ExpressionRef,
        pool: &mut ExpressionPool,
    ) -> Result<StaticType, AnalysisError> {
        let span = pool.get(expr_ref).span;
        // Clone the expression to avoid holding a borrow on pool during recursive calls
        let expression = pool.get(expr_ref).expression.clone();

        match expression {
            Expression::BoolLiteral(_) => Ok(StaticType::Bool),
            Expression::StringLiteral(_) => Ok(StaticType::String),
            Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => Ok(StaticType::Int),
            Expression::Float64Literal(_) => Ok(StaticType::Float),
            Expression::ComplexLiteral(_) => Ok(StaticType::Complex),
            Expression::Continue | Expression::Break => Ok(StaticType::unit()),
            Expression::Identifier { name, .. } => {
                if name == "None" {
                    // TODO: we're going to need something like HM to infer the type of option here, maybe force type annotations?
                    return Ok(StaticType::Option(Box::new(StaticType::Any)));
                }
                let binding = self.scope_tree.get_binding_any(&name).ok_or_else(|| {
                    AnalysisError::identifier_not_previously_declared(&name, span)
                })?;

                if let Expression::Identifier { resolved, .. } =
                    &mut pool.get_mut(expr_ref).expression
                {
                    *resolved = Binding::Resolved(binding);
                }

                Ok(self.scope_tree.get_type(binding).clone())
            }
            Expression::Statement(inner) => {
                self.analyse(inner, pool)?;
                Ok(StaticType::unit())
            }
            Expression::Logical { left, right, .. } => {
                self.analyse(left, pool)?; // TODO: throw error if type does not match bool?
                self.analyse(right, pool)?; // TODO: throw error if type does not match bool?
                Ok(StaticType::Bool)
            }
            Expression::Grouping(expr) => self.analyse(expr, pool),
            Expression::VariableDeclaration { value, .. } => {
                let typ = self.analyse(value, pool)?;
                // Clone the l_value for mutation, then write back
                let mut l_value = if let Expression::VariableDeclaration { l_value, .. } =
                    &pool.get(expr_ref).expression
                {
                    l_value.clone()
                } else {
                    unreachable!()
                };
                self.resolve_lvalue_declarative(&mut l_value, typ, span, pool)?;
                if let Expression::VariableDeclaration {
                    l_value: pool_l_value,
                    ..
                } = &mut pool.get_mut(expr_ref).expression
                {
                    *pool_l_value = l_value;
                }
                Ok(StaticType::unit()) // TODO: never type here?
            }
            Expression::Assignment { r_value, .. } => {
                // Clone the l_value for mutation, then write back
                let mut l_value = if let Expression::Assignment { l_value, .. } =
                    &pool.get(expr_ref).expression
                {
                    l_value.clone()
                } else {
                    unreachable!()
                };
                self.resolve_lvalue(&mut l_value, span, pool)?;
                if let Expression::Assignment {
                    l_value: pool_l_value,
                    ..
                } = &mut pool.get_mut(expr_ref).expression
                {
                    *pool_l_value = l_value;
                }
                self.analyse(r_value, pool)?;
                Ok(StaticType::unit())
            }
            Expression::OpAssignment {
                r_value,
                ref operation,
                ..
            } => {
                let operation = operation.clone();
                let mut l_value = if let Expression::OpAssignment { l_value, .. } =
                    &pool.get(expr_ref).expression
                {
                    l_value.clone()
                } else {
                    unreachable!()
                };
                let left_type = self.resolve_single_lvalue(&mut l_value, span, pool)?;
                if let Expression::OpAssignment {
                    l_value: pool_l_value,
                    ..
                } = &mut pool.get_mut(expr_ref).expression
                {
                    *pool_l_value = l_value;
                }
                let right_type = self.analyse(r_value, pool)?;
                let arg_types = vec![left_type, right_type];

                let resolved_assign_operation = self
                    .scope_tree
                    .resolve_function2(&format!("{operation}="), &arg_types);
                let resolved_operation =
                    self.scope_tree.resolve_function2(&operation, &arg_types);

                if let Binding::None = resolved_operation {
                    return Err(AnalysisError::function_not_found(
                        &operation,
                        &arg_types,
                        span,
                    ));
                }

                if let Expression::OpAssignment {
                    resolved_assign_operation: pool_rao,
                    resolved_operation: pool_ro,
                    ..
                } = &mut pool.get_mut(expr_ref).expression
                {
                    *pool_rao = resolved_assign_operation;
                    *pool_ro = resolved_operation;
                }

                Ok(StaticType::unit())
            }
            Expression::FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            } => {
                // TODO: figuring out the type signature of function declarations is the rest of the owl

                // Pre-register the function before analysing its body so recursive calls can
                // resolve the name. The return type is unknown at this point so we use Any.
                let pre_slot = if let Some(ref name) = name {
                    let param_types: Vec<StaticType> =
                        std::iter::repeat_n(StaticType::Any, extract_argument_arity(parameters, pool))
                            .collect();

                    let placeholder = StaticType::Function {
                        parameters: Some(param_types),
                        return_type: Box::new(StaticType::Any),
                    };
                    Some(
                        self.scope_tree
                            .create_local_binding(name.clone(), placeholder),
                    )
                } else {
                    None
                };

                self.scope_tree.new_scope();
                let param_types = self.resolve_parameters_declarative(parameters, pool)?;

                let return_type = self.analyse(body, pool)?;
                self.scope_tree.destroy_scope();

                let function_type = StaticType::Function {
                    parameters: Some(param_types.clone()),
                    return_type: Box::new(return_type.clone()),
                };

                if let Expression::FunctionDeclaration {
                    return_type: return_type_slot,
                    resolved_name,
                    ..
                } = &mut pool.get_mut(expr_ref).expression
                {
                    *return_type_slot = Some(return_type);

                    if let Some(slot) = pre_slot {
                        self.scope_tree
                            .update_binding_type(slot, function_type.clone());
                        *resolved_name = Some(slot);
                    }
                }

                Ok(function_type)
            }
            Expression::Block { statements } => {
                self.scope_tree.new_scope();
                let mut last = None;
                for s in statements {
                    last = Some(self.analyse(s, pool)?);
                }
                self.scope_tree.destroy_scope();

                Ok(last.unwrap_or_else(StaticType::unit))
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.analyse(condition, pool)?;
                let true_type = self.analyse(on_true, pool)?;
                let false_type = if let Some(on_false) = on_false {
                    self.analyse(on_false, pool)?
                } else {
                    StaticType::unit()
                };

                if true_type != StaticType::unit() {
                    // TODO: Emit warning for not using a semicolon in this if
                }

                if true_type != false_type {
                    // TODO maybe show warning?
                }

                Ok(true_type.lub(&false_type))
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                self.analyse(expression, pool)?;
                self.analyse(loop_body, pool)?;
                Ok(StaticType::unit())
            }
            Expression::For { iterations, body } => {
                let mut iterations = iterations;
                let mut body = body;
                let result = self.resolve_for_iterations(&mut iterations, &mut body, span, pool)?;
                if let Expression::For {
                    iterations: pool_iterations,
                    body: pool_body,
                } = &mut pool.get_mut(expr_ref).expression
                {
                    *pool_iterations = iterations;
                    *pool_body = body;
                }
                Ok(result)
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let mut type_sig = Vec::with_capacity(arguments.len());
                for a in arguments {
                    type_sig.push(self.analyse(a, pool)?);
                }

                let StaticType::Function { return_type, .. } =
                    self.resolve_function_with_argument_types(function, &type_sig, span, pool)?
                else {
                    // If we couldn't resolve the identifier to a function we have to just assume that
                    // whatever identifier we did find is a function at runtime and will return Any
                    return Ok(StaticType::Any);
                };

                Ok(*return_type)
            }
            Expression::Index { index, value } => {
                self.analyse(index, pool)?;
                let container_type = self.analyse(value, pool)?;

                container_type
                    .index_element_type()
                    .ok_or_else(|| AnalysisError::unable_to_index_into(&container_type, span))
            }
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse(v, pool)?);
                }

                Ok(StaticType::Tuple(types))
            }
            Expression::List { values } => {
                let element_type = self.analyse_multiple_with_same_type(&values, pool)?;

                // TODO: for now if we encounter an empty list expression we say the list is generic over Any but this clearly is not a good solution
                Ok(StaticType::List(Box::new(
                    element_type.unwrap_or(StaticType::Any),
                )))
            }
            Expression::Map { values, default } => {
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in values {
                    if let Some(key_type) = &mut key_type {
                        let next_type = self.analyse(key, pool)?;
                        *key_type = key_type.lub(&next_type);
                    } else {
                        key_type = Some(self.analyse(key, pool)?);
                    }
                    if let Some(value) = value {
                        if let Some(value_type) = &mut value_type {
                            let next_type = self.analyse(value, pool)?;
                            if &next_type != value_type {
                                *value_type = value_type.lub(&next_type);
                            }
                        } else {
                            value_type = Some(self.analyse(value, pool)?);
                        }
                    }
                }

                if let Some(default) = default {
                    self.analyse(default, pool)?;
                }

                // TODO: defaulting to Any here is surely going to bite us later
                Ok(StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or_else(StaticType::unit)),
                })
            }
            // Return evaluates to the type of the expression it returns, which makes type checking easier!
            // Actually it doesn't seem to make it any easier
            Expression::Return { value } => self.analyse(value, pool),
            Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse(start, pool)?;
                }
                if let Some(end) = end {
                    self.analyse(end, pool)?;
                }

                Ok(StaticType::Iterator(Box::new(StaticType::Int)))
            }
        }
    }

    fn resolve_function_with_argument_types(
        &mut self,
        fn_expr_ref: ExpressionRef,
        argument_types: &[StaticType],
        span: Span,
        pool: &mut ExpressionPool,
    ) -> Result<StaticType, AnalysisError> {
        let expression = pool.get(fn_expr_ref).expression.clone();

        let Expression::Identifier { name, .. } = expression else {
            // It's possible that we're not trying to invoke an identifier `foo()` but instead we're
            // invoking a value like `get_function()()` so in this case we just continue like normal?
            return self.analyse(fn_expr_ref, pool);
        };

        // println!("resolve fn {name} {}", argument_types.iter().join(", "));

        let binding = self.scope_tree.resolve_function2(&name, argument_types);

        let out_type = match &binding {
            Binding::None => {
                return Err(AnalysisError::function_not_found(
                    &name,
                    argument_types,
                    span,
                ));
            }
            Binding::Resolved(res) => self.scope_tree.get_type(*res).clone(),

            // TODO: are we just going to lie about the type or is this just how truthful we can be
            Binding::Dynamic(_) => StaticType::Function {
                parameters: None,
                return_type: Box::new(StaticType::Any),
            },
        };

        if let Expression::Identifier { resolved, .. } = &mut pool.get_mut(fn_expr_ref).expression
        {
            *resolved = binding;
        }

        Ok(out_type)
    }

    fn resolve_for_iterations(
        &mut self,
        iterations: &mut Vec<ForIteration>,
        body: &mut Box<ForBody>,
        span: Span,
        pool: &mut ExpressionPool,
    ) -> Result<StaticType, AnalysisError> {
        let Some((iteration, tail)) = iterations.split_first_mut() else {
            unreachable!("because this function is never called with an empty slice");
        };

        let mut do_destroy = false;
        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                let sequence_type = self.analyse(*sequence, pool)?;

                self.scope_tree.new_scope();

                // TODO: when we give type parameters to all instances of sequence we can correctly infer StaticType::Any in this position
                self.resolve_lvalue_declarative(
                    l_value,
                    sequence_type
                        .sequence_element_type()
                        .unwrap_or(StaticType::Any),
                    span,
                    pool,
                )?;
                do_destroy = true; // TODO: why is this correct
            }
            ForIteration::Guard(expr) => {
                self.analyse(*expr, pool)?;
            }
        }

        let out_type = if !tail.is_empty() {
            self.resolve_for_iterations(
                &mut tail.to_vec(),
                body,
                span,
                pool,
            )?
        } else {
            match body.as_mut() {
                ForBody::Block(block) => {
                    self.analyse(*block, pool)?;
                    StaticType::unit()
                }
                ForBody::List(list) => StaticType::List(Box::new(self.analyse(*list, pool)?)),
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    let key_type = self.analyse(*key, pool)?;
                    let value_type = if let Some(value) = value {
                        self.analyse(*value, pool)?
                    } else {
                        StaticType::unit()
                    };

                    if let Some(default) = default {
                        self.analyse(**default, pool)?;
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

        Ok(out_type)
    }

    fn resolve_single_lvalue(
        &mut self,
        lvalue: &mut Lvalue,
        span: Span,
        pool: &mut ExpressionPool,
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
            Lvalue::Index { index, value } => {
                self.analyse(*index, pool)?;
                let type_of_index_target = self.analyse(*value, pool)?;

                type_of_index_target
                    .index_element_type()
                    .ok_or_else(|| AnalysisError::unable_to_index_into(&type_of_index_target, span))
            }
            Lvalue::Sequence(_) => {
                Err(AnalysisError::lvalue_required_to_be_single_identifier(span))
            }
        }
    }

    fn resolve_lvalue(
        &mut self,
        lvalue: &mut Lvalue,
        span: Span,
        pool: &mut ExpressionPool,
    ) -> Result<(), AnalysisError> {
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
            }
            Lvalue::Index { index, value } => {
                self.analyse(*index, pool)?;
                self.analyse(*value, pool)?;
            }
            Lvalue::Sequence(seq) => {
                for sub_lvalue in seq {
                    self.resolve_lvalue(sub_lvalue, span, pool)?
                }
            }
        }

        Ok(())
    }

    /// Resolve expressions as arguments to a function and return the function arity
    fn resolve_parameters_declarative(
        &mut self,
        params_ref: ExpressionRef,
        pool: &mut ExpressionPool,
    ) -> Result<Vec<StaticType>, AnalysisError> {
        let mut types: Vec<StaticType> = Vec::new();
        let mut names: Vec<String> = Vec::new();

        let values = if let Expression::Tuple { values } = &pool.get(params_ref).expression {
            values.clone()
        } else {
            panic!("expected arguments to be tuple");
        };

        for param_ref in values {
            let (name, param_span) = if let Expression::Identifier { name, .. } =
                &pool.get(param_ref).expression
            {
                (name.clone(), pool.get(param_ref).span)
            } else {
                panic!("expected tuple values to be ident");
            };

            // TODO: big challenge how do we figure out the function parameter types?
            //       it seems like this is something we need an HM like system for!?
            let resolved_type = StaticType::Any;
            types.push(resolved_type.clone());
            if names.contains(&name) {
                return Err(AnalysisError::parameter_redefined(&name, param_span));
            }
            names.push(name.clone());

            let binding = self
                .scope_tree
                .create_local_binding(name, resolved_type);

            if let Expression::Identifier { resolved, .. } =
                &mut pool.get_mut(param_ref).expression
            {
                *resolved = Binding::Resolved(binding);
            }
        }

        Ok(types)
    }

    fn resolve_lvalue_declarative(
        &mut self,
        lvalue: &mut Lvalue,
        typ: StaticType,
        span: Span,
        pool: &mut ExpressionPool,
    ) -> Result<(), AnalysisError> {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
                inferred_type,
                ..
            } => {
                *resolved = Some(
                    self.scope_tree
                        .create_local_binding(identifier.clone(), typ.clone()),
                );
                *inferred_type = Some(typ);
            }
            Lvalue::Index { index, value } => {
                self.analyse(*index, pool)?;
                self.analyse(*value, pool)?;
            }
            Lvalue::Sequence(seq) => {
                let sub_types = typ
                    .unpack()
                    .ok_or_else(|| AnalysisError::unable_to_unpack_type(&typ, span))?;

                for (sub_lvalue, sub_lvalue_type) in seq.iter_mut().zip(sub_types) {
                    self.resolve_lvalue_declarative(
                        sub_lvalue,
                        sub_lvalue_type.clone(),
                        /* todo: figure out how to narrow this span */ span,
                        pool,
                    )?
                }
            }
        }

        Ok(())
    }

    fn analyse_multiple_with_same_type(
        &mut self,
        expressions: &[ExpressionRef],
        pool: &mut ExpressionPool,
    ) -> Result<Option<StaticType>, AnalysisError> {
        let mut element_type: Option<StaticType> = None;

        for &expression in expressions {
            if let Some(element_type) = &mut element_type {
                let following_type = self.analyse(expression, pool)?;

                *element_type = element_type.lub(&following_type);
            } else {
                element_type = Some(self.analyse(expression, pool)?);
            }
        }

        Ok(element_type)
    }
}

fn extract_argument_arity(params_ref: ExpressionRef, pool: &ExpressionPool) -> usize {
    let Expression::Tuple { values } = &pool.get(params_ref).expression else {
        panic!("expected arguments to be tuple");
    };

    values.len()
}

#[derive(Debug, Clone)]
pub struct ScopeTree {
    current_scope_idx: usize,
    global_scope: Scope,
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub fn from_global_scope(global_scope_map: Vec<(String, StaticType)>) -> Self {
        Self {
            current_scope_idx: 0,
            global_scope: Scope {
                parent_idx: None,
                identifiers: global_scope_map,
            },
            scopes: vec![Scope::new(None)],
        }
    }

    fn get_type(&self, res: ResolvedVar) -> &StaticType {
        match res {
            ResolvedVar::Captured { slot, depth } => {
                let mut scope_idx = self.current_scope_idx;
                let mut depth = depth;
                while depth > 0 {
                    depth -= 1;
                    scope_idx = self.scopes[scope_idx]
                        .parent_idx
                        .expect("parent_idx was None while traversing the scope tree");
                }
                &self.scopes[scope_idx].identifiers[slot].1
            }
            // for now all globals are functions
            ResolvedVar::Global { slot } => &self.global_scope.identifiers[slot].1,
        }
    }

    fn new_scope(&mut self) -> &Scope {
        let old_scope_idx = self.current_scope_idx;
        self.current_scope_idx = self.scopes.len();
        let new_scope = Scope::new(Some(old_scope_idx));
        self.scopes.push(new_scope);
        &self.scopes[self.current_scope_idx]
    }

    fn destroy_scope(&mut self) {
        let next = self.scopes[self.current_scope_idx]
            .parent_idx
            .expect("tried to destroy scope while there were none");
        self.current_scope_idx = next;
    }

    fn get_binding_any(&mut self, ident: &str) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_slot_by_name(ident) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.find_slot_by_name(ident)?,
                });
            }
        }
    }

    fn resolve_function_dynamic(&mut self, ident: &str, sig: &[StaticType]) -> Vec<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            let candidates = self.scopes[scope_ptr].find_function_candidates(ident, sig);
            if !candidates.is_empty() {
                return candidates
                    .into_iter()
                    .map(|slot| ResolvedVar::Captured { slot, depth })
                    .collect();
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return self
                    .global_scope
                    .find_function_candidates(ident, sig)
                    .into_iter()
                    .map(|slot| ResolvedVar::Global { slot })
                    .collect();
            }
        }
    }

    fn resolve_function2(&mut self, ident: &str, sig: &[StaticType]) -> Binding {
        self.resolve_function(ident, sig)
            .map(Binding::Resolved)
            .or_else(|| {
                let loose_bindings = self.resolve_function_dynamic(ident, sig);

                if loose_bindings.is_empty() {
                    return None;
                }

                Some(Binding::Dynamic(loose_bindings))
            })
            // If we can't find any function in scope that could match, fall back to all same-named
            // bindings so runtime dynamic dispatch (including vectorization) can pick the right one.
            .or_else(|| {
                let all_bindings = self.get_all_bindings_by_name(ident);
                if all_bindings.is_empty() {
                    return None;
                }
                Some(Binding::Dynamic(all_bindings))
            })
            .unwrap_or(Binding::None)
    }

    fn get_all_bindings_by_name(&self, ident: &str) -> Vec<ResolvedVar> {
        let mut results = Vec::new();
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            let slots = self.scopes[scope_ptr].find_all_slots_by_name(ident);
            results.extend(
                slots
                    .into_iter()
                    .map(|slot| ResolvedVar::Captured { slot, depth }),
            );

            if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                let global_slots = self.global_scope.find_all_slots_by_name(ident);
                results.extend(
                    global_slots
                        .into_iter()
                        .map(|slot| ResolvedVar::Global { slot }),
                );
                break;
            }
        }

        results
    }

    fn resolve_function(&mut self, ident: &str, arg_types: &[StaticType]) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_function(ident, arg_types) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.find_function(ident, arg_types)?,
                });
            }
        }
    }

    fn create_local_binding(&mut self, ident: String, typ: StaticType) -> ResolvedVar {
        let scope = &mut self.scopes[self.current_scope_idx];
        let slot = scope.identifiers.len();
        scope.identifiers.push((ident, typ));
        ResolvedVar::Captured {
            slot,
            depth: 0,
        }
    }

    pub fn update_binding_type(&mut self, target: ResolvedVar, typ: StaticType) {
        match target {
            ResolvedVar::Captured { slot, depth } => {
                let mut scope_idx = self.current_scope_idx;
                let mut depth = depth;
                while depth > 0 {
                    depth -= 1;
                    scope_idx = self.scopes[scope_idx]
                        .parent_idx
                        .expect("parent_idx was None");
                }
                self.scopes[scope_idx].identifiers[slot].1 = typ;
            }
            ResolvedVar::Global { slot } => {
                self.global_scope.identifiers[slot].1 = typ;
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Scope {
    parent_idx: Option<usize>,
    identifiers: Vec<(String, StaticType)>,
}

impl Scope {
    fn new(parent_idx: Option<usize>) -> Self {
        Self {
            parent_idx,
            identifiers: Vec::new(),
        }
    }

    fn find_slot_by_name(&self, ident: &str) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|(name, _)| name == ident)
    }

    fn find_all_slots_by_name(&self, ident: &str) -> Vec<usize> {
        self.identifiers
            .iter()
            .enumerate()
            .filter_map(|(slot, (name, _))| if name == ident { Some(slot) } else { None })
            .collect()
    }

    fn find_function(&self, ident: &str, arg_types: &[StaticType]) -> Option<usize> {
        self.identifiers.iter().rposition(|(name, typ)| {
            name == ident && typ.is_fn_and_matches(arg_types)
        })
    }

    fn find_function_candidates(&self, ident: &str, sig: &[StaticType]) -> Vec<usize> {
        self.identifiers
            .iter()
            .enumerate()
            .filter_map(|(slot, (name, typ))| {
                if name == ident && typ.is_fn_and_matches(sig) {
                    Some(slot)
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct AnalysisError {
    text: String,
    span: Span,
    help_text: Option<String>,
}

impl std::error::Error for AnalysisError {}

impl std::fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl AnalysisError {
    fn identifier_not_previously_declared(ident: &str, span: Span) -> Self {
        Self {
            text: format!("Identifier '{ident}' not previously declared"),
            span,
            help_text: None,
        }
    }

    fn function_not_found(name: &str, arg_types: &[StaticType], span: Span) -> Self {
        Self {
            text: format!(
                "No function called '{name}' found that matches the arguments: ({})",
                arg_types.iter().join(", ")
            ),
            span,
            help_text: None,
        }
    }

    fn unable_to_index_into(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Cannot index into type '{typ}'"),
            span,
            help_text: None,
        }
    }

    fn lvalue_required_to_be_single_identifier(span: Span) -> Self {
        Self {
            text: "Left-hand side of augmented assignment must be a single identifier or index expression".to_string(),
            span,
            help_text: None,
        }
    }

    fn unable_to_unpack_type(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Cannot unpack type '{typ}' into a pattern"),
            span,
            help_text: None,
        }
    }

    fn parameter_redefined(name: &str, span: Span) -> Self {
        Self {
            text: format!("Parameter '{name}' is defined more than once"),
            span,
            help_text: None,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn help_text(&self) -> Option<&str> {
        self.help_text.as_deref()
    }
}
