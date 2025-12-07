use crate::ast::{
    Binding, Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, ResolvedVar,
};
use crate::interpreter::function::StaticType;
use crate::lexer::Span;
use itertools::Itertools;

#[derive(Debug)]
pub struct Analyser {
    scope_tree: ScopeTree,
}

impl Analyser {
    pub fn from_scope_tree(scope_tree: ScopeTree) -> Self {
        Self { scope_tree }
    }
}

impl Analyser {
    pub fn analyse(
        &mut self,
        ExpressionLocation { expression, span }: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        match expression {
            Expression::BoolLiteral(_) => Ok(StaticType::Bool),
            Expression::StringLiteral(_) => Ok(StaticType::String),
            Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => Ok(StaticType::Int),
            Expression::Float64Literal(_) => Ok(StaticType::Float),
            Expression::ComplexLiteral(_) => Ok(StaticType::Complex),
            Expression::Continue | Expression::Break => Ok(StaticType::unit()), // TODO: change to never type?
            Expression::Identifier {
                name: ident,
                resolved,
            } => {
                if ident == "None" {
                    // THIS IS VERY UNHINGED
                    return Ok(StaticType::Option);
                }
                let binding = self.scope_tree.get_binding_any(ident).ok_or_else(|| {
                    AnalysisError::identifier_not_previously_declared(ident, *span)
                })?;

                *resolved = Binding::Resolved(binding);

                Ok(self.scope_tree.get_type(binding).clone())
            }
            Expression::Statement(inner) => {
                self.analyse(inner)?;
                Ok(StaticType::unit())
            }
            Expression::Logical { left, right, .. } => {
                self.analyse(left)?; // TODO: throw error if type does not match bool?
                self.analyse(right)?; // TODO: throw error if type does not match bool?
                Ok(StaticType::Bool)
            }
            Expression::Grouping(expr) => self.analyse(expr),
            Expression::VariableDeclaration { l_value, value } => {
                let typ = self.analyse(value)?;
                self.resolve_lvalue_declarative(l_value, typ)?;
                Ok(StaticType::unit()) // TODO: never type here?
            }
            Expression::Assignment { l_value, r_value } => {
                self.resolve_lvalue(l_value, *span)?;
                self.analyse(r_value)?;
                Ok(StaticType::unit())
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
                resolved_assign_operation,
                resolved_operation,
            } => {
                let left_type = self.resolve_lvalue_as_ident(l_value, *span)?;
                let right_type = self.analyse(r_value)?;
                let arg_types = vec![left_type, right_type];

                *resolved_assign_operation = self
                    .scope_tree
                    .resolve_function2(&format!("{operation}="), &arg_types);
                *resolved_operation = self.scope_tree.resolve_function2(operation, &arg_types);

                if let Binding::None = resolved_operation {
                    return Err(AnalysisError::function_not_found(
                        operation, &arg_types, *span,
                    ));
                }

                Ok(StaticType::unit())
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                parameters,
                body,
                ..
            } => {
                // TODO: figuring out the type signature of function declarations is the rest of the owl
                let param_types: Vec<StaticType> =
                    std::iter::repeat_n(StaticType::Any, extract_argument_arity(parameters))
                        .collect();

                self.scope_tree.new_scope();
                self.resolve_parameters_declarative(parameters);

                let return_type = self.analyse(body)?;
                self.scope_tree.destroy_scope();

                let function_type = StaticType::Function {
                    parameters: Some(param_types.clone()),
                    return_type: Box::new(return_type),
                };

                if let Some(name) = name {
                    // TODO: is this correct, for now we just always create a new binding, we could
                    //       also produce an error if we are generating a conflicting binding
                    *resolved_name = Some(
                        self.scope_tree
                            .create_local_binding(name.clone(), function_type.clone()),
                    );
                }

                Ok(function_type)
            }
            Expression::Block { statements } => {
                self.scope_tree.new_scope();
                let mut last = None;
                for s in statements {
                    last = Some(self.analyse(s)?);
                }
                self.scope_tree.destroy_scope();

                Ok(last.unwrap_or_else(StaticType::unit))
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.analyse(condition)?;
                let true_type = self.analyse(on_true)?;
                let false_type = if let Some(on_false) = on_false {
                    Some(self.analyse(on_false)?)
                } else {
                    None
                };

                if false_type.is_none() {
                    if true_type != StaticType::unit() {
                        // TODO: Emit warning for not using a semicolon in this if
                    }

                    Ok(StaticType::unit())
                } else if let Some(false_type) = false_type
                    && false_type == true_type
                {
                    Ok(true_type)
                } else {
                    // TODO: maybe create a warning to show the user they're doing something cursed
                    // TODO: figure out the nearest common ancestor for true_type and false_type
                    Ok(StaticType::Any)
                }
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                self.analyse(expression)?;
                self.analyse(loop_body)?;
                Ok(StaticType::unit())
            }
            Expression::For { iterations, body } => {
                self.resolve_for_iterations(iterations, body)?;

                match &**body {
                    // for now this is good enough?
                    ForBody::Block(_) => Ok(StaticType::unit()),
                    ForBody::List(_) => Ok(StaticType::List),
                    ForBody::Map { .. } => Ok(StaticType::Map),
                }
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let mut type_sig = Vec::with_capacity(arguments.len());
                for a in arguments {
                    type_sig.push(self.analyse(a)?);
                }

                let StaticType::Function { return_type, .. } =
                    self.resolve_function_with_argument_types(function, &type_sig, *span)?
                else {
                    unreachable!(
                        "resolve_function_with_argument_types should guarantee us a function type"
                    );
                };

                Ok(*return_type)
            }
            Expression::Index { index, value } => {
                self.analyse(index)?;
                self.analyse(value)?;
                // TODO: figure out the type here
                Ok(StaticType::Any)
            }
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse(v)?);
                }

                Ok(StaticType::Tuple(types))
            }
            Expression::List { values } => {
                for v in values {
                    self.analyse(v)?;
                }

                Ok(StaticType::List)
            }
            Expression::Map { values, default } => {
                for (key, value) in values {
                    self.analyse(key)?;
                    if let Some(value) = value {
                        self.analyse(value)?;
                    }
                }

                if let Some(default) = default {
                    self.analyse(default)?;
                }

                Ok(StaticType::Map)
            }
            Expression::Return { value } => {
                self.analyse(value)?;
                Ok(StaticType::unit()) // TODO or never?
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse(start)?;
                }
                if let Some(end) = end {
                    self.analyse(end)?;
                }

                Ok(StaticType::Iterator)
            }
        }
    }
    fn resolve_function_with_argument_types(
        &mut self,
        ident: &mut ExpressionLocation,
        argument_types: &[StaticType],
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        let ExpressionLocation {
            expression: Expression::Identifier { name, resolved },
            ..
        } = ident
        else {
            // It's possible that we're not trying to invoke an identifier `foo()` but instead we're
            // invoking a value like `get_function()()` so in this case we just continue like normal?
            return self.analyse(ident);
        };

        let binding = self.scope_tree.resolve_function2(name, argument_types);

        let out_type = match &binding {
            Binding::None => {
                return Err(AnalysisError::function_not_found(
                    name,
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

        *resolved = binding;

        Ok(out_type)
    }
    fn resolve_for_iterations(
        &mut self,
        iterations: &mut [ForIteration],
        body: &mut ForBody,
    ) -> Result<(), AnalysisError> {
        let Some((iteration, tail)) = iterations.split_first_mut() else {
            unreachable!("because this function is never called with an empty slice");
        };

        let mut do_destroy = false;
        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                self.analyse(sequence)?;

                self.scope_tree.new_scope();

                // TODO: inferring this as any is a massive cop-out
                self.resolve_lvalue_declarative(l_value, StaticType::Any)?;
                do_destroy = true;
            }
            ForIteration::Guard(expr) => {
                self.analyse(expr)?;
            }
        }

        if !tail.is_empty() {
            self.resolve_for_iterations(tail, body)?
        } else {
            match body {
                ForBody::Block(block) => {
                    self.analyse(block)?;
                }
                ForBody::List(list) => {
                    self.analyse(list)?;
                }
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    self.analyse(key)?;
                    if let Some(value) = value {
                        self.analyse(value)?;
                    }

                    if let Some(default) = default {
                        self.analyse(default)?;
                    }
                }
            }
        }

        if do_destroy {
            self.scope_tree.destroy_scope();
        }

        Ok(())
    }

    fn resolve_lvalue_as_ident(
        &mut self,
        lvalue: &mut Lvalue,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        let Lvalue::Identifier {
            identifier,
            resolved,
        } = lvalue
        else {
            return Err(AnalysisError::lvalue_required_to_be_single_identifier(span));
        };

        let Some(target) = self.scope_tree.get_binding_any(&identifier) else {
            return Err(AnalysisError::identifier_not_previously_declared(
                identifier, span,
            ));
        };

        *resolved = Some(target);

        Ok(self.scope_tree.get_type(target).clone())
    }

    fn resolve_lvalue(&mut self, lvalue: &mut Lvalue, span: Span) -> Result<(), AnalysisError> {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
            } => {
                let Some(target) = self.scope_tree.get_binding_any(identifier) else {
                    return Err(AnalysisError::identifier_not_previously_declared(
                        identifier, span,
                    ));
                };

                *resolved = Some(target);
            }
            Lvalue::Index { index, value } => {
                self.analyse(index)?;
                self.analyse(value)?;
            }
            Lvalue::Sequence(seq) => {
                for sub_lvalue in seq {
                    self.resolve_lvalue(sub_lvalue, span)?
                }
            }
        }

        Ok(())
    }

    /// Resolve expressions as arguments to a function and return the function arity
    fn resolve_parameters_declarative(&mut self, arguments: &mut ExpressionLocation) {
        let ExpressionLocation {
            expression: Expression::Tuple { values },
            ..
        } = arguments
        else {
            panic!("expected arguments to be tuple");
        };

        for arg in values {
            let ExpressionLocation {
                expression: Expression::Identifier { name, resolved },
                ..
            } = arg
            else {
                panic!("expected tuple values to be ident");
            };

            // TODO: big challenge how do we figure out the function parameter types?
            //       it seems like this is something we need an HM like system for!?
            *resolved = Binding::Resolved(
                self.scope_tree
                    .create_local_binding((*name).to_string(), StaticType::Any),
            );
        }
    }
    fn resolve_lvalue_declarative(
        &mut self,
        lvalue: &mut Lvalue,
        typ: StaticType,
    ) -> Result<(), AnalysisError> {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
            } => {
                *resolved = Some(
                    self.scope_tree
                        .create_local_binding(identifier.clone(), typ),
                );
            }
            Lvalue::Index { index, value } => {
                self.analyse(index)?;
                self.analyse(value)?;
            }
            Lvalue::Sequence(seq) => {
                for sub_lvalue in seq {
                    self.resolve_lvalue_declarative(sub_lvalue, typ.clone())?
                }
            }
        }

        Ok(())
    }
}

fn extract_argument_arity(arguments: &ExpressionLocation) -> usize {
    let ExpressionLocation {
        expression: Expression::Tuple { values },
        ..
    } = arguments
    else {
        panic!("expected arguments to be tuple");
    };

    values.len()
}

#[derive(Debug)]
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
            .unwrap_or(Binding::None)
    }
    fn resolve_function(&mut self, ident: &str, sig: &[StaticType]) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_function(ident, sig) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.find_function(ident, sig)?,
                });
            }
        }
    }

    fn create_local_binding(&mut self, ident: String, typ: StaticType) -> ResolvedVar {
        ResolvedVar::Captured {
            slot: self.scopes[self.current_scope_idx].allocate(ident, typ),
            depth: 0,
        }
    }
}

#[derive(Debug)]
struct Scope {
    parent_idx: Option<usize>,
    identifiers: Vec<(String, StaticType)>,
}

impl Scope {
    fn new(parent_idx: Option<usize>) -> Self {
        Self {
            parent_idx,
            identifiers: Default::default(),
        }
    }

    pub fn find_slot_by_name(&self, find_ident: &str) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|(ident, _)| ident == find_ident)
    }

    fn find_function_candidates(&self, find_ident: &str, find_types: &[StaticType]) -> Vec<usize> {
        self.identifiers.iter()
            .enumerate()
            .rev()
            .filter_map(|(slot, (ident, typ))| {
                if ident != find_ident {
                    return None;
                }

                // If the thing is not a function we're not interested
                let StaticType::Function { parameters, .. } = typ else {
                    return None;
                };

                let Some(param_types) = parameters else {
                    // If this branch happens then the function we're matching against is variadic meaning it's always a match
                    debug_assert!(false, "we should never be calling find_function_candidates if there were variadic matches");
                    // TODO: Change to unreachable?
                    return Some(slot);
                };

                let is_good = param_types.len() == find_types.len()
                    && param_types.iter().zip(find_types.iter()).all(|(typ_1, typ_2)| !typ_1.is_incompatible_with(typ_2));

                is_good.then_some(slot)
            })
            .collect()
    }
    fn find_function(&self, find_ident: &str, find_types: &[StaticType]) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|(ident, typ)| ident == find_ident && typ.is_fn_and_matches(find_types))
    }

    fn allocate(&mut self, name: String, typ: StaticType) -> usize {
        self.identifiers.push((name, typ));
        // Slot is just the length of the list minus one
        self.identifiers.len() - 1
    }
}
#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[error("{text}")]
pub struct AnalysisError {
    text: String,
    #[label("related to this")]
    span: Span,
}

impl AnalysisError {
    fn lvalue_required_to_be_single_identifier(span: Span) -> Self {
        Self {
            text: "This lvalue is required to be a single identifier".to_string(),
            span,
        }
    }

    fn function_not_found(ident: &str, types: &[StaticType], span: Span) -> Self {
        Self {
            text: format!(
                "No function called '{ident}' found that matches the arguments {}",
                types.iter().join(", ")
            ),
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
