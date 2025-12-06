use crate::ast::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, ResolvedVar};
use crate::interpreter::function::StaticType;
use crate::lexer::Span;
use itertools::Itertools;
use std::iter::repeat;

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

                *resolved = Some(binding);

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
                let op_assign_arg_types = vec![left_type, right_type];

                if let Some(binding) = self
                    .scope_tree
                    .get_function_binding(&format!("{operation}="), &op_assign_arg_types)
                {
                    *resolved_assign_operation = Some(binding);
                }

                if let Some(binding) = self
                    .scope_tree
                    .get_function_binding(&operation, &op_assign_arg_types)
                {
                    *resolved_operation = Some(binding);

                    Ok(StaticType::unit())
                } else {
                    // For now, we require that the normal operation is present and the special assignment operation is optional
                    Err(AnalysisError::identifier_not_previously_declared(
                        operation, *span,
                    ))
                }
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                parameters,
                body,
                ..
            } => {
                // TODO: figuring out the type signature of function declarations is the rest of the owl
                let param_types: Vec<StaticType> = repeat(StaticType::Any)
                    .take(extract_argument_arity(parameters))
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
                    // TODO: is this correct, for now we just always create a new binding, we could also produce an error if we are generating a conflicting binding
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
                let false_typ = if let Some(on_false) = on_false {
                    Some(self.analyse(on_false)?)
                } else {
                    None
                };

                if false_typ.is_none() {
                    if true_type != StaticType::unit() {
                        // TODO: Emit warning for not using a semicolon in this if
                    }

                    Ok(StaticType::unit())
                } else if let Some(false_type) = false_typ
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

        let binding = self
            .scope_tree
            .get_function_binding(name, argument_types)
            // TODO: instead of throwing an error we can emit a dynamic binding
            .ok_or_else(|| AnalysisError::function_not_found(name, argument_types, span))?;

        *resolved = Some(binding);

        Ok(self.scope_tree.get_type(binding).clone())
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
                let Some(target) = self.scope_tree.get_binding_any(&identifier) else {
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
            *resolved = Some(
                self.scope_tree
                    .create_local_binding(name.to_string(), StaticType::Any),
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

    // fn resolve_type(
    //     &mut self,
    //     ExpressionLocation { expression, .. }: &ExpressionLocation,
    // ) -> StaticType {
    //     match expression {
    //         Expression::BoolLiteral(_) | Expression::Logical { .. } => StaticType::Bool,
    //         Expression::StringLiteral(_) => StaticType::String,
    //         Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => StaticType::Int,
    //         Expression::Float64Literal(_) => StaticType::Float,
    //         Expression::ComplexLiteral(_) => StaticType::Complex,
    //         Expression::Identifier { resolved, name } => {
    //             println!(
    //                 "resolving name: {name}, {resolved:?}\n\n{}\n\n{:?}",
    //                 self.scope_tree.current_scope_idx, self.scope_tree.scopes
    //             );
    //             self
    //             .scope_tree
    //             .get_type(resolved.unwrap_or_else(|| {
    //             panic!(
    //                 "previously mentioned identifier {name} was not resolved during type resolution"
    //             )
    //         }))
    //         }
    //         Expression::Statement(_)
    //         | Expression::While { .. }
    //         | Expression::Break
    //         | Expression::Assignment { .. } => StaticType::unit(),
    //         Expression::Grouping(expr) | Expression::Return { value: expr } => {
    //             self.resolve_type(expr)
    //         }
    //         Expression::VariableDeclaration { .. } => {
    //             debug_assert!(
    //                 false,
    //                 "trying to get type of variable declaration, does this make sense?"
    //             );
    //             StaticType::unit() // specifically unit tuple
    //         }
    //         Expression::OpAssignment { .. } => {
    //             debug_assert!(
    //                 false,
    //                 "trying to get type of op assignment, does this make sense?"
    //             );
    //             StaticType::unit() // specifically unit tuple
    //         }
    //         Expression::FunctionDeclaration { .. } => StaticType::Function,
    //         Expression::Block { statements } => statements
    //             .iter()
    //             .last()
    //             .map_or(StaticType::unit(), |last| self.resolve_type(last)),
    //         Expression::If {
    //             on_true, on_false, ..
    //         } => {
    //             let on_false_type = on_false
    //                 .as_ref()
    //                 .map_or(StaticType::unit(), |expr| self.resolve_type(expr));
    //
    //             assert_eq!(
    //                 self.resolve_type(on_true),
    //                 on_false_type,
    //                 "if branches have different types"
    //             );
    //             on_false_type
    //         }
    //         Expression::For { body, .. } => ,
    //         Expression::Call {
    //             function: _,
    //             arguments: _,
    //         } => {
    //             // TODO: Okay this is actually hard
    //             StaticType::Any
    //         }
    //         Expression::Index { .. } => {
    //             // TODO: this is also hard since we don't have generics
    //             StaticType::Any
    //         }
    //         Expression::Tuple { .. } => {
    //             // TODO: this is hard
    //             StaticType::Any
    //         }
    //         Expression::List { .. } => StaticType::List,
    //         Expression::Map { .. } => StaticType::Map,
    //         Expression::Continue => StaticType::Any, // Maybe we need a Never type?
    //         Expression::RangeInclusive { .. } | Expression::RangeExclusive { .. } => {
    //             StaticType::Iterator
    //         }
    //     }
    // }
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

    fn get_function_binding(&mut self, ident: &str, sig: &[StaticType]) -> Option<ResolvedVar> {
        // println!("get function binding {ident} {sig:?}");

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

    fn find_function(&mut self, find_ident: &str, find_typ: &[StaticType]) -> Option<usize> {
        self.identifiers.iter().rposition(|(ident, typ)| {
            if ident != find_ident {
                return false;
            }

            let StaticType::Function { parameters, .. } = typ else {
                return false;
            };

            let Some(param_types) = parameters else {
                // If this branch happens then the function we're matching against is variadic meaning it's always a match
                return true;
            };

            ident == find_ident
                && param_types.len() == find_typ.len()
                && find_typ
                    .iter()
                    .zip(param_types.iter())
                    .all(|(typ1, typ2)| typ1.is_compatible_with(typ2))
        })
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
                "No function named '{ident}' found that matches the arguments {}",
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
