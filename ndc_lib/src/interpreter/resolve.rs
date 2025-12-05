use crate::ast::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, ResolvedVar};
use crate::lexer::Span;

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[error("{text}")]
pub struct ResolveError {
    text: String,
    #[label("related to this")]
    span: Span,
}

impl ResolveError {
    fn identifier_not_previously_declared(ident: &str, span: Span) -> Self {
        Self {
            text: format!("Identifier {ident} has not previously been declared"),
            span,
        }
    }
}

pub fn resolve_pass(
    ExpressionLocation { expression, span }: &mut ExpressionLocation,
    lexical_data: &mut LexicalData,
) -> Result<(), ResolveError> {
    match expression {
        Expression::BoolLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::Int64Literal(_)
        | Expression::Float64Literal(_)
        | Expression::BigIntLiteral(_)
        | Expression::ComplexLiteral(_)
        | Expression::Continue
        | Expression::Break => { /* nothing to do here */ }
        Expression::Identifier {
            name: ident,
            resolved,
        } => {
            if ident == "None" {
                // THIS IS VERY UNHINGED
                return Ok(());
            }
            let binding = lexical_data
                .get_binding_any(ident)
                .ok_or_else(|| ResolveError::identifier_not_previously_declared(ident, *span))?;

            *resolved = Some(binding)
        }
        Expression::Statement(inner) => {
            resolve_pass(inner, lexical_data)?;
        }
        Expression::Logical { left, right, .. } => {
            resolve_pass(left, lexical_data)?;
            resolve_pass(right, lexical_data)?;
        }
        Expression::Grouping(expr) => {
            resolve_pass(expr, lexical_data)?;
        }
        Expression::VariableDeclaration { l_value, value } => {
            resolve_pass(value, lexical_data)?;
            resolve_lvalue_declarative(l_value, lexical_data)?;
        }
        Expression::Assignment { l_value, r_value } => {
            resolve_lvalue(l_value, *span, lexical_data)?;
            resolve_pass(r_value, lexical_data)?;
        }
        Expression::OpAssignment {
            l_value,
            r_value,
            operation,
            resolved_assign_operation,
            resolved_operation,
        } => {
            resolve_lvalue(l_value, *span, lexical_data)?;
            resolve_pass(r_value, lexical_data)?;

            // In this future should be able to use our type system to figure out which specific
            // instance of the function we need. When this is the case we can stop inserting both
            // versions in the OpAssignment expression and just figure out the correct one or error
            // if there is none
            let op_assign_ident = LexicalIdentifier::Function {
                name: format!("{operation}="),
                arity: Some(2),
            };

            if let Some(binding) = lexical_data.get_binding(&op_assign_ident) {
                *resolved_assign_operation = Some(binding);
            }

            let operation_ident = LexicalIdentifier::Function {
                name: (*operation).clone(),
                arity: Some(2),
            };

            if let Some(binding) = lexical_data.get_binding(&operation_ident) {
                *resolved_operation = Some(binding);
            } else {
                // For now, we require that the normal operation is present and the special assignment operation is optional
                return Err(ResolveError::identifier_not_previously_declared(
                    operation, *span,
                ));
            }
        }
        Expression::FunctionDeclaration {
            name,
            resolved_name,
            arguments,
            body,
            ..
        } => {
            if let Some(name) = name {
                let function_ident = LexicalIdentifier::Function {
                    name: (*name).clone(),
                    arity: Some(extract_argument_arity(arguments)),
                };

                *resolved_name = if let Some(binding) =
                    lexical_data.get_or_create_local_binding(function_ident.clone())
                {
                    Some(binding)
                } else {
                    Some(lexical_data.create_local_binding(function_ident))
                }
            }

            lexical_data.new_scope();
            resolve_arguments_declarative(arguments, lexical_data);

            resolve_pass(body, lexical_data)?;
            lexical_data.destroy_scope();
        }
        Expression::Block { statements } => {
            lexical_data.new_scope();
            for s in statements {
                resolve_pass(s, lexical_data)?;
            }
            lexical_data.destroy_scope();
        }
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            resolve_pass(condition, lexical_data)?;
            resolve_pass(on_true, lexical_data)?;
            if let Some(on_false) = on_false {
                resolve_pass(on_false, lexical_data)?;
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            resolve_pass(expression, lexical_data)?;
            resolve_pass(loop_body, lexical_data)?;
        }
        Expression::For { iterations, body } => {
            resolve_for_iterations(iterations, body, lexical_data)?;
        }
        Expression::Call {
            function,
            arguments,
        } => {
            resolve_function_ident_arity(function, arguments.len(), *span, lexical_data)?;
            for a in arguments {
                resolve_pass(a, lexical_data)?;
            }
        }
        Expression::Index { index, value } => {
            resolve_pass(index, lexical_data)?;
            resolve_pass(value, lexical_data)?;
        }
        Expression::Tuple { values } | Expression::List { values } => {
            for v in values {
                resolve_pass(v, lexical_data)?;
            }
        }
        Expression::Map { values, default } => {
            for (key, value) in values {
                resolve_pass(key, lexical_data)?;
                if let Some(value) = value {
                    resolve_pass(value, lexical_data)?;
                }
            }

            if let Some(default) = default {
                resolve_pass(default, lexical_data)?;
            }
        }
        Expression::Return { value } => {
            resolve_pass(value, lexical_data)?;
        }
        Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
            if let Some(start) = start {
                resolve_pass(start, lexical_data)?;
            }
            if let Some(end) = end {
                resolve_pass(end, lexical_data)?;
            }
        }
    }

    Ok(())
}
fn resolve_function_ident_arity(
    ident: &mut ExpressionLocation,
    arity: usize,
    span: Span,
    lexical_data: &mut LexicalData,
) -> Result<(), ResolveError> {
    let ExpressionLocation {
        expression: Expression::Identifier { name, resolved },
        ..
    } = ident
    else {
        // It's possible that we're not trying to invoke an identifier `foo()` but instead we're
        // invoking a value like `get_function()()` so in this case we just continue like normal?
        return resolve_pass(ident, lexical_data);
    };

    let binding = lexical_data
        .get_binding(&LexicalIdentifier::Function {
            name: name.clone(),
            arity: Some(arity),
        })
        .or_else(|| {
            lexical_data.get_binding(&LexicalIdentifier::Function {
                name: name.clone(),
                arity: None,
            })
        })
        // NOTE: for now if we can't find a function that matches we just bind to a variable with that name (if possible)
        //       this fixes some cases until we have full type checking
        .or_else(|| (lexical_data).get_binding_any(name))
        .ok_or_else(|| ResolveError::identifier_not_previously_declared(name, span))?;

    *resolved = Some(binding);

    Ok(())
}
fn resolve_for_iterations(
    iterations: &mut [ForIteration],
    body: &mut ForBody,
    lexical_data: &mut LexicalData,
) -> Result<(), ResolveError> {
    let Some((iteration, tail)) = iterations.split_first_mut() else {
        unreachable!("because this function is never called with an empty slice");
    };

    let mut do_destroy = false;
    match iteration {
        ForIteration::Iteration { l_value, sequence } => {
            resolve_pass(sequence, lexical_data)?;
            lexical_data.new_scope();
            resolve_lvalue_declarative(l_value, lexical_data)?;
            do_destroy = true;
        }
        ForIteration::Guard(expr) => {
            resolve_pass(expr, lexical_data)?;
        }
    }

    if !tail.is_empty() {
        resolve_for_iterations(tail, body, lexical_data)?
    } else {
        match body {
            ForBody::Block(block) => {
                resolve_pass(block, lexical_data)?;
            }
            ForBody::List(list) => {
                resolve_pass(list, lexical_data)?;
            }
            ForBody::Map {
                key,
                value,
                default,
            } => {
                resolve_pass(key, lexical_data)?;
                if let Some(value) = value {
                    resolve_pass(value, lexical_data)?;
                }

                if let Some(default) = default {
                    resolve_pass(default, lexical_data)?;
                }
            }
        }
    }

    if do_destroy {
        lexical_data.destroy_scope();
    }

    Ok(())
}

fn resolve_lvalue(
    lvalue: &mut Lvalue,
    span: Span,
    lexical_data: &mut LexicalData,
) -> Result<(), ResolveError> {
    match lvalue {
        Lvalue::Identifier {
            identifier,
            resolved,
        } => {
            let Some(target) = lexical_data.get_binding(&identifier.clone().into()) else {
                return Err(ResolveError::identifier_not_previously_declared(
                    identifier, span,
                ));
            };

            *resolved = Some(target);
        }
        Lvalue::Index { index, value } => {
            resolve_pass(index, lexical_data)?;
            resolve_pass(value, lexical_data)?;
        }
        Lvalue::Sequence(seq) => {
            for sub_lvalue in seq {
                resolve_lvalue(sub_lvalue, span, lexical_data)?
            }
        }
    }

    Ok(())
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
/// Resolve expressions as arguments to a function and return the function arity
fn resolve_arguments_declarative(
    arguments: &mut ExpressionLocation,
    lexical_data: &mut LexicalData,
) {
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

        *resolved = Some(
            lexical_data.create_local_binding(LexicalIdentifier::Variable {
                name: (*name).clone(),
            }),
        );
    }
}
fn resolve_lvalue_declarative(
    lvalue: &mut Lvalue,
    lexical_data: &mut LexicalData,
) -> Result<(), ResolveError> {
    match lvalue {
        Lvalue::Identifier {
            identifier,
            resolved,
        } => {
            *resolved = Some(
                lexical_data.create_local_binding(LexicalIdentifier::Variable {
                    name: (*identifier).clone(),
                }),
            );
        }
        Lvalue::Index { index, value } => {
            resolve_pass(index, lexical_data)?;
            resolve_pass(value, lexical_data)?;
        }
        Lvalue::Sequence(seq) => {
            for sub_lvalue in seq {
                resolve_lvalue_declarative(sub_lvalue, lexical_data)?
            }
        }
    }

    Ok(())
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum LexicalIdentifier {
    Variable { name: String },
    Function { name: String, arity: Option<usize> },
}

impl From<&str> for LexicalIdentifier {
    fn from(value: &str) -> Self {
        Self::Variable { name: value.into() }
    }
}
impl From<String> for LexicalIdentifier {
    fn from(value: String) -> Self {
        Self::Variable { name: value }
    }
}

#[derive(Debug)]
pub struct LexicalData {
    current_scope_idx: usize,
    global_scope: LexicalScope,
    scopes: Vec<LexicalScope>,
}

impl LexicalData {
    pub fn from_global_scope(global_scope_map: Vec<LexicalIdentifier>) -> Self {
        Self {
            current_scope_idx: 0,
            global_scope: LexicalScope {
                parent_idx: None,
                identifiers: global_scope_map,
            },
            scopes: vec![LexicalScope::new(None)],
        }
    }

    fn new_scope(&mut self) -> &LexicalScope {
        let old_scope_idx = self.current_scope_idx;
        self.current_scope_idx = self.scopes.len();
        let new_scope = LexicalScope::new(Some(old_scope_idx));
        self.scopes.push(new_scope);
        &self.scopes[self.current_scope_idx]
    }

    fn destroy_scope(&mut self) {
        let next = self.scopes[self.current_scope_idx]
            .parent_idx
            .expect("tried to destroy scope while there were none");
        self.current_scope_idx = next;
    }

    fn get_or_create_local_binding(&mut self, ident: LexicalIdentifier) -> Option<ResolvedVar> {
        Some(ResolvedVar::Captured {
            slot: self.scopes[self.current_scope_idx].get_or_allocate(ident),
            depth: 0,
        })
    }

    fn get_binding_any(&mut self, ident: &str) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].get_slot_by_name(ident) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.get_slot_by_name(ident)?,
                });
            }
        }
    }

    fn get_binding(&mut self, name: &LexicalIdentifier) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].get_slot_by_identifier(name) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.get_slot_by_identifier(name)?,
                });
            }
        }
    }

    fn create_local_binding(&mut self, ident: LexicalIdentifier) -> ResolvedVar {
        ResolvedVar::Captured {
            slot: self.scopes[self.current_scope_idx].allocate(ident),
            depth: 0,
        }
    }
}

#[derive(Debug)]
struct LexicalScope {
    parent_idx: Option<usize>,
    identifiers: Vec<LexicalIdentifier>,
}

impl LexicalScope {
    fn new(parent_idx: Option<usize>) -> Self {
        Self {
            parent_idx,
            identifiers: Default::default(),
        }
    }

    pub fn get_slot_by_name(&self, find_ident: &str) -> Option<usize> {
        for (slot, ident_in_scope) in self.identifiers.iter().enumerate().rev() {
            let name_in_scope = match ident_in_scope {
                LexicalIdentifier::Variable { name } | LexicalIdentifier::Function { name, .. } => {
                    name
                }
            };

            if name_in_scope == find_ident {
                return Some(slot);
            }
        }

        None
    }

    /// Either returns the slot in this current scope or creates a new one
    pub fn get_or_allocate(&mut self, ident: LexicalIdentifier) -> usize {
        if let Some(idx) = self.get_slot_by_identifier(&ident) {
            idx
        } else {
            self.allocate(ident)
        }
    }

    fn get_slot_by_identifier(&mut self, find_ident: &LexicalIdentifier) -> Option<usize> {
        for (slot, ident_in_scope) in self.identifiers.iter().enumerate().rev() {
            if ident_in_scope == find_ident {
                return Some(slot);
            }
        }

        None
    }

    fn allocate(&mut self, name: LexicalIdentifier) -> usize {
        self.identifiers.push(name);
        // Slot is just the length of the list
        self.identifiers.len() - 1
    }
}
