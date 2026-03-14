use crate::function::StaticType;
use crate::semantic::ScopeTree;
use itertools::Itertools;
use ndc_lexer::Span;
use ndc_parser::{
    Binding, Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, TypeSignature,
};
use std::fmt::Debug;

#[derive(Debug)]
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
        ExpressionLocation { expression, span }: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        match expression {
            Expression::BoolLiteral(_) => Ok(StaticType::Bool),
            Expression::StringLiteral(_) => Ok(StaticType::String),
            Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => Ok(StaticType::Int),
            Expression::Float64Literal(_) => Ok(StaticType::Float),
            Expression::ComplexLiteral(_) => Ok(StaticType::Complex),
            Expression::Continue | Expression::Break => Ok(StaticType::unit()),
            Expression::Identifier {
                name: ident,
                resolved,
            } => {
                if ident == "None" {
                    // TODO: we're going to need something like HM to infer the type of option here, maybe force type annotations?
                    return Ok(StaticType::Option(Box::new(StaticType::Any)));
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
                self.resolve_lvalue_declarative(l_value, typ, *span)?;
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
                let left_type = self.resolve_single_lvalue(l_value, *span)?;
                let right_type = self.analyse(r_value)?;
                let arg_types = vec![left_type, right_type];

                *resolved_assign_operation = self
                    .scope_tree
                    .resolve_function_binding(&format!("{operation}="), &arg_types);
                *resolved_operation = self
                    .scope_tree
                    .resolve_function_binding(operation, &arg_types);

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
                type_signature,
                body,
                return_type: return_type_slot,
                captures,
                ..
            } => {
                // Pre-register the function before analysing its body so recursive calls can
                // resolve the name. The return type is unknown at this point so we use Any.
                let pre_slot = if let Some(name) = name {
                    let placeholder = StaticType::Function {
                        parameters: type_signature.types(),
                        return_type: Box::new(StaticType::Any),
                    };
                    Some(
                        self.scope_tree
                            .create_local_binding(name.clone(), placeholder),
                    )
                } else {
                    None
                };

                self.scope_tree.new_function_scope();
                let param_types = self.resolve_parameters_declarative(type_signature, *span)?;

                let return_type = self.analyse(body)?;
                *captures = self.scope_tree.current_scope_captures();
                self.scope_tree.destroy_scope();
                *return_type_slot = Some(return_type);

                let function_type = StaticType::Function {
                    parameters: Some(param_types.clone()),
                    return_type: Box::new(
                        return_type_slot
                            .clone()
                            .expect("must have a value at this point"),
                    ),
                };

                if let Some(slot) = pre_slot {
                    // TODO: is this correct, for now we just always create a new binding, we could
                    //       also produce an error if we are generating a conflicting binding
                    self.scope_tree
                        .update_binding_type(slot, function_type.clone());
                    *resolved_name = Some(slot);
                }

                Ok(function_type)
            }
            Expression::Block { statements } => {
                self.scope_tree.new_block_scope();
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
                    self.analyse(on_false)?
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
                self.analyse(expression)?;
                self.analyse(loop_body)?;
                Ok(StaticType::unit())
            }
            Expression::For { iterations, body } => {
                Ok(self.resolve_for_iterations(iterations, body, *span)?)
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
                    // If we couldn't resolve the identifier to a function we have to just assume that
                    // whatever identifier we did find is a function at runtime and will return Any
                    return Ok(StaticType::Any);
                };

                Ok(*return_type)
            }
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse(v)?);
                }

                Ok(StaticType::Tuple(types))
            }
            Expression::List { values } => {
                let element_type = self.analyse_multiple_expression_with_same_type(values)?;

                // TODO: for now if we encounter an empty list expression we say the list is generic over Any but this clearly is not a good solution
                Ok(StaticType::List(Box::new(
                    element_type.unwrap_or(StaticType::Any),
                )))
            }
            Expression::Map { values, default } => {
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in values {
                    // let map = %{
                    //     "key": 1,
                    //     10: 1,
                    // }
                    if let Some(key_type) = &mut key_type {
                        let next_type = self.analyse(key)?;
                        *key_type = key_type.lub(&next_type);
                    } else {
                        key_type = Some(self.analyse(key)?);
                    }
                    if let Some(value) = value {
                        if let Some(value_type) = &mut value_type {
                            let next_type = self.analyse(value)?;
                            if &next_type != value_type {
                                *value_type = value_type.lub(&next_type);
                            }
                        } else {
                            value_type = Some(self.analyse(value)?);
                        }
                    }
                }

                if let Some(default) = default {
                    self.analyse(default)?;
                }

                // TODO: defaulting to Any here is surely going to bite us later
                Ok(StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or_else(StaticType::unit)),
                })
            }
            // Return evaluates to the type of the expression it returns, which makes type checking easier!
            // Actually it doesn't seem to make it any easier
            Expression::Return { value } => self.analyse(value),
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse(start)?;
                }
                if let Some(end) = end {
                    self.analyse(end)?;
                }

                Ok(StaticType::Iterator(Box::new(StaticType::Int)))
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

        // println!("resolve fn {name} {}", argument_types.iter().join(", "));

        let binding = self
            .scope_tree
            .resolve_function_binding(name, argument_types);

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
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        let Some((iteration, tail)) = iterations.split_first_mut() else {
            unreachable!("because this function is never called with an empty slice");
        };

        let mut do_destroy = false;
        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                let sequence_type = self.analyse(sequence)?;

                self.scope_tree.new_iteration_scope();

                // TODO: when we give type parameters to all instances of sequence we can correctly infer StaticType::Any in this position
                self.resolve_lvalue_declarative(
                    l_value,
                    sequence_type
                        .sequence_element_type()
                        .unwrap_or(StaticType::Any),
                    span,
                )?;
                do_destroy = true;
            }
            ForIteration::Guard(expr) => {
                self.analyse(expr)?;
            }
        }

        let out_type = if !tail.is_empty() {
            self.resolve_for_iterations(tail, body, span)?
        } else {
            match body {
                ForBody::Block(block) => {
                    self.analyse(block)?;
                    StaticType::unit()
                }
                ForBody::List(list) => StaticType::List(Box::new(self.analyse(list)?)),
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    let key_type = self.analyse(key)?;
                    let value_type = if let Some(value) = value {
                        self.analyse(value)?
                    } else {
                        StaticType::unit()
                    };

                    if let Some(default) = default {
                        self.analyse(default)?;
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
            Lvalue::Index { index, value, .. } => {
                self.analyse(index)?;
                let type_of_index_target = self.analyse(value)?;

                type_of_index_target
                    .index_element_type()
                    .ok_or_else(|| AnalysisError::unable_to_index_into(&type_of_index_target, span))
            }
            Lvalue::Sequence(_) => {
                Err(AnalysisError::lvalue_required_to_be_single_identifier(span))
            }
        }
    }

    fn resolve_lvalue(&mut self, lvalue: &mut Lvalue, span: Span) -> Result<(), AnalysisError> {
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
            Lvalue::Index {
                index,
                value,
                resolved_set,
            } => {
                self.analyse(index)?;
                self.analyse(value)?;
                *resolved_set = Some(self.scope_tree.resolve_function_binding("[]=", &[]));
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
    fn resolve_parameters_declarative(
        &mut self,
        type_signature: &TypeSignature,
        span: Span,
    ) -> Result<Vec<StaticType>, AnalysisError> {
        let TypeSignature::Exact(parameters) = type_signature else {
            return Ok(vec![]);
        };

        let mut types: Vec<StaticType> = Vec::new();
        let mut seen_names: Vec<&str> = Vec::new();

        for param in parameters {
            // TODO: big challenge how do we figure out the function parameter types?
            //       it seems like this is something we need an HM like system for!?
            let resolved_type = StaticType::Any;
            types.push(resolved_type.clone());
            if seen_names.contains(&param.name.as_str()) {
                return Err(AnalysisError::parameter_redefined(&param.name, span));
            }
            seen_names.push(&param.name);

            self.scope_tree
                .create_local_binding(param.name.clone(), resolved_type);
        }

        Ok(types)
    }
    fn resolve_lvalue_declarative(
        &mut self,
        lvalue: &mut Lvalue,
        typ: StaticType,
        span: Span,
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
            Lvalue::Index { index, value, .. } => {
                self.analyse(index)?;
                self.analyse(value)?;
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
                    )?
                }
            }
        }

        Ok(())
    }
    fn analyse_multiple_expression_with_same_type(
        &mut self,
        expressions: &mut Vec<ExpressionLocation>,
    ) -> Result<Option<StaticType>, AnalysisError> {
        let mut element_type: Option<StaticType> = None;

        for expression in expressions {
            if let Some(element_type) = &mut element_type {
                let following_type = self.analyse(expression)?;

                *element_type = element_type.lub(&following_type);
            } else {
                element_type = Some(self.analyse(expression)?);
            }
        }

        Ok(element_type)
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

    fn identifier_not_previously_declared(ident: &str, span: Span) -> Self {
        Self {
            text: format!("Identifier {ident} has not previously been declared"),
            span,
        }
    }
}
