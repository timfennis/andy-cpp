use crate::ast::{Expression, ExpressionLocation, ForBody, ForIteration, LogicalOperator, Lvalue};
use crate::hash_map::HashMap;
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::function::{Function, FunctionBody, FunctionCarrier, OverloadedFunction};
use crate::interpreter::int::Int;
use crate::interpreter::iterator::mut_value_to_iterator;
use crate::interpreter::num::Number;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::{Value, ValueType};
use crate::lexer::Span;
use index::{Offset, evaluate_as_index, get_at_index, set_at_index};
use itertools::Itertools;
use log::error;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub type EvaluationResult = Result<Value, FunctionCarrier>;

mod index;

#[allow(clippy::too_many_lines)]
pub(crate) fn evaluate_expression(
    expression_location: &ExpressionLocation,
    environment: &mut EnvironmentRef,
) -> EvaluationResult {
    let span = expression_location.span;
    let literal: Value = match &expression_location.expression {
        Expression::BoolLiteral(b) => Value::Bool(*b),
        Expression::StringLiteral(s) => Value::string(s),
        Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
        Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
        Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
        Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
        Expression::Grouping(expr) => evaluate_expression(expr, environment)?,
        Expression::Identifier { name, resolved } => {
            if name == "None" {
                return Ok(Value::none());
            }
            environment
                .borrow()
                .get(resolved.expect("identifier was not resolved before execution"))
                .borrow()
                .clone()
        }
        Expression::VariableDeclaration { l_value, value } => {
            let value = evaluate_expression(value, environment)?;
            declare_or_assign_variable(l_value, value, environment, span)?;
            Value::unit()
        }
        Expression::Assignment {
            l_value,
            r_value: value,
        } => match l_value {
            l_value @ (Lvalue::Identifier { .. } | Lvalue::Sequence(_)) => {
                let value = evaluate_expression(value, environment)?;
                declare_or_assign_variable(l_value, value, environment, span)?
            }
            Lvalue::Index {
                value: lhs_expression,
                index: index_expression,
            } => {
                let mut lhs = evaluate_expression(lhs_expression, environment)?;

                // the computation of this value may need the list that we assign to,
                // therefore the value needs to be computed before we mutably borrow the list
                // see: `bug0001_in_place_map.ndct`
                let rhs = evaluate_expression(value, environment)?;

                let index = evaluate_as_index(index_expression, environment)?;

                set_at_index(&mut lhs, rhs, index, span)?;

                Value::unit()
            }
        },
        Expression::OpAssignment {
            l_value,
            r_value,
            resolved_assign_operation,
            resolved_operation,
            ..
        } => {
            let mut operations_to_try = [
                resolved_assign_operation.map(|op| (op, true)),
                resolved_operation.map(|op| (op, false)),
            ]
            .into_iter()
            .flatten()
            .peekable();

            match l_value {
                Lvalue::Identifier {
                    identifier,
                    resolved: resolved_l_value,
                } => {
                    let resolved_l_value = resolved_l_value.expect("lvalue must be resolved");
                    let rhs = evaluate_expression(r_value, environment)?;

                    // TODO: this statement does damage which isn't reverted when for instance we can't find the function
                    let Some(lhs) = environment.borrow_mut().take(resolved_l_value) else {
                        return Err(EvaluationError::undefined_variable(identifier, span).into());
                    };

                    let mut arguments = [lhs, rhs];

                    while let Some((resolved_op, modified_in_place)) = operations_to_try.next() {
                        let operation = environment.borrow().get(resolved_op);
                        let operation_val = &*operation.borrow();

                        let Value::Function(func) = operation_val else {
                            unreachable!(
                                "the resolver pass should have guaranteed that the operation points to a function"
                            );
                        };

                        let result = match call_function(func, &mut arguments, environment, span) {
                            Err(FunctionCarrier::FunctionNotFound)
                                if operations_to_try.peek().is_none() =>
                            {
                                let argument_string =
                                    arguments.iter().map(Value::value_type).join(", ");

                                return Err(FunctionCarrier::EvaluationError(
                                    EvaluationError::new(
                                        format!(
                                            "no function called 'TODO FIGURE OUT NAME' found matches the arguments: ({argument_string})"
                                        ),
                                        span,
                                    ),
                                ));
                            }
                            Err(FunctionCarrier::FunctionNotFound) => continue,
                            eval_result => eval_result?,
                        };

                        if modified_in_place {
                            environment.borrow_mut().set(
                                resolved_l_value,
                                std::mem::replace(&mut arguments[0], Value::unit()),
                            );
                        } else {
                            environment.borrow_mut().set(resolved_l_value, result);
                        }

                        break; // LMAO!?!
                    }

                    Value::unit()
                }
                // NOTE THIS IS AN ABSOLUTE MESS BUT WE'LL FIX IT LATER
                Lvalue::Index {
                    value: lhs_expression,
                    index: index_expression,
                } => {
                    let mut lhs_value = evaluate_expression(lhs_expression, environment)?;
                    let index = evaluate_as_index(index_expression, environment)?;
                    let value_at_index = get_at_index(&lhs_value, index.clone(), span)?;

                    let right_value = evaluate_expression(r_value, environment)?;

                    while let Some((resolved_operation, modified_in_place)) =
                        operations_to_try.next()
                    {
                        let operation = environment.borrow().get(resolved_operation);
                        let operation_val = &*operation.borrow();

                        let Value::Function(func) = operation_val else {
                            unreachable!(
                                "the resolver pass should have guaranteed that the operation points to a function"
                            );
                        };

                        let result = match call_function(
                            func,
                            &mut [value_at_index.clone(), right_value.clone()],
                            environment,
                            span,
                        ) {
                            Err(FunctionCarrier::FunctionNotFound)
                                if operations_to_try.peek().is_none() =>
                            {
                                let argument_string = [value_at_index.clone(), right_value.clone()]
                                    .iter()
                                    .map(Value::value_type)
                                    .join(", ");

                                return Err(FunctionCarrier::EvaluationError(
                                    EvaluationError::new(
                                        format!(
                                            "no function called 'TODO FIGURE OUT NAME' found matches the arguments: ({argument_string})"
                                        ),
                                        span,
                                    ),
                                ));
                            }
                            Err(FunctionCarrier::FunctionNotFound) => continue,
                            eval_result => eval_result?,
                        };

                        if !modified_in_place {
                            set_at_index(&mut lhs_value, result, index.clone(), span)?;
                        }

                        break;
                    }

                    Value::unit()
                }
                Lvalue::Sequence(_) => {
                    return Err(EvaluationError::syntax_error(
                        "cannot use augmented assignment in combination with destructuring"
                            .to_string(),
                        span,
                    )
                    .into());
                }
            }
        }
        Expression::Block { statements } => {
            let mut local_scope = Environment::new_scope(environment);

            let mut value = Value::unit();
            for stm in statements {
                // TODO: the scope doesn't get dropped if this returns, breaks, or continues
                value = evaluate_expression(stm, &mut local_scope)?;
            }

            drop(local_scope);
            value
        }
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            let result = evaluate_expression(condition, environment)?;

            match (result, on_false) {
                (Value::Bool(true), _) => evaluate_expression(on_true, environment)?,
                (Value::Bool(false), Some(block)) => evaluate_expression(block, environment)?,
                (Value::Bool(false), None) => Value::unit(),
                (value, _) => {
                    return Err(EvaluationError::new(
                        format!(
                            "mismatched types: expected {}, found {}",
                            ValueType::Bool,
                            ValueType::from(&value)
                        ),
                        span,
                    )
                    .into());
                }
            }
        }
        Expression::Statement(expression) => {
            evaluate_expression(expression, environment)?;
            Value::unit()
        }
        Expression::Logical {
            operator,
            left,
            right,
        } => {
            let left = evaluate_expression(left, environment)?;
            match (operator, left) {
                (LogicalOperator::And, Value::Bool(true))
                | (LogicalOperator::Or, Value::Bool(false)) => {
                    evaluate_expression(right, environment)?
                }
                (LogicalOperator::And, Value::Bool(false)) => Value::Bool(false),
                (LogicalOperator::Or, Value::Bool(true)) => Value::Bool(true),
                (LogicalOperator::And | LogicalOperator::Or, value) => {
                    return Err(EvaluationError::new(
                        format!(
                            "Cannot apply logical operator to non bool value {}",
                            ValueType::from(&value)
                        ),
                        span,
                    )
                    .into());
                }
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            loop {
                let lit = evaluate_expression(expression, environment)?;
                if lit == Value::Bool(true) {
                    let result = evaluate_expression(loop_body, environment);
                    match result {
                        Err(FunctionCarrier::Break(value)) => return Ok(value),
                        Err(FunctionCarrier::Continue) | Ok(_) => {}
                        Err(err) => return Err(err),
                    }
                } else if lit == Value::Bool(false) {
                    break;
                } else {
                    return Err(EvaluationError::new(
                        "Expression in a while structure must return a bool".to_string(),
                        span,
                    )
                    .into());
                }
            }
            Value::unit()
        }
        Expression::Call {
            function,
            arguments,
        } => {
            let mut evaluated_args = Vec::new();

            for argument in arguments {
                evaluated_args.push(evaluate_expression(argument, environment)?);
            }

            // The Expression in `function` must either be an identifier in which case it will be looked up in the
            // environment, or it must be some expression that evaluates to a function.
            // In case the expression is an identifier we get ALL the values that match the identifier
            // ordered by the distance is the scope-hierarchy.
            let function_as_value = evaluate_expression(function, environment)?;

            return if let Value::Function(function) = function_as_value {
                match call_function(&function, &mut evaluated_args, environment, span) {
                    Err(FunctionCarrier::FunctionNotFound) => {
                        let argument_string =
                            evaluated_args.iter().map(Value::value_type).join(", ");

                        return Err(FunctionCarrier::EvaluationError(EvaluationError::new(
                            format!(
                                "no function called '{}' found matches the arguments: ({argument_string})",
                                function.borrow().name().unwrap_or("unnamed function")
                            ),
                            span,
                        )));
                    }
                    result => result,
                }
            } else {
                Err(FunctionCarrier::EvaluationError(EvaluationError::new(
                    "Failed to invoke expression as function possibly because it's not a function"
                        .to_string(),
                    span,
                )))
            };
        }
        Expression::FunctionDeclaration {
            arguments,
            body,
            resolved_name,
            pure,
            ..
        } => {
            let mut user_function = FunctionBody::Closure {
                parameter_names: arguments.try_into_parameters()?,
                body: *body.clone(),
                environment: environment.clone(),
            };

            if *pure {
                user_function = FunctionBody::Memoized {
                    cache: RefCell::default(),
                    function: Box::new(user_function),
                }
            }

            if let Some(resolved_name) = *resolved_name {
                // TODO: this is almost certainly wrong because we simply set the function at the location
                //       in memory but we want to merge it with existing function that may exist
                environment.borrow_mut().set(
                    resolved_name,
                    Value::function(Function::from_body(user_function)),
                );

                Value::unit()
            } else {
                Value::function(user_function)
            }
        }

        Expression::Tuple { values } => {
            let mut out_values = Vec::with_capacity(values.len());
            for value in values {
                out_values.push(evaluate_expression(value, environment)?);
            }

            Value::Sequence(Sequence::Tuple(Rc::new(out_values)))
        }
        Expression::List { values } => {
            let mut values_out = Vec::with_capacity(values.len());
            for expression in values {
                let v = evaluate_expression(expression, environment)?;
                values_out.push(v);
            }
            Value::Sequence(Sequence::List(Rc::new(RefCell::new(values_out))))
        }
        Expression::Map { values, default } => {
            let mut hashmap = HashMap::with_capacity(values.len());
            for (key, value) in values {
                let key = evaluate_expression(key, environment)?;
                let value = if let Some(value) = value {
                    evaluate_expression(value, environment)?
                } else {
                    Value::unit()
                };

                hashmap.insert(key, value);
            }

            let default = if let Some(default) = default {
                Some(Box::new(evaluate_expression(default, environment)?))
            } else {
                None
            };

            Value::Sequence(Sequence::Map(Rc::new(RefCell::new(hashmap)), default))
        }
        Expression::For { iterations, body } => {
            let mut out_values = Vec::new();
            let result =
                execute_for_iterations(iterations, body, &mut out_values, environment, span);

            match result {
                Err(FunctionCarrier::Break(break_value)) => return Ok(break_value),
                Err(FunctionCarrier::Continue) => unreachable!(),
                Err(err) => return Err(err),
                Ok(_) => {}
            }

            match &**body {
                ForBody::Block(_) => Value::unit(),
                ForBody::List(_) => Value::list(out_values),
                ForBody::Map {
                    key: _,
                    value: _,
                    default,
                } => Value::Sequence(Sequence::Map(
                    Rc::new(RefCell::new(
                        out_values
                            .into_iter()
                            .map(TryInto::<(Value, Value)>::try_into)
                            .collect::<Result<HashMap<Value, Value>, _>>()
                            .into_evaluation_result(span)?,
                    )),
                    default
                        .as_ref()
                        .map(|default| evaluate_expression(default, environment).map(Box::new))
                        .transpose()?,
                )),
            }
        }
        Expression::Return { value } => {
            return Err(FunctionCarrier::Return(evaluate_expression(
                value,
                environment,
            )?));
        }
        // TODO: for now we just put unit in here so we can improve break functionality later
        Expression::Break => return Err(FunctionCarrier::Break(Value::unit())),
        Expression::Continue => return Err(FunctionCarrier::Continue),
        Expression::Index {
            value: lhs_expr,
            index: index_expr,
        } => {
            let lhs_value = evaluate_expression(lhs_expr, environment)?;

            match lhs_value {
                Value::Sequence(Sequence::String(string)) => {
                    let string = string.borrow();

                    let index = evaluate_as_index(index_expr, environment)?
                        .try_into_offset(string.chars().count(), index_expr.span)?;

                    let (start, end) = index.into_tuple();
                    let new = string
                        .chars()
                        .dropping(start)
                        .take(end - start)
                        .collect::<String>();

                    new.into()
                }
                Value::Sequence(Sequence::List(list)) => {
                    let list_length = list.borrow().len();

                    let index = evaluate_as_index(index_expr, environment)?
                        .try_into_offset(list_length, index_expr.span)?;

                    match index {
                        Offset::Element(usize_index) => {
                            let list = list.borrow();
                            let Some(value) = list.get(usize_index) else {
                                return Err(
                                    EvaluationError::out_of_bounds(index, index_expr.span).into()
                                );
                            };
                            value.clone()
                        }
                        Offset::Range(from_usize, to_usize) => {
                            let list = list.borrow();
                            let Some(values) = list.get(from_usize..to_usize) else {
                                return Err(
                                    EvaluationError::out_of_bounds(index, index_expr.span).into()
                                );
                            };

                            Value::list(values)
                        }
                    }
                }
                Value::Sequence(Sequence::Tuple(tuple)) => {
                    let index = evaluate_as_index(index_expr, environment)?
                        .try_into_offset(tuple.len(), index_expr.span)?;

                    match index {
                        Offset::Element(index_usize) => {
                            let Some(value) = tuple.get(index_usize) else {
                                return Err(
                                    EvaluationError::out_of_bounds(index, index_expr.span).into()
                                );
                            };

                            value.clone()
                        }

                        Offset::Range(from_usize, to_usize) => {
                            let Some(values) = tuple.get(from_usize..to_usize) else {
                                return Err(
                                    EvaluationError::out_of_bounds(index, index_expr.span).into()
                                );
                            };

                            Value::Sequence(Sequence::Tuple(Rc::new(values.to_vec())))
                        }
                    }
                }
                Value::Sequence(Sequence::Deque(deque)) => {
                    let list_length = deque.borrow().len();

                    let index = evaluate_as_index(index_expr, environment)?
                        .try_into_offset(list_length, index_expr.span)?;

                    match index {
                        Offset::Element(usize_index) => {
                            let list = deque.borrow();
                            let Some(value) = list.get(usize_index) else {
                                return Err(
                                    EvaluationError::out_of_bounds(index, index_expr.span).into()
                                );
                            };
                            value.clone()
                        }
                        Offset::Range(from_usize, to_usize) => {
                            let list = deque.borrow();
                            let out = list
                                .iter()
                                .dropping(from_usize)
                                .take(to_usize - from_usize)
                                .cloned()
                                .collect::<Vec<_>>();

                            Value::list(out)
                        }
                    }
                }
                Value::Sequence(Sequence::Map(dict, default)) => {
                    let key = evaluate_expression(index_expr, environment)?;
                    // let dict = dict.borrow();

                    let value = { dict.borrow().get(&key).cloned() };

                    return if let Some(value) = value {
                        Ok(value)
                    } else if let Some(default) = default {
                        let default_value = produce_default_value(
                            &default,
                            environment,
                            // NOTE: this span points at the entire expression instead of the
                            // function that cannot be executed because we don't have that span here
                            // maybe we can check the function signature earlier when we do have the span
                            lhs_expr.span.merge(index_expr.span),
                        )?;

                        // TODO: This borrow_mut can fail, handle it better!!
                        dict.borrow_mut().insert(key, default_value.clone());

                        Ok(default_value)
                    } else {
                        Err(EvaluationError::key_not_found(&key, index_expr.span).into())
                    };
                }
                value => {
                    return Err(EvaluationError::new(
                        format!("cannot index into {}", value.value_type()),
                        lhs_expr.span,
                    )
                    .into());
                }
            }
        }
        Expression::RangeInclusive {
            start: range_start,
            end: range_end,
        } => {
            let range_start = if let Some(range_start) = range_start {
                evaluate_expression(range_start, environment)?
            } else {
                return Err(EvaluationError::new(
                    "ranges without a lower bound cannot be evaluated into a value".to_string(),
                    span,
                )
                .into());
            };

            let range_start = i64::try_from(range_start).into_evaluation_result(span)?;

            if let Some(range_end) = range_end {
                let range_end = evaluate_expression(range_end, environment)?;
                let range_end = i64::try_from(range_end).into_evaluation_result(span)?;

                Value::from(range_start..=range_end)
            } else {
                Value::from(range_start..)
            }
        }
        Expression::RangeExclusive {
            start: range_start,
            end: range_end,
        } => {
            let range_start = if let Some(range_start) = range_start {
                evaluate_expression(range_start, environment)?
            } else {
                return Err(EvaluationError::new(
                    "ranges without a lower bound cannot be evaluated into a value".to_string(),
                    span,
                )
                .into());
            };

            let range_start = i64::try_from(range_start).into_evaluation_result(span)?;

            if let Some(range_end) = range_end {
                let range_end = evaluate_expression(range_end, environment)?;
                let range_end = i64::try_from(range_end).into_evaluation_result(span)?;

                Value::from(range_start..range_end)
            } else {
                Value::from(range_start..)
            }
        }
    };

    Ok(literal)
}

fn produce_default_value(
    default: &Value,
    environment: &EnvironmentRef,
    span: Span,
) -> EvaluationResult {
    match default {
        Value::Function(function) => {
            match call_function(function, &mut [], environment, span) {
                Err(FunctionCarrier::FunctionNotFound) => {
                    Err(FunctionCarrier::EvaluationError(EvaluationError::new(
                        "default function is not callable without arguments".to_string(),
                        span,
                    )))
                }
                a => a,
            }
            // test
        }
        value => Ok(value.clone()),
    }
}

fn declare_or_assign_variable(
    l_value: &Lvalue,
    value: Value,
    environment: &mut EnvironmentRef,
    span: Span,
) -> EvaluationResult {
    match l_value {
        Lvalue::Identifier {
            identifier: _,
            resolved,
        } => {
            environment
                .borrow_mut()
                .set(resolved.expect("must be resolved"), value.clone());
        }
        Lvalue::Sequence(l_values) => {
            let mut remaining = l_values.len();
            let mut iter = l_values.iter().zip(value.try_into_iter().ok_or_else(|| {
                FunctionCarrier::EvaluationError(EvaluationError::syntax_error(
                    "failed to unpack non iterable value into pattern".to_string(),
                    span,
                ))
            })?);

            for (l_value, value) in iter.by_ref() {
                remaining -= 1;
                declare_or_assign_variable(l_value, value, environment, span)?;
            }

            if remaining > 0 || iter.next().is_some() {
                return Err(EvaluationError::syntax_error(
                    "failed to unpack value into pattern because the lengths do not match"
                        .to_string(),
                    span,
                )
                .into());
            }
        }
        Lvalue::Index {
            value: lhs_expr,
            index,
        } => {
            let mut lhs = evaluate_expression(lhs_expr, environment)?;

            let index = evaluate_as_index(index, environment)?;

            set_at_index(&mut lhs, value, index, span)?;
        }
    };

    Ok(Value::unit())
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[error("{text}")]
pub struct EvaluationError {
    text: String,
    #[label("related to this")]
    span: Span,
    #[help]
    help_text: Option<String>,
}

impl EvaluationError {
    #[must_use]
    pub fn with_help(text: String, span: Span, help_text: String) -> Self {
        Self {
            text,
            span,
            help_text: Some(help_text),
        }
    }

    #[must_use]
    pub fn undefined_variable(identifier: &str, span: Span) -> Self {
        Self {
            text: format!("Undefined variable '{identifier}'"),
            span,
            help_text: None,
        }
    }

    #[must_use]
    pub fn new(message: String, span: Span) -> Self {
        Self {
            text: message,
            span,
            help_text: None,
        }
    }

    #[must_use]
    pub fn mutation_error(message: &str, span: Span) -> Self {
        Self {
            text: format!("Mutation error: {message}"),
            span,
            help_text: None,
        }
    }
    #[must_use]
    pub fn type_error(message: String, span: Span) -> Self {
        Self {
            text: message,
            span,
            help_text: None,
        }
    }
    #[must_use]
    pub fn syntax_error(message: String, span: Span) -> Self {
        Self {
            text: message,
            span,
            help_text: None,
        }
    }

    #[must_use]
    pub fn io_error(err: &std::io::Error, span: Span) -> Self {
        Self {
            text: format!("IO error: {err}"),
            span,
            help_text: None,
        }
    }

    #[must_use]
    pub fn out_of_bounds(index: Offset, span: Span) -> Self {
        match index {
            Offset::Element(index) => Self {
                text: format!("Index {index} out of bounds"),
                span,
                help_text: None,
            },
            Offset::Range(from, to) => Self {
                text: format!("Index {from}..{to} out of bounds"),
                span,
                help_text: None,
            },
        }
    }

    #[must_use]
    pub fn key_not_found(key: &Value, span: Span) -> Self {
        Self {
            text: format!("Key not found in map: {key}"),
            span,
            help_text: None,
        }
    }

    #[must_use]
    pub fn argument_error(message: &str, span: Span) -> Self {
        Self {
            text: message.to_string(),
            span,
            help_text: None,
        }
    }
}

pub trait ErrorConverter: fmt::Debug + fmt::Display {
    fn as_evaluation_error(&self, span: Span) -> EvaluationError;
}

impl<E> ErrorConverter for E
where
    E: fmt::Debug + fmt::Display,
{
    fn as_evaluation_error(&self, span: Span) -> EvaluationError {
        EvaluationError {
            text: format!("{self}"),
            span,
            help_text: None,
        }
    }
}

// NOTE: this is called `IntoEvaluationResult` but it actually only takes care of the error part of an evaluation result.
// `EvaluationResult` always wants the `Ok` type to be `Value` but this converter doesn't care.
pub trait IntoEvaluationResult<R> {
    fn into_evaluation_result(self, span: Span) -> Result<R, FunctionCarrier>;
}

impl<E, R> IntoEvaluationResult<R> for Result<R, E>
where
    E: ErrorConverter,
{
    fn into_evaluation_result(self, span: Span) -> Result<R, FunctionCarrier> {
        self.map_err(|err| FunctionCarrier::EvaluationError(err.as_evaluation_error(span)))
    }
}

fn call_function(
    function: &Rc<RefCell<OverloadedFunction>>,
    evaluated_args: &mut [Value],
    environment: &EnvironmentRef,
    span: Span,
) -> EvaluationResult {
    let result = function.borrow().call(evaluated_args, environment);

    match result {
        Err(FunctionCarrier::Return(value)) | Ok(value) => Ok(value),

        e @ Err(
            FunctionCarrier::FunctionNotFound
            | FunctionCarrier::EvaluationError(_)
            // TODO: for now we just pass the break from inside the function to outside the function. This would allow some pretty funky code and might introduce weird bugs?
            | FunctionCarrier::Break(_)
            | FunctionCarrier::Continue
        ) => e,
        Err(carrier @ FunctionCarrier::IntoEvaluationError(_)) => Err(carrier.lift_if(span)),
    }
}

fn execute_body(
    body: &ForBody,
    environment: &mut EnvironmentRef,
    result: &mut Vec<Value>,
) -> EvaluationResult {
    match body {
        ForBody::Block(expr) => {
            evaluate_expression(expr, environment)?;
        }
        ForBody::List(expr) => {
            let value = evaluate_expression(expr, environment)?;
            result.push(value);
        }
        ForBody::Map { key, value, .. } => {
            result.push(Value::tuple(vec![
                evaluate_expression(key, environment)?,
                value
                    .as_ref()
                    .map(|value| evaluate_expression(value, environment))
                    .transpose()?
                    .unwrap_or(Value::unit()),
            ]));
        }
    }
    Ok(Value::unit())
}

/// Execute a `ForBody` for a slice of `ForIteration`s.
/// # Panics
/// If the slice of `ForIterations` is empty which is something the parser should take care of for us
#[allow(clippy::too_many_lines)]
fn execute_for_iterations(
    iterations: &[ForIteration],
    body: &ForBody,
    out_values: &mut Vec<Value>,
    environment: &mut EnvironmentRef,
    span: Span,
) -> Result<Value, FunctionCarrier> {
    let Some((cur, tail)) = iterations.split_first() else {
        unreachable!("slice of for-iterations was empty")
    };

    match cur {
        ForIteration::Iteration { l_value, sequence } => {
            let mut sequence = evaluate_expression(sequence, environment)?;
            let iter = mut_value_to_iterator(&mut sequence).into_evaluation_result(span)?;

            for r_value in iter {
                // In a previous version this scope was lifted outside the loop and reset for every iteration inside the loop
                // in the following code sample this matters (a lot):
                // ```ndc
                // [fn(x) { x + i } for i in 0...10]
                // ```
                // With the current implementation with a new scope declared for every iteration this produces 10 functions
                // each with their own scope and their own version of `i`, this might potentially be a bit slower though
                let mut scope = Environment::new_scope(environment);
                declare_or_assign_variable(l_value, r_value, &mut scope, span)?;

                if tail.is_empty() {
                    match execute_body(body, &mut scope, out_values) {
                        Err(FunctionCarrier::Continue) => {}
                        Err(error) => return Err(error),
                        Ok(_value) => {}
                    }
                } else {
                    execute_for_iterations(tail, body, out_values, &mut scope, span)?;
                }
            }
        }
        ForIteration::Guard(guard) => match evaluate_expression(guard, environment)? {
            Value::Bool(true) if tail.is_empty() => {
                execute_body(body, environment, out_values)?;
            }
            Value::Bool(true) => {
                execute_for_iterations(tail, body, out_values, environment, span)?;
            }
            Value::Bool(false) => {}
            value => {
                return Err(EvaluationError::type_error(
                    format!(
                        "mismatched types: expected {}, found {}",
                        ValueType::Bool,
                        ValueType::from(&value)
                    ),
                    span,
                )
                .into());
            }
        },
    }

    Ok(Value::unit())
}
