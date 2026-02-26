use crate::ast::{
    Binding, Expression, ExpressionLocation, ForBody, ForIteration, LogicalOperator, Lvalue,
};
use crate::hash_map::HashMap;
use crate::interpreter::environment::Environment;
use crate::interpreter::function::{Function, FunctionBody, FunctionCarrier, StaticType};
use crate::interpreter::int::Int;
use crate::interpreter::iterator::mut_value_to_iterator;
use crate::interpreter::num::Number;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use crate::lexer::Span;
use index::{Offset, evaluate_as_index, get_at_index, set_at_index};
use itertools::Itertools;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub type EvaluationResult = Result<Value, FunctionCarrier>;

mod index;

#[allow(clippy::too_many_lines)]
pub(crate) fn evaluate_expression(
    expression_location: &ExpressionLocation,
    environment: &Rc<RefCell<Environment>>,
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

            match resolved {
                Binding::None => panic!("binding not resolved at runtime"),
                Binding::Resolved(resolved) => environment.borrow().get(*resolved),
                Binding::Dynamic(_) => panic!("attempted to evaluate dynamic binding"),
            }
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
            match l_value {
                Lvalue::Identifier {
                    identifier,
                    resolved: resolved_l_value,
                    ..
                } => {
                    let resolved_l_value = resolved_l_value.expect("lvalue must be resolved");
                    let rhs = evaluate_expression(r_value, environment)?;

                    // TODO: this statement does damage which isn't reverted when for instance we can't find the function
                    let Some(lhs) = environment.borrow_mut().take(resolved_l_value) else {
                        return Err(EvaluationError::undefined_variable(identifier, span).into());
                    };

                    let types = [lhs.static_type(), rhs.static_type()];
                    let mut arguments = [lhs, rhs];

                    let mut operations_to_try = [
                        (
                            resolve_dynamic_binding(resolved_assign_operation, &types, environment),
                            true,
                        ),
                        (
                            resolve_dynamic_binding(resolved_operation, &types, environment),
                            false,
                        ),
                    ]
                    .into_iter()
                    .filter_map(|(value, in_place)| value.map(|value| (value, in_place)))
                    .peekable();

                    while let Some((operation, modified_in_place)) = operations_to_try.next() {
                        let Value::Function(func) = operation else {
                            unreachable!(
                                "the resolver pass should have guaranteed that the operation points to a function"
                            );
                        };
                        // (&func, &mut arguments, environment, span)
                        let result = match func.call_checked(&mut arguments, environment) {
                            Err(FunctionCarrier::FunctionTypeMismatch)
                                if operations_to_try.peek().is_none() =>
                            {
                                let argument_string =
                                    arguments.iter().map(Value::static_type).join(", ");

                                return Err(FunctionCarrier::EvaluationError(
                                    EvaluationError::new(
                                        format!(
                                            "no function called 'TODO FIGURE OUT NAME' found matches the arguments: ({argument_string})"
                                        ),
                                        span,
                                    ),
                                ));
                            }
                            Err(FunctionCarrier::FunctionTypeMismatch) => continue,
                            Err(carrier @ FunctionCarrier::IntoEvaluationError(_)) => {
                                return Err(carrier.lift_if(span));
                            }
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

                    let types = [value_at_index.static_type(), right_value.static_type()];
                    let mut operations_to_try = [
                        (
                            resolve_dynamic_binding(resolved_assign_operation, &types, environment),
                            true,
                        ),
                        (
                            resolve_dynamic_binding(resolved_operation, &types, environment),
                            false,
                        ),
                    ]
                    .into_iter()
                    .filter_map(|(value, in_place)| value.map(|value| (value, in_place)))
                    .peekable();

                    while let Some((operation_val, modified_in_place)) = operations_to_try.next() {
                        let Value::Function(func) = operation_val else {
                            unreachable!(
                                "the resolver pass should have guaranteed that the operation points to a function"
                            );
                        };

                        let result = match func.call_checked(
                            &mut [value_at_index.clone(), right_value.clone()],
                            environment,
                        ) {
                            Err(FunctionCarrier::FunctionTypeMismatch)
                                if operations_to_try.peek().is_none() =>
                            {
                                return Err(FunctionCarrier::EvaluationError(
                                    EvaluationError::new(
                                        format!(
                                            "no function called 'TODO FIGURE OUT NAME' found matches the arguments: ({}, {})",
                                            value_at_index.static_type(),
                                            right_value.static_type()
                                        ),
                                        span,
                                    ),
                                ));
                            }
                            Err(FunctionCarrier::FunctionTypeMismatch) => continue,
                            Err(carrier @ FunctionCarrier::IntoEvaluationError(_)) => {
                                return Err(carrier.lift_if(span));
                            }
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
            let local_scope = Rc::new(RefCell::new(Environment::new_scope(environment)));

            let mut value = Value::unit();
            for stm in statements {
                value = evaluate_expression(stm, &local_scope)?;
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
                            StaticType::Bool,
                            value.static_type()
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
                            value.static_type()
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
                let arg = evaluate_expression(argument, environment)?;
                evaluated_args.push(arg);
            }

            resolve_and_call(function, evaluated_args, environment, span)?
        }
        Expression::FunctionDeclaration {
            parameters: arguments,
            body,
            resolved_name,
            return_type,
            pure,
            ..
        } => {
            let mut user_function = FunctionBody::Closure {
                parameter_names: arguments.try_into_parameters()?,
                body: *body.clone(),
                return_type: return_type.clone().unwrap_or_else(StaticType::unit),
                environment: environment.clone(),
            };

            if *pure {
                user_function = FunctionBody::Memoized {
                    cache: RefCell::default(),
                    function: Box::new(user_function),
                }
            }

            if let Some(resolved_name) = *resolved_name {
                environment.borrow_mut().set(
                    resolved_name,
                    // TODO: put name in declaration?
                    Value::function(Function::from_body(user_function)),
                );

                Value::unit()
            } else {
                Value::function(Function::from_body(user_function))
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
                        // NOTE: WHEN DOES IT FAIL!?
                        dict.borrow_mut().insert(key, default_value.clone());

                        Ok(default_value)
                    } else {
                        Err(EvaluationError::key_not_found(&key, index_expr.span).into())
                    };
                }
                value => {
                    return Err(EvaluationError::new(
                        format!("cannot index into {}", value.static_type()),
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
    environment: &Rc<RefCell<Environment>>,
    span: Span,
) -> EvaluationResult {
    match default {
        Value::Function(function) => {
            match function.call_checked(&mut [], environment) {
                Err(FunctionCarrier::FunctionTypeMismatch) => {
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
    environment: &Rc<RefCell<Environment>>,
    span: Span,
) -> EvaluationResult {
    match l_value {
        Lvalue::Identifier { resolved, .. } => {
            environment
                .borrow_mut()
                .set(resolved.expect("must be resolved"), value.clone());
        }
        Lvalue::Sequence(l_values) => {
            let r_values = value.try_into_vec().ok_or_else(|| {
                FunctionCarrier::EvaluationError(EvaluationError::syntax_error(
                    "failed to unpack non iterable value into pattern".to_string(),
                    span,
                ))
            })?;

            if l_values.len() != r_values.len() {
                return Err(EvaluationError::syntax_error(
                    "failed to unpack value into pattern because the lengths do not match"
                        .to_string(),
                    span,
                )
                .into());
            }
            let mut iter = l_values.iter().zip(r_values);

            for (l_value, value) in iter.by_ref() {
                declare_or_assign_variable(l_value, value, environment, span)?;
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

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct EvaluationError {
    text: String,
    span: Span,
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

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn help_text(&self) -> Option<&str> {
        self.help_text.as_deref()
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

pub trait LiftEvaluationResult<R> {
    fn add_span(self, span: Span) -> Result<R, FunctionCarrier>;
}

impl<R> LiftEvaluationResult<R> for Result<R, FunctionCarrier> {
    fn add_span(self, span: Span) -> Self {
        self.map_err(|err| err.lift_if(span))
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

fn execute_for_body(
    body: &ForBody,
    environment: &Rc<RefCell<Environment>>,
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
    environment: &Rc<RefCell<Environment>>,
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
                let scope = Rc::new(RefCell::new(Environment::new_scope(environment)));
                declare_or_assign_variable(l_value, r_value, &scope, span)?;

                if tail.is_empty() {
                    match execute_for_body(body, &scope, out_values) {
                        Err(FunctionCarrier::Continue) => {}
                        Err(error) => return Err(error),
                        Ok(_value) => {}
                    }
                } else {
                    execute_for_iterations(tail, body, out_values, &scope, span)?;
                }
            }
        }
        ForIteration::Guard(guard) => match evaluate_expression(guard, environment)? {
            Value::Bool(true) if tail.is_empty() => {
                execute_for_body(body, environment, out_values)?;
            }
            Value::Bool(true) => {
                execute_for_iterations(tail, body, out_values, environment, span)?;
            }
            Value::Bool(false) => {}
            value => {
                return Err(EvaluationError::type_error(
                    format!(
                        "mismatched types: expected {}, found {}",
                        StaticType::Bool,
                        value.static_type(),
                    ),
                    span,
                )
                .into());
            }
        },
    }

    Ok(Value::unit())
}

// fn evaluate_as_function(
//     function_expression: &ExpressionLocation,
//     arg_types: &[StaticType],
//     environment: &Rc<RefCell<Environment>>,
// ) -> EvaluationResult {
//     let ExpressionLocation { expression, .. } = function_expression;
//
//     if let Expression::Identifier { resolved, .. } = expression {
//         resolve_dynamic_binding(resolved, arg_types, environment).ok_or_else(|| {
//             FunctionCarrier::EvaluationError(EvaluationError::new(
//                 format!(
//                     "Failed to find a function that can handle the arguments ({}) at runtime",
//                     arg_types.iter().join(", ")
//                 ),
//                 function_expression.span,
//             ))
//         })
//     } else {
//         evaluate_expression(function_expression, environment)
//     }
// }

fn resolve_and_call(
    function_expression: &ExpressionLocation,
    mut args: Vec<Value>,
    environment: &Rc<RefCell<Environment>>,
    span: Span,
) -> EvaluationResult {
    let ExpressionLocation { expression, .. } = function_expression;

    let function_as_value = if let Expression::Identifier { name, resolved, .. } = expression {
        let arg_types = args.iter().map(|arg| arg.static_type()).collect::<Vec<_>>();

        let opt = match resolved {
            Binding::None => None,
            Binding::Resolved(var) => Some(environment.borrow().get(*var)),
            Binding::Dynamic(dynamic_binding) => dynamic_binding.iter().find_map(|binding| {
                let value = environment.borrow().get(*binding);

                let Value::Function(fun) = &value else {
                    panic!("dynamic binding resolved to non-function type at runtime");
                };

                if fun.static_type().is_fn_and_matches(&arg_types) {
                    return Some(value);
                }

                None
            }),
        };

        if opt.is_none() {
            if let Binding::Dynamic(dynamic_binding) = resolved {
                if let [left_type, right_type] = arg_types.as_slice() {
                    if left_type.supports_vectorization_with(right_type) {
                        let elem_types = vectorized_element_types(left_type, right_type);
                        let inner_fn = dynamic_binding.iter().find_map(|binding| {
                            let value = environment.borrow().get(*binding);
                            let Value::Function(fun) = &value else {
                                panic!("dynamic binding resolved to non-function type at runtime");
                            };
                            if fun.static_type().is_fn_and_matches(&elem_types) {
                                Some(Rc::clone(&fun))
                            } else {
                                None
                            }
                        });
                        if let Some(inner_fn) = inner_fn {
                            return inner_fn
                                .call_vectorized(&mut args, environment)
                                .add_span(span);
                        }
                    }
                }
            }
        }

        opt.ok_or_else(|| {
            FunctionCarrier::EvaluationError(EvaluationError::new(
                format!(
                    "no function called '{name}' found matches the arguments: ({})",
                    arg_types.iter().join(", ")
                ),
                function_expression.span,
            ))
        })?
    } else {
        evaluate_expression(function_expression, environment)?
    };

    if let Value::Function(function) = function_as_value {
        function.call(&mut args, environment).add_span(span)
    } else {
        Err(FunctionCarrier::EvaluationError(EvaluationError::new(
            format!(
                "Unable to invoke {} as a function.",
                function_as_value.static_type()
            ),
            span,
        )))
    }
}

fn vectorized_element_types(left: &StaticType, right: &StaticType) -> [StaticType; 2] {
    let left_elem = left.sequence_element_type().unwrap_or_else(|| left.clone());
    let right_elem = right
        .sequence_element_type()
        .unwrap_or_else(|| right.clone());
    [left_elem, right_elem]
}

fn resolve_dynamic_binding(
    binding: &Binding,
    arg_types: &[StaticType],
    environment: &Rc<RefCell<Environment>>,
) -> Option<Value> {
    match binding {
        Binding::None => None,
        Binding::Resolved(var) => Some(environment.borrow().get(*var)),
        Binding::Dynamic(dynamic_binding) => dynamic_binding
            .iter() // TODO: should we consider the binding order?
            .find_map(|binding| {
                let value = environment.borrow().get(*binding);

                let Value::Function(fun) = &value else {
                    panic!("dynamic binding resolved to non-function type at runtime");
                };

                // Find the first function that matches
                if fun.static_type().is_fn_and_matches(arg_types) {
                    return Some(value);
                }

                None
            }),
    }
}
