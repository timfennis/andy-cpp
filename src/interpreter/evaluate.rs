use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, IndexMut, Mul, Neg, Not, Rem, Sub};
use std::rc::Rc;

use either::Either;
use index::{evaluate_as_index, set_at_index, Offset};
use itertools::Itertools;

use crate::ast::{
    BinaryOperator, Expression, ExpressionLocation, ForBody, ForIteration, LogicalOperator, Lvalue,
    UnaryOperator,
};
use crate::hash_map;
use crate::hash_map::HashMap;
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::function::{Function, FunctionCarrier, OverloadedFunction};
use crate::interpreter::int::Int;
use crate::interpreter::iterator::mut_value_to_iterator;
use crate::interpreter::num::{EuclideanDivisionError, Number};
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::{Value, ValueType};
use crate::lexer::Span;

use super::iterator::ValueIterator;
use super::num::into_fallible_operation;

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
        Expression::StringLiteral(s) => {
            Value::Sequence(Sequence::String(Rc::new(RefCell::new(s.to_string()))))
        }
        Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
        Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
        Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
        Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
        Expression::Unary {
            expression: expression_location,
            operator,
        } => {
            let value = evaluate_expression(expression_location, environment)?;
            match (value, operator) {
                (Value::Number(n), UnaryOperator::BitNot) => Value::Number(n.not()),
                (Value::Number(n), UnaryOperator::Neg) => Value::Number(n.neg()),
                // Just like in C the bitwise negation of `false` is `-1`
                (Value::Bool(b), UnaryOperator::BitNot) => i64::from(b).not().into(),
                (Value::Bool(b), UnaryOperator::Not) => Value::Bool(b.not()),
                (v, UnaryOperator::Not) => {
                    return Err(EvaluationError::new(
                        format!("the '!' operator cannot be applied to {}", v.value_type()),
                        span,
                    )
                    .into());
                }
                (v, UnaryOperator::Neg) => {
                    return Err(EvaluationError::new(
                        format!("{} does not support negation", v.value_type()),
                        span,
                    )
                    .into());
                }
                (v, UnaryOperator::BitNot) => {
                    return Err(EvaluationError::new(
                        format!("{} does not support bitwise negation", v.value_type()),
                        span,
                    )
                    .into());
                }
            }
        }
        Expression::Binary {
            left,
            operator: operator_token,
            right,
        } => {
            let span = left.span.merge(right.span);
            let left = evaluate_expression(left, environment)?;
            let right = evaluate_expression(right, environment)?;
            apply_operator(left, *operator_token, right).into_evaluation_result(span)?
        }
        Expression::Grouping(expr) => evaluate_expression(expr, environment)?,
        Expression::VariableDeclaration { l_value, value } => {
            let value = evaluate_expression(value, environment)?;
            declare_or_assign_variable(l_value, value, true, environment, span)?;
            Value::unit()
        }
        Expression::Assignment {
            l_value,
            r_value: value,
        } => match l_value {
            l_value @ (Lvalue::Variable { .. } | Lvalue::Sequence(_)) => {
                let value = evaluate_expression(value, environment)?;
                declare_or_assign_variable(l_value, value, false, environment, span)?
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
            value,
            operation,
        } => match l_value {
            Lvalue::Variable { identifier } => {
                let right_value = evaluate_expression(value, environment)?;

                // We need to use with_existing in this situation to ensure the refcount doesn't
                // increase which does happen when we `get` something from the environment because
                // `get` clones the value it returns.
                environment
                    .borrow()
                    .with_existing::<EvaluationResult>(identifier, |existing_value| {
                        let existing_value = &mut *existing_value.borrow_mut();

                        apply_operation_to_value(
                            environment,
                            existing_value,
                            operation,
                            right_value,
                            span,
                        )
                    })
                    .ok_or_else(|| EvaluationError::undefined_variable(identifier, span))??
            }
            Lvalue::Index {
                value: lhs_expr,
                index: index_expr,
            } => {
                let assign_to = evaluate_expression(lhs_expr, environment)?;
                let new_value = match &assign_to {
                    Value::Sequence(Sequence::List(list)) => {
                        // It's important that index and right_value are computed before the list is borrowed
                        let right_value = evaluate_expression(value, environment)?;
                        let size = list.try_borrow().into_evaluation_result(span)?.len();
                        let index = evaluate_as_index(index_expr, environment)?
                            .try_into_offset(size, index_expr.span)?;

                        let mut list = list.try_borrow_mut().into_evaluation_result(span)?;

                        match index {
                            Offset::Element(index) => {
                                let list_item = list.index_mut(index);

                                apply_operation_to_value(
                                    environment,
                                    list_item,
                                    operation,
                                    right_value,
                                    span,
                                )?
                            }
                            Offset::Range(_, _) => {
                                todo!("OpAssign into an index with range is not yet implemented")
                            }
                        }
                    }
                    Value::Sequence(Sequence::Map(dict, default)) => {
                        let right_value = evaluate_expression(value, environment)?;
                        let index = evaluate_expression(index_expr, environment)?;

                        let mut dict = dict.try_borrow_mut().into_evaluation_result(span)?;

                        let map_entry = if let Some(default) = default {
                            dict.entry(index).or_insert(Value::clone(default))
                        } else {
                            dict.get_mut(&index)
                                .ok_or_else(|| EvaluationError::key_not_found(&index, span))?
                        };

                        apply_operation_to_value(
                            environment,
                            map_entry,
                            operation,
                            right_value,
                            span,
                        )?
                    }
                    Value::Sequence(Sequence::String(_)) => {
                        return Err(EvaluationError::new(
                            "cannot OpAssign into a string".to_string(),
                            span,
                        )
                        .into());
                    }
                    _ => {
                        return Err(EvaluationError::syntax_error(
                            format!("cannot OpAssign an index into a {}", assign_to.value_type()),
                            span,
                        )
                        .into());
                    }
                };

                new_value
            }
            Lvalue::Sequence(_) => {
                return Err(EvaluationError::syntax_error(
                    "cannot use augmented assignment in combination with destructuring".to_string(),
                    span,
                )
                .into())
            }
        },
        Expression::Block { statements } => {
            let mut local_scope = Environment::new_scope(environment);

            let mut value = Value::unit();
            for stm in statements {
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
                            "mismatched types: expected bool, found {}",
                            ValueType::from(&value)
                        ),
                        span,
                    )
                    .into())
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
                (LogicalOperator::And, Value::Bool(true)) => {
                    evaluate_expression(right, environment)?
                }
                (LogicalOperator::And, Value::Bool(false)) => Value::Bool(false),
                (LogicalOperator::Or, Value::Bool(false)) => {
                    evaluate_expression(right, environment)?
                }
                (LogicalOperator::Or, Value::Bool(true)) => Value::Bool(true),
                (LogicalOperator::And | LogicalOperator::Or, value) => {
                    return Err(EvaluationError::new(
                        format!(
                            "Cannot apply logical operator to non bool value {}",
                            ValueType::from(&value)
                        ),
                        span,
                    )
                    .into())
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
                        Err(err) => return Err(err),
                        Ok(_) => {}
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
            // drop(local_scope);
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
            return if let Expression::Identifier(identifier) = &function.expression {
                call_function_by_name(identifier, &mut evaluated_args, environment, span)
            } else {
                let function_as_value = evaluate_expression(function, environment)?;

                try_call_function(
                    &[RefCell::new(function_as_value)],
                    &mut evaluated_args,
                    environment,
                    span,
                )
            };
        }
        Expression::FunctionDeclaration {
            arguments,
            body,
            name,
            pure,
        } => {
            let name = name
                .as_ref()
                .map(|it| it.try_into_identifier())
                .transpose()?;

            let mut user_function = Function::Closure {
                parameter_names: arguments.try_into_parameters()?,
                body: body.clone(),
                environment: environment.clone(),
            };

            if *pure {
                user_function = Function::Memoized {
                    cache: RefCell::default(),
                    function: Box::new(user_function),
                }
            }

            if let Some(name) = name {
                environment
                    .borrow_mut()
                    .declare_function(name, user_function);

                Value::unit()
            } else {
                user_function.into()
            }
        }

        Expression::Tuple { values } => {
            let mut out_values = Vec::with_capacity(values.len());
            for value in values {
                out_values.push(evaluate_expression(value, environment)?);
            }

            Value::Sequence(Sequence::Tuple(Rc::new(out_values)))
        }
        Expression::Identifier(identifier) => {
            // MAGIC??!
            if identifier == "None" {
                return Ok(Value::none());
            }

            if let Some(value) = environment.borrow().get(identifier) {
                value.borrow().clone()
            } else {
                return Err(EvaluationError::undefined_variable(identifier, span).into());
            }
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
                Err(err) => return Err(err),
                Ok(_) => {}
            }

            match &**body {
                ForBody::Block(_) => Value::unit(),
                ForBody::List(_) => Value::from(out_values),
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

                            values.to_vec().into()
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
                    .into())
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
    declare: bool,
    environment: &mut EnvironmentRef,
    span: Span,
) -> EvaluationResult {
    match l_value {
        Lvalue::Variable { identifier } => {
            if declare {
                // If we enable this check we can reuse environments a little bit better and gain some
                // performance but the downside is that closures behave weirdly. We could probably enable
                // it if we ensure that closures always close over the previously defined environment
                // instead of resolving at runtime. Check page 177 of the book for more info.
                // ^--- I think we did this
                if environment.borrow().contains(identifier) {
                    let new_env = Environment::new_scope(environment);
                    *environment = new_env;
                }
                environment.borrow_mut().declare(identifier, value.clone());
            } else {
                if !environment.borrow().contains(identifier) {
                    Err(EvaluationError::undefined_variable(identifier, span))?;
                }

                environment.borrow_mut().assign(identifier, value.clone());
            }
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
                declare_or_assign_variable(l_value, value, declare, environment, span)?;
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
        Lvalue::Index { .. } => {
            return Err(EvaluationError::syntax_error(
                format!(
                    "Can't declare values into {}",
                    l_value.expression_type_name()
                ),
                span,
            )
            .into())
        }
    };

    Ok(Value::unit())
}

// Applies operations like `+` or functions like `max(x, y)` to mutable pointers to values. This is
// used to optimize various OpAssign expressions that would otherwise create copies.
// ```
// x ++= [1,2,3]
// // becomes
// x.append([1,2,3]);
// // instead of
// x = x ++ [1,2,3]
// ``
fn apply_operation_to_value(
    environment: &EnvironmentRef,
    value: &mut Value,
    operation: &Either<BinaryOperator, String>,
    right_value: Value,
    span: Span,
) -> Result<Value, FunctionCarrier> {
    if let Either::Left(BinaryOperator::Concat) = operation {
        match (value, right_value) {
            (Value::Sequence(Sequence::String(left)), Value::Sequence(Sequence::String(right))) => {
                if Rc::ptr_eq(left, &right) {
                    let copy = String::from(&*right.borrow());
                    left.borrow_mut().push_str(&copy);
                } else {
                    left.borrow_mut().push_str(&right.borrow());
                }
                Ok(Value::Sequence(Sequence::String(left.clone())))
            }
            (Value::Sequence(Sequence::List(left)), Value::Sequence(Sequence::List(right))) => {
                // Special case for when a list is extended with itself
                // x := [1,2,3]; x ++= x;
                if Rc::ptr_eq(left, &right) {
                    let copy = Vec::clone(&*right.borrow());
                    left.borrow_mut().extend_from_slice(&copy);
                } else {
                    left.borrow_mut().extend_from_slice(&right.borrow());
                };
                Ok(Value::Sequence(Sequence::List(left.clone())))
            }
            (
                Value::Sequence(Sequence::Tuple(ref mut left)),
                Value::Sequence(Sequence::Tuple(mut right)),
            ) => {
                let right = Rc::make_mut(&mut right);
                Rc::make_mut(left).append(right);
                Ok(Value::Sequence(Sequence::Tuple(Rc::clone(left))))
            }
            (left, right) => Err(EvaluationError::new(
                format!(
                    "cannot apply the ++ operator to {} and {}",
                    left.value_type(),
                    right.value_type()
                ),
                span,
            )
            .into()),
        }
    } else if let Either::Left(BinaryOperator::Or) = operation {
        match (value, right_value) {
            (
                Value::Sequence(Sequence::Map(left, default)),
                Value::Sequence(Sequence::Map(right, _)),
            ) => {
                // If right and left are the same we can just do nothing and prevent a borrow checker error later
                if !Rc::ptr_eq(left, &right) {
                    match Rc::try_unwrap(right) {
                        Ok(right) => {
                            left.borrow_mut().extend(right.take());
                        }
                        Err(right) => {
                            // If we ever figure out how to make the borrow below panic we should add a test and fix it
                            left.borrow_mut()
                                .extend(right.borrow().iter().map(|(a, b)| (a.clone(), b.clone())));
                        }
                    }
                }

                return Ok(Value::Sequence(Sequence::Map(
                    left.clone(),
                    default.to_owned(),
                )));
            }
            _ => Err(EvaluationError::new(
                "cannot apply the | operator between these types".to_string(),
                span,
            )
            .into()),
        }
    } else {
        let old_value = std::mem::replace(value, Value::unit());
        match operation {
            Either::Left(binary_operator) => {
                *value = apply_operator(old_value, *binary_operator, right_value)
                    .into_evaluation_result(span)?;
            }
            Either::Right(identifier) => {
                *value = call_function_by_name(
                    identifier,
                    &mut [old_value, right_value],
                    environment,
                    span,
                )?;
            }
        }
        Ok(value.clone())
    }
}

#[derive(thiserror::Error, Debug)]
enum BinaryOpError {
    #[error("operator {operator} is not defined for {left} and {right}")]
    UndefinedOperation {
        operator: BinaryOperator,
        left: ValueType,
        right: ValueType,
    },
    #[error(transparent)]
    EuclideanDivisionFailed(#[from] EuclideanDivisionError),
    #[error("operator {operator} failed because one of its operands is invalid")]
    InvalidOperand { operator: BinaryOperator },
    #[error(
        "couldn't vectorize operation because the operands have different lengths {0} and {1}"
    )]
    InvalidLength(usize, usize),
}
#[allow(clippy::too_many_lines)]
fn apply_operator(
    left: Value,
    operator: BinaryOperator,
    right: Value,
) -> Result<Value, BinaryOpError> {
    let (left_type, right_type) = (left.value_type(), right.value_type());
    let create_type_error = || BinaryOpError::UndefinedOperation {
        operator,
        left: left_type,
        right: right_type,
    };

    let val: Value = match operator {
        BinaryOperator::Equality => left.eq(&right).into(),
        BinaryOperator::Inequality => left.ne(&right).into(),
        BinaryOperator::Greater => {
            (left.partial_cmp(&right).ok_or_else(create_type_error)? == Ordering::Greater).into()
        }
        BinaryOperator::GreaterEquals => {
            (left.partial_cmp(&right).ok_or_else(create_type_error)? != Ordering::Less).into()
        }
        BinaryOperator::Less => {
            (left.partial_cmp(&right).ok_or_else(create_type_error)? == Ordering::Less).into()
        }
        BinaryOperator::LessEquals => {
            (left.partial_cmp(&right).ok_or_else(create_type_error)? != Ordering::Greater).into()
        }
        BinaryOperator::Spaceship => {
            match left.partial_cmp(&right).ok_or_else(create_type_error)? {
                Ordering::Less => Value::from(-1),
                Ordering::Equal => Value::from(0),
                Ordering::Greater => Value::from(1),
            }
        }
        BinaryOperator::InverseSpaceship => {
            match left.partial_cmp(&right).ok_or_else(create_type_error)? {
                Ordering::Less => Value::from(1),
                Ordering::Equal => Value::from(0),
                Ordering::Greater => Value::from(-1),
            }
        }
        BinaryOperator::Plus => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple::<BinaryOpError>(
                left,
                right,
                into_fallible_operation(Number::add),
                BinaryOperator::Plus,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::Minus => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),

            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple::<BinaryOpError>(
                left,
                right,
                into_fallible_operation(Number::sub),
                BinaryOperator::Minus,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::Multiply => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),

            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple::<BinaryOpError>(
                left,
                right,
                into_fallible_operation(Number::mul),
                BinaryOperator::Multiply,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::Divide => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple::<BinaryOpError>(
                left,
                right,
                into_fallible_operation(Number::div),
                BinaryOperator::Divide,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::FloorDivide => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.floor_div(b)),

            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple::<BinaryOpError>(
                left,
                right,
                into_fallible_operation(Number::floor_div),
                BinaryOperator::FloorDivide,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::CModulo => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.rem(b)),

            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple::<BinaryOpError>(
                left,
                right,
                into_fallible_operation(Number::rem),
                BinaryOperator::CModulo,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::EuclideanModulo => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.checked_rem_euclid(b)?),

            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple(
                left,
                right,
                Number::checked_rem_euclid,
                BinaryOperator::EuclideanModulo,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::Exponent => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.pow(b)),

            (
                left @ Value::Sequence(Sequence::Tuple(_)),
                right @ Value::Sequence(Sequence::Tuple(_)),
            ) => apply_operator_to_tuple(
                left,
                right,
                into_fallible_operation::<BinaryOpError>(Number::pow),
                BinaryOperator::Exponent,
            )?,
            _ => return Err(create_type_error()),
        },
        BinaryOperator::And => match (left, right) {
            (Value::Bool(a), Value::Bool(b)) => a.bitand(b).into(),
            (Value::Number(Number::Int(a)), Value::Number(Number::Int(b))) => {
                Value::from(Number::from(a & b))
            }
            // Note that when using & on two dictionaries with a default values the default value from left is kept
            (
                Value::Sequence(Sequence::Map(left, default)),
                Value::Sequence(Sequence::Map(right, _)),
            ) => Value::Sequence(Sequence::Map(
                Rc::new(RefCell::new(hash_map::intersection(
                    &*left.borrow(),
                    &*right.borrow(),
                ))),
                default,
            )),
            _ => return Err(create_type_error()),
        },
        BinaryOperator::Or => match (left, right) {
            (Value::Bool(a), Value::Bool(b)) => a.bitor(b).into(),
            (Value::Number(Number::Int(a)), Value::Number(Number::Int(b))) => {
                Value::from(Number::from(a | b))
            }
            (
                Value::Sequence(Sequence::Map(left, default)),
                Value::Sequence(Sequence::Map(right, _)),
            ) => Value::Sequence(Sequence::Map(
                Rc::new(RefCell::new(hash_map::union(
                    &*left.borrow(),
                    &*right.borrow(),
                ))),
                default,
            )),
            _ => return Err(create_type_error()),
        },
        BinaryOperator::Xor => match (left, right) {
            (Value::Bool(a), Value::Bool(b)) => a.bitxor(b).into(),
            (Value::Number(Number::Int(a)), Value::Number(Number::Int(b))) => {
                Value::from(Number::from(a ^ b))
            }
            (
                Value::Sequence(Sequence::Map(left, default)),
                Value::Sequence(Sequence::Map(right, _)),
            ) => Value::Sequence(Sequence::Map(
                Rc::new(RefCell::new(hash_map::symmetric_difference(
                    &*left.borrow(),
                    &*right.borrow(),
                ))),
                default,
            )),
            _ => return Err(create_type_error()),
        },
        BinaryOperator::ShiftRight => match (left, right) {
            (Value::Number(Number::Int(a)), Value::Number(Number::Int(b))) => {
                Value::Number(Number::Int(
                    a.checked_shr(b)
                        .ok_or(BinaryOpError::InvalidOperand { operator })?,
                ))
            }
            _ => return Err(create_type_error()),
        },
        BinaryOperator::ShiftLeft => match (left, right) {
            (Value::Number(Number::Int(a)), Value::Number(Number::Int(b))) => {
                Value::Number(Number::Int(
                    a.checked_shl(b)
                        .ok_or(BinaryOpError::InvalidOperand { operator })?,
                ))
            }
            _ => return Err(create_type_error()),
        },
        BinaryOperator::In => match (left, right) {
            (
                Value::Sequence(Sequence::String(needle)),
                Value::Sequence(Sequence::String(haystack)),
            ) => haystack.borrow().contains(&*needle.borrow()).into(),
            (needle, Value::Sequence(Sequence::List(haystack))) => {
                haystack.borrow().contains(&needle).into()
            }
            (needle, Value::Sequence(Sequence::Tuple(haystack))) => {
                haystack.contains(&needle).into()
            }
            (needle, Value::Sequence(Sequence::Map(map, _))) => {
                map.borrow().contains_key(&needle).into()
            }
            (needle, Value::Sequence(Sequence::Iterator(iter))) => {
                let iter = ValueIterator::clone(&*iter.borrow());
                let c = match iter {
                    super::iterator::ValueIterator::ValueRange(range) => range.contains(&needle),
                    super::iterator::ValueIterator::ValueRangeFrom(range) => {
                        range.contains(&needle)
                    }
                    super::iterator::ValueIterator::ValueRangeInclusive(range) => {
                        range.contains(&needle)
                    } // For non range iterators the implementation probably has to fallback to a slow scan
                };
                Value::from(c)
            }
            _ => Value::Bool(false),
        },
        BinaryOperator::Concat => match (left, right) {
            (Value::Sequence(Sequence::String(left)), Value::Sequence(Sequence::String(right))) => {
                let mut new_string = left.borrow().clone();
                new_string.push_str(&right.borrow());
                Value::from(new_string)
            }
            (Value::Sequence(Sequence::List(left)), Value::Sequence(Sequence::List(right))) => {
                // TODO: NOTE: this first branch is an experiment, I'm not sure if it'll always work correctly
                // If we can take ownership of right which is possible if it's used as a literal expression
                // we borrow right as mutable and copy the elements over to the new list which I guess is maybe faster?
                // Case where this might work is the following: `[1] ++ [2]`
                // Case where this might not work is: `l := [1]; [1] ++ l`
                match Rc::try_unwrap(right) {
                    Ok(right) => {
                        // TODO: if there is no benefit to this branch we might as wel remove it
                        let mut new_list = left.borrow().clone();
                        new_list.append(&mut right.borrow_mut());
                        Value::from(new_list)
                    }
                    Err(right) => Value::from(
                        left.borrow()
                            .iter()
                            .chain(right.borrow().iter())
                            .cloned()
                            .collect::<Vec<_>>(),
                    ),
                }
            }
            (
                Value::Sequence(Sequence::Tuple(mut left)),
                Value::Sequence(Sequence::Tuple(mut right)),
            ) => {
                let left_mut = Rc::make_mut(&mut left);
                let right_mut = Rc::make_mut(&mut right);
                left_mut.append(right_mut);

                Value::Sequence(Sequence::Tuple(left))
            }
            _ => return Err(create_type_error()),
        },
        BinaryOperator::StringConcat => Value::from(format!("{left}{right}")),
    };

    Ok(val)
}

fn apply_operator_to_tuple<E>(
    left: Value,
    right: Value,
    op: impl Fn(Number, Number) -> Result<Number, E>,
    operator: BinaryOperator,
) -> Result<Value, BinaryOpError>
where
    BinaryOpError: From<E>,
{
    let (left_type, right_type) = (left.value_type(), right.value_type());
    let (Value::Sequence(Sequence::Tuple(left_nums)), Value::Sequence(Sequence::Tuple(right_nums))) =
        (left, right)
    else {
        return Err(BinaryOpError::UndefinedOperation {
            operator,
            left: left_type,
            right: right_type,
        });
    };

    let mut left_vec = Rc::try_unwrap(left_nums).unwrap_or_else(|rc| rc.iter().cloned().collect());

    // Zip the mutable vector with the immutable right side and perform the operations on all elements
    for (l, r) in left_vec.iter_mut().zip(right_nums.iter()) {
        if let (Value::Number(l), Value::Number(r)) = (l, r) {
            // TODO: remove these clones once we fix operator implementations
            *l = match op(l.clone(), r.clone()) {
                Ok(v) => v,
                Err(_) => {
                    return Err(BinaryOpError::UndefinedOperation {
                        operator,
                        left: left_type,
                        right: right_type,
                    })
                }
            }
        } else {
            // We could also consider removing this case, but it's probably better to expose this
            unreachable!("this case should already be covered by the type checker above")
        }
    }

    Ok(Value::tuple(left_vec))
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

fn call_function_by_name(
    name: &str,
    evaluated_args: &mut [Value],
    environment: &EnvironmentRef,
    span: Span,
) -> EvaluationResult {
    let values = environment.borrow().get_all_by_name(name);
    let result = try_call_function(&values, evaluated_args, environment, span);
    if let Err(FunctionCarrier::FunctionNotFound) = result {
        let arguments = evaluated_args.iter().map(Value::value_type).join(", ");
        return Err(FunctionCarrier::EvaluationError(EvaluationError::new(
            format!("no function called '{name}' found matches the arguments: ({arguments})"),
            span,
        )));
    }

    result
}

/// Executes a function with some extra steps:
///     * `values`: a list of values that are attempted to be executed in the order they appear in
///     * `evaluated_args`: a slice of values passed as arguments to the function
///     * `environment`: the execution environment for the function
///     * `span`: span of the expression used for error reporting
fn try_call_function(
    values: &[RefCell<Value>],
    evaluated_args: &mut [Value],
    environment: &EnvironmentRef,
    span: Span,
) -> EvaluationResult {
    // We evaluate all potential function values by first checking if they're a function and
    // then attempting to call them. If the `OverloadedFunction` returns that there are no
    // matches we defer to the next value and see if there is a match.
    //
    // This implies that a less specific function can shadow a more specific function
    //
    // fn sqrt(n: Float);
    // {
    //      fn sqrt(n: Number); // Shadows the sqrt in the parent scope completely
    // }
    for value in values {
        let value = &*value.borrow();

        // Skip all values that aren't functions
        if let Value::Function(function) = value {
            let result = call_function(function, evaluated_args, environment, span);

            if let Err(FunctionCarrier::FunctionNotFound) = result {
                continue;
            }

            return result;
        }
    }

    Err(FunctionCarrier::FunctionNotFound)
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
            FunctionCarrier::EvaluationError(_)
            | FunctionCarrier::FunctionNotFound
            // TODO: for now we just pass the break from inside the function to outside the function. This would allow some pretty funky code and might introduce weird bugs?
            | FunctionCarrier::Break(_),
        ) => e,
        Err(carrier @ FunctionCarrier::IntoEvaluationError(_)) => Err(carrier.lift(span)),
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
                // In a previous version this scope was lifted outside of the loop and reset for every iteration inside the loop
                // in the following code sample this matters (a lot):
                // ```ndc
                // [fn(x) { x + i } for i in 0...10]
                // ```
                // With the current implementation with a new scope declared for every iteration this produces 10 functions
                // each with their own scope and their own version of `i`, this might potentially be a bit slower though
                let mut scope = Environment::new_scope(environment);
                declare_or_assign_variable(l_value, r_value, true, &mut scope, span)?;

                if tail.is_empty() {
                    execute_body(body, &mut scope, out_values)?;
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
                        "mismatched types: expected bool, found {}",
                        ValueType::from(&value)
                    ),
                    span,
                )
                .into())
            }
        },
    }

    Ok(Value::unit())
}
