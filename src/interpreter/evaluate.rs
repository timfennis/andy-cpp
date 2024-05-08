use std::cell::RefCell;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::ops::{IndexMut, Neg, Not, Range, RangeInclusive, Rem};
use std::rc::Rc;

use either::Either;

use crate::ast::{
    BinaryOperator, Expression, ExpressionLocation, LogicalOperator, Lvalue, UnaryOperator,
};
use crate::hash_map;
use crate::hash_map::HashMap;
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::function::{Function, FunctionCarrier, OverloadedFunction};
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value, ValueType};
use crate::lexer::Location;

pub type EvaluationResult = Result<Value, FunctionCarrier>;

#[allow(clippy::too_many_lines)]
pub(crate) fn evaluate_expression(
    expression_location: &ExpressionLocation,
    environment: &mut EnvironmentRef,
) -> EvaluationResult {
    let (start, end) = (expression_location.start, expression_location.end);
    let literal: Value = match &expression_location.expression {
        Expression::BoolLiteral(b) => Value::Bool(*b),
        Expression::StringLiteral(s) => {
            Value::Sequence(Sequence::String(Rc::new(RefCell::new(s.to_string()))))
        }
        Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
        Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
        Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
        Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
        Expression::UnitLiteral => Value::Unit,
        Expression::Unary {
            expression: expression_location,
            operator,
        } => {
            let value = evaluate_expression(expression_location, environment)?;
            match (value, operator) {
                (Value::Number(n), UnaryOperator::Neg) => Value::Number(n.neg()),
                (Value::Bool(b), UnaryOperator::Bang) => Value::Bool(b.not()),
                (_, UnaryOperator::Bang) => {
                    return Err(EvaluationError::type_error(
                        "the '!' operator cannot be applied to this type",
                        start,
                        end,
                    )
                    .into());
                }
                (_, UnaryOperator::Neg) => {
                    return Err(EvaluationError::type_error(
                        "this type cannot be negated",
                        start,
                        end,
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
            let left = evaluate_expression(left, environment)?;
            let right = evaluate_expression(right, environment)?;
            apply_operator(left, *operator_token, right)?
        }
        Expression::Grouping(expr) => evaluate_expression(expr, environment)?,
        Expression::VariableDeclaration { l_value, value } => {
            let Lvalue::Variable { identifier } = l_value else {
                return Err(EvaluationError::syntax_error(
                    &format!(
                        "Can't declare values into {}",
                        l_value.expression_type_name()
                    ),
                    start,
                    end,
                )
                .into());
            };

            let value = evaluate_expression(value, environment)?;

            // If we enable this check we can reuse environments a little bit better and gain some
            // performance but the downside is that closures behave weirdly. We could probably enable
            // it if we ensure that closures always close over the previously defined environment
            // instead of resolving at runtime. Check page 177 of the book for more info.
            if environment.borrow().contains(identifier) {
                let new_env = Environment::new_scope(environment);
                *environment = new_env;
            }

            environment.borrow_mut().declare(identifier, value.clone());

            value
        }
        Expression::Assignment { l_value, value } => match l_value {
            Lvalue::Variable { identifier } => {
                if !environment.borrow().contains(identifier) {
                    Err(EvaluationError::undefined_variable(identifier, start, end))?;
                }

                let value = evaluate_expression(value, environment)?;
                environment.borrow_mut().assign(identifier, value.clone());
                value
            }
            Lvalue::Index {
                value: assign_to,
                index,
            } => {
                let assign_to = evaluate_expression(assign_to, environment)?;
                match &assign_to {
                    Value::Sequence(Sequence::List(list)) => {
                        // the computation of this value may need the list that we assign to,
                        // therefore the value needs to be computed before we mutably borrow the list
                        // see: `bug0001_in_place_map.ndct`
                        let value = evaluate_expression(value, environment)?;

                        let index = value_to_forward_index(
                            evaluate_expression(index, environment)?,
                            list.borrow().len(),
                            start,
                            end,
                        )?;

                        let mut list = list
                            .try_borrow_mut()
                            .map_err(|_| EvaluationError::mutation_error("you cannot mutate a value in a list while you're iterating over this list", start, end))?;

                        let x = list.index_mut(index);
                        *x = value;
                    }
                    Value::Sequence(Sequence::String(insertion_target)) => {
                        let value = evaluate_expression(value, environment)?;

                        if let Value::Sequence(Sequence::String(target_string)) = value {
                            let target_string = target_string.borrow();
                            let index = value_to_forward_index(
                                evaluate_expression(index, environment)?,
                                insertion_target.borrow().chars().count(),
                                start,
                                end,
                            )?;

                            let mut insertion_target = insertion_target.borrow_mut();
                            insertion_target.replace_range(index..=index, target_string.as_str());
                        } else {
                            return Err(EvaluationError::syntax_error(
                                &format!("cannot insert {} at index", value.value_type()),
                                start,
                                end,
                            )
                            .into());
                        }
                    }
                    Value::Sequence(Sequence::Map(map, _)) => {
                        let value = evaluate_expression(value, environment)?;
                        let key = evaluate_expression(index, environment)?;

                        let mut map = map.try_borrow_mut().into_evaluation_result(start, end)?;

                        map.insert(key, value);
                    }
                    _ => {
                        return Err(EvaluationError::syntax_error(
                            &format!("cannot insert into {} at index", assign_to.value_type()),
                            start,
                            end,
                        )
                        .into());
                    }
                }

                assign_to
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
                            start,
                            end,
                        )
                    })
                    .ok_or_else(|| EvaluationError::undefined_variable(identifier, start, end))??
            }
            Lvalue::Index {
                value: assign_to,
                index,
            } => {
                let assign_to = evaluate_expression(assign_to, environment)?;
                let new_value = match &assign_to {
                    Value::Sequence(Sequence::List(list)) => {
                        // It's important that index and right_value are computed before the list is borrowed
                        let right_value = evaluate_expression(value, environment)?;
                        let index = value_to_forward_index(
                            evaluate_expression(index, environment)?,
                            list.try_borrow().into_evaluation_result(start, end)?.len(),
                            start,
                            end,
                        )?;

                        let mut list = list.try_borrow_mut().into_evaluation_result(start, end)?;

                        let list_item = list.index_mut(index);

                        apply_operation_to_value(
                            environment,
                            list_item,
                            operation,
                            right_value,
                            start,
                            end,
                        )?
                    }
                    Value::Sequence(Sequence::Map(dict, default)) => {
                        let right_value = evaluate_expression(value, environment)?;
                        let index = evaluate_expression(index, environment)?;

                        let mut dict = dict.try_borrow_mut().into_evaluation_result(start, end)?;

                        let map_entry = if let Some(default) = default {
                            dict.entry(index).or_insert(Value::clone(default))
                        } else {
                            dict.get_mut(&index)
                                .ok_or_else(|| EvaluationError::key_not_found(&index, start, end))?
                        };

                        apply_operation_to_value(
                            environment,
                            map_entry,
                            operation,
                            right_value,
                            start,
                            end,
                        )?
                    }
                    Value::Sequence(Sequence::String(_)) => {
                        return Err(EvaluationError::type_error(
                            "cannot OpAssign into a string",
                            start,
                            end,
                        )
                        .into());
                    }
                    _ => {
                        return Err(EvaluationError::syntax_error(
                            &format!("cannot OpAssign an index into a {}", assign_to.value_type()),
                            start,
                            end,
                        )
                        .into());
                    }
                };

                new_value
            }
        },
        Expression::Block { statements } => {
            let mut local_scope = Environment::new_scope(environment);

            let mut value = Value::Unit;
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
                (Value::Bool(false), None) => Value::Unit,
                (value, _) => {
                    return Err(EvaluationError::type_error(
                        &format!(
                            "mismatched types: expected bool, found {}",
                            ValueType::from(&value)
                        ),
                        start,
                        end,
                    )
                    .into())
                }
            }
        }
        Expression::Statement(expression) => {
            evaluate_expression(expression, environment)?;
            Value::Unit
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
                    return Err(EvaluationError::type_error(
                        &format!(
                            "Cannot apply logical operator to non bool value {}",
                            ValueType::from(&value)
                        ),
                        start,
                        end,
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
                    evaluate_expression(loop_body, environment)?;
                } else if lit == Value::Bool(false) {
                    break;
                } else {
                    return Err(EvaluationError::type_error(
                        "Expression in a while structure must return a bool",
                        start,
                        end,
                    )
                    .into());
                }
            }
            // drop(local_scope);
            Value::Unit
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
                call_function_by_name(identifier, &evaluated_args, environment, start, end)
            } else {
                let function_as_value = evaluate_expression(function, environment)?;

                try_call_function(
                    &[RefCell::new(function_as_value)],
                    &evaluated_args,
                    environment,
                    start,
                    end,
                )
            };
        }
        Expression::FunctionDeclaration {
            arguments,
            body,
            name,
        } => {
            let name = name.try_into_identifier()?;

            let user_function = Function::Closure {
                parameter_names: arguments.try_into_parameters()?,
                body: body.clone(),
                environment: environment.clone(),
            };

            environment
                .borrow_mut()
                .declare_function(&name, user_function);

            Value::Unit
        }

        Expression::Tuple { values } => {
            let mut out_values = Vec::with_capacity(values.len());
            for value in values {
                out_values.push(evaluate_expression(value, environment)?);
            }

            Value::Sequence(Sequence::Tuple(Rc::new(out_values)))
        }
        Expression::Identifier(identifier) => {
            if let Some(value) = environment.borrow().get(identifier) {
                // TODO: is cloning the value a good idea here??
                value.borrow().clone()
            } else {
                return Err(EvaluationError::undefined_variable(identifier, start, end).into());
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
                    Value::Unit
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
        Expression::For {
            l_value,
            sequence,
            loop_body,
        } => {
            let sequence = evaluate_expression(sequence, environment)?;
            let Value::Sequence(sequence) = sequence else {
                return Err(EvaluationError::syntax_error(
                    &format!("cannot iterate over {}", sequence.value_type()),
                    start,
                    end,
                )
                .into());
            };

            let Lvalue::Variable {
                identifier: var_name,
            } = l_value
            else {
                return Err(EvaluationError::syntax_error(
                    "cannot use this expression in for loop",
                    start,
                    end,
                )
                .into());
            };

            match sequence {
                Sequence::String(str) => {
                    let str = str.borrow();
                    for char in str.chars() {
                        let mut scope = Environment::new_scope(environment);

                        // TODO: allocating a new string here is probably not optimal
                        scope.borrow_mut().declare(
                            var_name,
                            Value::Sequence(Sequence::String(Rc::new(RefCell::new(String::from(
                                char,
                            ))))),
                        );

                        evaluate_expression(loop_body, &mut scope)?;
                    }
                    drop(str);
                }
                Sequence::List(xs) => {
                    let xs = xs.borrow();
                    for x in xs.iter() {
                        let mut scope = Environment::new_scope(environment);

                        scope.borrow_mut().declare(var_name, x.clone());

                        evaluate_expression(loop_body, &mut scope)?;
                    }
                }
                Sequence::Tuple(values) => {
                    for x in &*values {
                        let mut scope = Environment::new_scope(environment);

                        scope.borrow_mut().declare(var_name, x.clone());

                        evaluate_expression(loop_body, &mut scope)?;
                    }
                }
                Sequence::Map(map, _) => {
                    let xs = map.borrow();
                    for (key, value) in xs.iter() {
                        // TODO: support pattern matching tuples in var name
                        let mut scope = Environment::new_scope(environment);
                        scope.borrow_mut().declare(
                            var_name,
                            Value::Sequence(Sequence::Tuple(Rc::new(vec![
                                key.clone(),
                                value.clone(),
                            ]))),
                        );
                        evaluate_expression(loop_body, &mut scope)?;
                    }
                }
            }

            Value::Unit
        }
        Expression::Return { value } => {
            return Err(FunctionCarrier::Return(evaluate_expression(
                value,
                environment,
            )?));
        }
        Expression::Index {
            value: value_expr,
            index: index_expr,
        } => {
            let value = evaluate_expression(value_expr, environment)?;

            match value {
                Value::Sequence(sequence) => {
                    match sequence {
                        Sequence::String(string) => {
                            let string = string.borrow();

                            let index = value_to_forward_index(
                                evaluate_expression(index_expr, environment)?,
                                string.chars().count(),
                                index_expr.start,
                                index_expr.end,
                            )?;

                            let Some(char) = string.chars().nth(index) else {
                                return Err(EvaluationError::out_of_bounds(
                                    index,
                                    index_expr.start,
                                    index_expr.end,
                                )
                                .into());
                            };
                            Value::Sequence(Sequence::String(Rc::new(RefCell::new(String::from(
                                char,
                            )))))
                        }
                        Sequence::List(list) => {
                            let index = value_to_forward_index(
                                evaluate_expression(index_expr, environment)?,
                                list.borrow().len(),
                                index_expr.start,
                                index_expr.end,
                            )?;

                            let list = list.borrow();
                            let Some(value) = list.get(index) else {
                                return Err(EvaluationError::out_of_bounds(
                                    index,
                                    index_expr.start,
                                    index_expr.end,
                                )
                                .into());
                            };
                            value.clone()
                        }
                        // TODO: this implementation is 99% the same as the one above
                        Sequence::Tuple(tuple) => {
                            let index = value_to_forward_index(
                                evaluate_expression(index_expr, environment)?,
                                tuple.len(),
                                index_expr.start,
                                index_expr.end,
                            )?;

                            let Some(value) = tuple.get(index) else {
                                return Err(EvaluationError::out_of_bounds(
                                    index,
                                    index_expr.start,
                                    index_expr.end,
                                )
                                .into());
                            };

                            value.clone()
                        }
                        Sequence::Map(dict, default) => {
                            let key = evaluate_expression(index_expr, environment)?;
                            let dict = dict.borrow();

                            return if let Some(value) = dict.get(&key) {
                                Ok(value.clone())
                            } else if let Some(default) = default {
                                Ok(Value::clone(&*default))
                            } else {
                                Err(EvaluationError::key_not_found(
                                    &key,
                                    index_expr.start,
                                    index_expr.end,
                                )
                                .into())
                            };
                        }
                    }
                }
                // TODO: improve error handling
                value => {
                    return Err(EvaluationError::type_error(
                        &format!("cannot index into {}", value.value_type()),
                        value_expr.start,
                        value_expr.end,
                    )
                    .into())
                }
            }
        }
        Expression::RangeInclusive {
            start: range_start,
            end: range_end,
        } => {
            let range_start = evaluate_expression(
                range_start
                    .as_deref()
                    .expect("Unbound ranges are not yet implemented"),
                environment,
            )?;
            let range_end = evaluate_expression(
                range_end
                    .as_deref()
                    .expect("Unbound ranges are not yet implemented"),
                environment,
            )?;

            let range_start = i64::try_from(range_start).into_evaluation_result(start, end)?;
            let range_end = i64::try_from(range_end).into_evaluation_result(start, end)?;
            let range = RangeInclusive::new(range_start, range_end)
                .map(Value::from)
                .collect::<Vec<Value>>();

            Value::from(range)
        }
        Expression::RangeExclusive {
            start: range_start,
            end: range_end,
        } => {
            let range_start = evaluate_expression(
                range_start
                    .as_deref()
                    .expect("Unbound ranges are not yet implemented"),
                environment,
            )?;
            let range_end = evaluate_expression(
                range_end
                    .as_deref()
                    .expect("Unbound ranges are not yet implemented"),
                environment,
            )?;

            let range_start = i64::try_from(range_start).into_evaluation_result(start, end)?;
            let range_end = i64::try_from(range_end).into_evaluation_result(start, end)?;
            let range = Range {
                start: range_start,
                end: range_end,
            }
            .map(Value::from)
            .collect::<Vec<Value>>();

            Value::from(range)
        }
    };

    Ok(literal)
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
    start: Location,
    end: Location,
) -> Result<Value, FunctionCarrier> {
    return if let Either::Left(BinaryOperator::Concat) = operation {
        match (value, right_value) {
            (Value::Sequence(Sequence::String(left)), Value::Sequence(Sequence::String(right))) => {
                left.borrow_mut().push_str(&right.borrow());
                Ok(Value::Sequence(Sequence::String(left.clone())))
            }
            (Value::Sequence(Sequence::List(left)), Value::Sequence(Sequence::List(right))) => {
                // TODO if left == right this will panic
                left.borrow_mut().extend_from_slice(&right.borrow());
                Ok(Value::Sequence(Sequence::List(left.clone())))
            }
            (
                Value::Sequence(Sequence::Tuple(ref mut left)),
                Value::Sequence(Sequence::Tuple(right)),
            ) => {
                //
                Rc::make_mut(left).extend_from_slice(&right);
                Ok(Value::Sequence(Sequence::Tuple(left.clone())))
            }
            _ => Err(EvaluationError::type_error(
                "cannot apply the ++ operator between these types",
                start,
                end,
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
                            left.borrow_mut().extend(
                                right
                                    .borrow() // TODO: change to try borrow in case something i
                                    .iter()
                                    .map(|(a, b)| (a.clone(), b.clone())),
                            );
                        }
                    }
                }

                return Ok(Value::Sequence(Sequence::Map(
                    left.clone(),
                    default.to_owned(),
                )));
            }
            _ => Err(EvaluationError::type_error(
                "cannot apply the | operator between these types",
                start,
                end,
            )
            .into()),
        }
    } else {
        let old_value = std::mem::replace(value, Value::Unit);
        match operation {
            Either::Left(binary_operator) => {
                *value = apply_operator(old_value, *binary_operator, right_value)?;
            }
            Either::Right(identifier) => {
                *value = call_function_by_name(
                    identifier,
                    &[old_value, right_value],
                    environment,
                    start,
                    end,
                )?;
            }
        }
        Ok(value.clone())
    };
}

#[allow(clippy::too_many_lines)]
fn apply_operator(
    left: Value,
    operator: BinaryOperator,
    right: Value,
) -> Result<Value, EvaluationError> {
    fn create_type_error(
        operator: BinaryOperator,
        left: ValueType,
        right: ValueType,
    ) -> EvaluationError {
        EvaluationError::type_error(
            &format!("cannot apply operator {operator:?} to {left} and {right}"),
            // TODO: fix error handling, possibly by changing the function signature to return a
            //       special kind of error that does not require line numbers
            Location { line: 0, column: 0 },
            Location { line: 0, column: 0 },
        )
    }
    let (left_type, right_type) = (left.value_type(), right.value_type());
    let create_type_error = { || create_type_error(operator, left_type, right_type) };

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
        BinaryOperator::Plus => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            _ => todo!("implement + for these types"),
        },
        BinaryOperator::Minus => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => todo!("implement - for these types"),
        },
        BinaryOperator::Multiply => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => todo!("implement * for these types"),
        },
        BinaryOperator::Divide => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            _ => todo!("implement / for these types"),
        },
        BinaryOperator::CModulo => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.rem(b)),
            _ => todo!("implement % for these types"),
        },
        BinaryOperator::EuclideanModulo => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.checked_rem_euclid(b)?),
            _ => todo!("implement %% for these types"),
        },
        BinaryOperator::Exponent => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a.checked_pow(b)?),
            _ => todo!("implement ^ for these types"),
        },
        BinaryOperator::And => match (left, right) {
            (
                Value::Number(Number::Int(Int::Int64(a))),
                Value::Number(Number::Int(Int::Int64(b))),
            ) => Value::from(a & b),
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
            _ => todo!("implement & for these types"),
        },
        BinaryOperator::Or => match (left, right) {
            (
                Value::Number(Number::Int(Int::Int64(a))),
                Value::Number(Number::Int(Int::Int64(b))),
            ) => Value::from(a | b),
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
            _ => todo!("implement & for these types"),
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
            _ => todo!("handle errors"),
        },
    };

    Ok(val)
}

// TODO: nice generic error for now?
#[derive(thiserror::Error, Debug)]
#[error("{message}")]
pub struct ErrorMessage {
    pub message: String,
}

pub struct EvaluationError {
    text: String,
    start: Location,
    #[allow(unused)]
    end: Location,
}

impl EvaluationError {
    #[must_use]
    pub fn undefined_variable(identifier: &str, start: Location, end: Location) -> Self {
        Self {
            text: format!("Undefined variable '{identifier}'"),
            start,
            end,
        }
    }

    #[must_use]
    pub fn new(message: String, start: Location, end: Location) -> Self {
        Self {
            text: message,
            start,
            end,
        }
    }

    #[must_use]
    pub fn mutation_error(message: &str, start: Location, end: Location) -> Self {
        Self {
            text: format!("Mutation error: {message}"),
            start,
            end,
        }
    }
    #[must_use]
    pub fn type_error(message: &str, start: Location, end: Location) -> Self {
        Self {
            text: format!("Type error: {message}"),
            start,
            end,
        }
    }
    #[must_use]
    pub fn syntax_error(message: &str, start: Location, end: Location) -> Self {
        Self {
            text: format!("Syntax error: {message}"),
            start,
            end,
        }
    }

    #[must_use]
    pub fn io_error(err: &std::io::Error, start: Location, end: Location) -> Self {
        Self {
            text: format!("IO error: {err}"),
            start,
            end,
        }
    }

    #[must_use]
    pub fn out_of_bounds(index: usize, start: Location, end: Location) -> Self {
        Self {
            text: format!("Index ({index}) out of bounds"),
            start,
            end,
        }
    }

    #[must_use]
    pub fn key_not_found(key: &Value, start: Location, end: Location) -> Self {
        Self {
            text: format!("Key not found in map: {key}"),
            start,
            end,
        }
    }

    #[must_use]
    pub fn argument_error(message: &str, start: Location, end: Location) -> Self {
        Self {
            text: message.to_string(),
            start,
            end,
        }
    }
}

impl fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on line {}", self.text, self.start.line)
    }
}

impl fmt::Debug for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Error for EvaluationError {}

pub trait ErrorConverter: fmt::Debug + fmt::Display {
    fn as_evaluation_error(&self, start: Location, end: Location) -> EvaluationError;
}

impl<E> ErrorConverter for E
where
    E: fmt::Debug + fmt::Display,
{
    fn as_evaluation_error(&self, start: Location, end: Location) -> EvaluationError {
        EvaluationError {
            text: format!("{self}"),
            start,
            end,
        }
    }
}

// NOTE: this is called `IntoEvaluationResult` but it actually only takes care of the error part of an evaluation result.
// `EvaluationResult` always wants the `Ok` type to be `Value` but this converter doesn't care.
pub trait IntoEvaluationResult<R> {
    fn into_evaluation_result(self, start: Location, end: Location) -> Result<R, FunctionCarrier>;
}

impl<E, R> IntoEvaluationResult<R> for Result<R, E>
where
    E: ErrorConverter,
{
    fn into_evaluation_result(self, start: Location, end: Location) -> Result<R, FunctionCarrier> {
        self.map_err(|err| FunctionCarrier::EvaluationError(err.as_evaluation_error(start, end)))
    }
}

/// Takes a value from the Andy C runtime and converts it to a forward index respecting bi-directional
/// indexing rules.
fn value_to_forward_index(
    value: Value,
    size: usize,
    start: Location,
    end: Location,
) -> Result<usize, EvaluationError> {
    let typ = value.value_type();
    let index = i64::try_from(value).map_err(|_| {
        EvaluationError::type_error(
            &format!("cannot use {typ} to index into a sequence, possibly because it's too big"),
            start,
            end,
        )
    })?;

    if index.is_negative() {
        let index = usize::try_from(index.abs())
            .map_err(|_err| EvaluationError::syntax_error("invalid index too large", start, end))?;

        size.checked_sub(index)
            .ok_or_else(|| EvaluationError::syntax_error("index out of bounds", start, end))
    } else {
        usize::try_from(index).map_err(|_| EvaluationError::syntax_error("kapot", start, end))
    }
}

fn call_function_by_name(
    name: &str,
    evaluated_args: &[Value],
    environment: &EnvironmentRef,
    start: Location,
    end: Location,
) -> EvaluationResult {
    let values = environment.borrow().get_all_by_name(name);
    let result = try_call_function(&values, evaluated_args, environment, start, end);
    if let Err(FunctionCarrier::FunctionNotFound) = result {
        return Err(FunctionCarrier::EvaluationError(EvaluationError::new(
            format!("no function called '{name}' found matches the arguments"),
            start,
            end,
        )));
    }

    result
}

/// Executes a function with some extra steps
///     * `values`: a list of values that are attempted to be executed in the order they appear in
///     * `evaluated_args`: a slice of values passed as arguments to the function
///     * `environment`: the execution environment for the function
///     * `start`: beginning of the expression (for error reporting)
///     * `end`: end of the expression (for error reporting)
fn try_call_function(
    values: &[RefCell<Value>],
    evaluated_args: &[Value],
    environment: &EnvironmentRef,
    start: Location,
    end: Location,
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
            let result = call_function(function, evaluated_args, environment, start, end);

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
    evaluated_args: &[Value],
    environment: &EnvironmentRef,
    start: Location,
    end: Location,
) -> EvaluationResult {
    let result = function.borrow().call(evaluated_args, environment);

    match result {
        Err(FunctionCarrier::Return(value)) | Ok(value) => Ok(value),
        e @ Err(FunctionCarrier::EvaluationError(_) | FunctionCarrier::FunctionNotFound) => e,
        Err(carrier @ FunctionCarrier::IntoEvaluationError(_)) => Err(carrier.lift(start, end)),
    }
}
