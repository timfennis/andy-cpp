use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt;
use std::ops::{IndexMut, Neg, Rem};
use std::rc::Rc;

use crate::ast::{
    BinaryOperator, Expression, ExpressionLocation, LogicalOperator, Lvalue, UnaryOperator,
};
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::function::{Closure, FunctionCarrier};
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
        Expression::Unary {
            expression: expression_location,
            operator,
        } => {
            let value = evaluate_expression(expression_location, environment)?;
            match (value, operator) {
                (Value::Number(n), UnaryOperator::Neg) => Value::Number(n.neg()),
                (Value::Bool(b), UnaryOperator::Bang) => Value::Bool(!b),
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
                todo!("other lvalues are not implemented in declaration");
            };

            let value = evaluate_expression(value, environment)?;

            if environment.borrow().contains(identifier) {
                let new_env = Environment::new_scope(environment);
                *environment = new_env;
            }
            environment.borrow_mut().declare(identifier, value.clone());

            value
        }
        Expression::VariableAssignment { l_value, value } => match l_value {
            Lvalue::Variable { identifier } => {
                if !environment.borrow().contains(identifier) {
                    Err(EvaluationError::syntax_error(
                        &format!("undefined variable {identifier}"),
                        start,
                        end,
                    ))?;
                }

                let value = evaluate_expression(value, environment)?;
                environment
                    .borrow_mut()
                    .assign(identifier.clone(), value.clone());
                value
            }
            Lvalue::Index {
                value: assign_to,
                index,
            } => {
                let assign_to = evaluate_expression(assign_to, environment)?;

                let index_value = evaluate_expression(index, environment)?;
                let value = evaluate_expression(value, environment)?;

                let index = usize::try_from(index_value).map_err(|e| {
                    FunctionCarrier::from(EvaluationError::syntax_error(
                        &format!("invalid index {e}"),
                        start,
                        end,
                    ))
                })?;

                // TODO: it's a little wasteful that we're first evaluating all the expressions
                //       before we check if we can even assign to the thing
                match &assign_to {
                    Value::Sequence(Sequence::List(list)) => {
                        let mut list = list.borrow_mut();
                        let x = list.index_mut(index);
                        *x = value;
                    }
                    Value::Sequence(Sequence::String(string)) => {
                        let mut string = string.borrow_mut();
                        let value = value.to_string(); // TODO: this is almost certainly wrong
                        string.replace_range(index..=index, &value);
                    }
                    _ => {
                        todo!("cannot assign index on this type");
                        // return Err(EvaluationError::syntax_error("", start, end).into());
                    }
                }

                assign_to
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
            expression,
            on_true,
            on_false,
        } => {
            let result = evaluate_expression(expression, environment)?;

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
            let mut local_scope = Environment::new_scope(environment);
            loop {
                let lit = evaluate_expression(expression, &mut local_scope)?;
                if lit == Value::Bool(true) {
                    evaluate_expression(loop_body, &mut local_scope)?;
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
            drop(local_scope);
            Value::Unit
        }
        Expression::BoolLiteral(b) => Value::Bool(*b),
        Expression::StringLiteral(s) => {
            // TODO: to_string will make a copy, is this the best way to handle these types
            //       or should we reconsider having String in an Rc for Expression because that
            //       was probably only convenient when the our values used just Rcs
            Value::Sequence(Sequence::String(Rc::new(RefCell::new(s.to_string()))))
        }
        Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
        Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
        Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
        Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
        Expression::UnitLiteral => Value::Unit,
        Expression::Call {
            function,
            arguments,
        } => {
            // The Expression in `function` must either be an identifier in which case it will be looked up in the
            // environment, or it must be some expression that evaluates to a function.
            let function = if let Expression::Identifier(name) = &function.expression {
                if let Some(refcell) = environment.borrow().get(name) {
                    let value = &*refcell.borrow();
                    if let Value::Function(function) = value {
                        function.clone()
                    } else {
                        return Err(EvaluationError::syntax_error(
                            &format!("{name} is not a function"),
                            expression_location.start,
                            expression_location.end,
                        )
                        .into());
                    }
                } else {
                    return Err(EvaluationError::syntax_error(
                        &format!("undefined function {name}"),
                        expression_location.start,
                        expression_location.end,
                    )
                    .into());
                }
            } else {
                let value = evaluate_expression(function, environment)?;
                if let Value::Function(function) = value {
                    function.clone()
                } else {
                    return Err(EvaluationError::syntax_error(
                        &format!("{} is not callable", ValueType::from(&value)),
                        // FIXME: this is the location of the expression and not the parentheses that make this a function call
                        expression_location.start,
                        expression_location.end,
                    )
                    .into());
                }
            };

            let mut evaluated_args = Vec::new();

            for argument in arguments {
                evaluated_args.push(evaluate_expression(argument, environment)?);
            }

            return function.call(&evaluated_args, environment);
        }
        Expression::FunctionDeclaration {
            arguments,
            body,
            name,
        } => {
            let name = name.try_into_identifier()?;

            let user_function = Closure {
                parameters: arguments.try_into_parameters()?,
                body: body.clone(),
                environment: environment.clone(),
            };

            environment
                .borrow_mut()
                .declare(&name, Value::Function(Rc::new(user_function)));
            Value::Unit
        }
        Expression::Tuple { .. } => todo!("tuples are not yet implemented in this position"),
        Expression::Identifier(identifier) => {
            if let Some(value) = environment.borrow().get(identifier) {
                // TODO: is cloning the value a good idea here??
                value.borrow().clone()
            } else {
                return Err(EvaluationError::syntax_error(
                    &format!("undefined variable {identifier}"),
                    expression_location.start,
                    expression_location.end,
                )
                .into());
            }
        }
        Expression::List { values } => {
            let mut values_out = VecDeque::with_capacity(values.len());
            let mut last_type = None;
            for expression in values {
                let v = evaluate_expression(expression, environment)?;
                if last_type.is_none() {
                    last_type = Some(ValueType::from(&v));
                } else if last_type != Some(ValueType::from(&v)) {
                    let add_type = ValueType::from(&v);
                    let last_type = last_type.unwrap();
                    return Err(EvaluationError::type_error(
                        &format!("cannot add {add_type} to a list of {last_type}",),
                        expression.start,
                        expression.end,
                    )
                    .into());
                }
                values_out.push_back(v);
            }
            Value::Sequence(Sequence::List(Rc::new(RefCell::new(values_out))))
        }
        Expression::For {
            l_value,
            sequence,
            loop_body,
        } => {
            let Value::Sequence(sequence) = evaluate_expression(sequence, environment)? else {
                // TODO: fix this error
                panic!("can only iterate over sequences")
            };

            let Lvalue::Variable {
                identifier: var_name,
            } = l_value
            else {
                // TODO: fix the location of this error
                return Err(EvaluationError::syntax_error(
                    "cannot use this expression in for loop",
                    Location { line: 0, column: 0 },
                    Location { line: 0, column: 0 },
                )
                .into());
            };

            match sequence {
                Sequence::String(str) => {
                    let str = str.borrow();
                    for n in 0..str.len() {
                        let mut scope = Environment::new_scope(environment);
                        let substr = &str[n..=n];

                        // TODO: allocating a new string here is probably not optimal
                        scope.borrow_mut().declare(
                            var_name,
                            Value::Sequence(Sequence::String(Rc::new(RefCell::new(String::from(
                                substr,
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

                        // TODO: is this clone here reasonable or should we look into getting an owned value
                        scope.borrow_mut().declare(var_name, x.clone());

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
            // let value_type = value.value_type();
            match value {
                Value::Sequence(sequence) => {
                    let index = evaluate_expression(index_expr, environment)?;
                    let Value::Number(Number::Int(Int::Int64(index))) = index else {
                        return Err(EvaluationError::syntax_error(
                            &format!("Index must be a int but was {}", index.value_type()),
                            index_expr.start,
                            index_expr.end,
                        )
                        .into());
                    };

                    match sequence {
                        Sequence::String(string) => {
                            let string = string.borrow();
                            let index = convert_index(index, string.len()).ok_or_else(|| {
                                EvaluationError::type_error(
                                    "cannot convert index to usable offset",
                                    index_expr.start,
                                    index_expr.end,
                                )
                            })?;

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
                            let list = list.borrow();
                            let index = convert_index(index, list.len()).ok_or_else(|| {
                                EvaluationError::type_error(
                                    "cannot convert index to usable offset",
                                    index_expr.start,
                                    index_expr.end,
                                )
                            })?;

                            let Some(value) = list.get(index) else {
                                return Err(EvaluationError::out_of_bounds(
                                    index,
                                    index_expr.start,
                                    index_expr.end,
                                )
                                .into());
                            };
                            value.clone() //TODO remove clone?
                        }
                    }
                }
                // TODO: improve error handling
                _ => {
                    return Err(EvaluationError::type_error(
                        "cannot index into this type",
                        value_expr.start,
                        value_expr.end,
                    )
                    .into())
                }
            }
        }
    };

    Ok(literal)
}

fn apply_operator(
    left: Value,
    operator: BinaryOperator,
    right: Value,
) -> Result<Value, EvaluationError> {
    let literal: Value = match (left, operator, right) {
        // Integer
        (Value::Number(a), op, Value::Number(b)) => match op {
            BinaryOperator::Equality => (a == b).into(),
            BinaryOperator::Inequality => (a != b).into(),
            BinaryOperator::Greater => (a > b).into(),
            BinaryOperator::GreaterEquals => (a >= b).into(),
            BinaryOperator::Less => (a < b).into(),
            BinaryOperator::LessEquals => (a <= b).into(),
            // Math operations
            BinaryOperator::Plus => Value::Number(a + b),
            BinaryOperator::Minus => Value::Number(a - b),
            BinaryOperator::Multiply => Value::Number(a * b),
            BinaryOperator::Divide => Value::Number(a / b),
            BinaryOperator::CModulo => Value::Number(a.rem(b)),
            BinaryOperator::EuclideanModulo => Value::Number(a.checked_rem_euclid(b)?),
            BinaryOperator::Exponent => Value::Number(a.checked_pow(b)?),
        },
        // Boolean
        (Value::Bool(a), BinaryOperator::Equality, Value::Bool(b)) => (a == b).into(),
        (Value::Bool(a), BinaryOperator::Inequality, Value::Bool(b)) => (a != b).into(),

        // Some mixed memes
        (Value::Sequence(Sequence::String(a)), BinaryOperator::Multiply, Value::Number(b)) => {
            let a = a.borrow();
            Value::Sequence(Sequence::String(Rc::new(RefCell::new(a.repeat(
                usize::try_from(b).map_err(|_err| {
                    EvaluationError::type_error(
                        "can't multiply a string with this value",
                        // TODO: somehow figure out the line number, or defer creation of this error to another location
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    )
                })?,
            )))))
        }
        (Value::Number(a), BinaryOperator::Multiply, Value::Sequence(Sequence::String(b))) => {
            let b = b.borrow();
            Value::Sequence(Sequence::String(Rc::new(RefCell::new(b.repeat(
                usize::try_from(a).map_err(|_| {
                    EvaluationError::type_error(
                        "can't multiply a string with this value",
                        // TODO: somehow figure out the line number, or defer creation of this error to another location
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    )
                })?,
            )))))
        }

        // String apply operators to string
        (Value::Sequence(Sequence::String(a)), op, Value::Sequence(Sequence::String(b))) => {
            let comp = a.cmp(&b);
            match op {
                BinaryOperator::Equality => (comp == Ordering::Equal).into(),
                BinaryOperator::Inequality => (comp != Ordering::Equal).into(),
                BinaryOperator::Greater => (comp == Ordering::Greater).into(),
                BinaryOperator::GreaterEquals => (comp != Ordering::Less).into(),
                BinaryOperator::Less => (comp == Ordering::Less).into(),
                BinaryOperator::LessEquals => (comp != Ordering::Greater).into(),
                BinaryOperator::Plus => Value::Sequence(Sequence::String(Rc::new(RefCell::new(
                    format!("{}{}", a.borrow(), b.borrow()),
                )))),
                _ => {
                    return Err(EvaluationError::type_error(
                        &format!("cannot apply operator {operator:?} to string and string"),
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    ));
                }
            }
        }

        (a, _op, b) => {
            return Err(EvaluationError::type_error(
                &format!(
                    "cannot apply operator {operator:?} to {} and {}",
                    ValueType::from(&a),
                    ValueType::from(&b)
                ),
                Location { line: 0, column: 0 },
                Location { line: 0, column: 0 },
            ));
        }
    };

    Ok(literal)
}

pub struct EvaluationError {
    text: String,
    start: Location,
    #[allow(unused)]
    end: Location,
}

impl EvaluationError {
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
}
impl From<std::io::Error> for EvaluationError {
    fn from(value: std::io::Error) -> Self {
        Self {
            text: format!("io error: {value:?}"),
            // TODO: fix start/end
            start: Location { line: 0, column: 0 },
            end: Location { line: 0, column: 0 },
        }
    }
}

impl fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.text, self.start)
    }
}

impl fmt::Debug for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Error for EvaluationError {}

fn convert_index(index: i64, size: usize) -> Option<usize> {
    if index.is_negative() {
        usize::try_from(index.abs())
            .ok()
            .and_then(|i| size.checked_sub(i))
    } else {
        usize::try_from(index).ok()
    }
}
