use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::{Neg, Rem};
use std::rc::Rc;

use crate::ast::{
    Expression, ExpressionLocation, LogicalOperator, Lvalue, Operator, UnaryOperator,
};
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::function::Closure;
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value, ValueType};
use crate::lexer::Location;

#[allow(clippy::too_many_lines)]
pub(crate) fn evaluate_expression(
    expression_location: &ExpressionLocation,
    environment: &mut EnvironmentRef,
) -> Result<Value, EvaluationError> {
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
                    return Err(EvaluationError::TypeError {
                        message: "the '!' operator cannot be applied to this type".to_string(),
                    });
                }
                (_, UnaryOperator::Neg) => {
                    return Err(EvaluationError::TypeError {
                        message: "this type cannot be negated".to_string(),
                    });
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
        Expression::VariableDeclaration {
            l_value: Lvalue::Variable { identifier },
            value,
        } => {
            let value = evaluate_expression(value, environment)?;

            if environment.borrow().contains(identifier) {
                let new_env = Environment::new_scope(environment);
                *environment = new_env;
            }
            environment.borrow_mut().declare(identifier, value.clone());

            value
        }
        Expression::VariableAssignment {
            l_value: Lvalue::Variable { identifier },
            value,
        } => {
            if !environment.borrow().contains(identifier) {
                return Err(EvaluationError::UndefinedVariable {
                    identifier: identifier.clone(),
                    start,
                    end,
                });
            }
            let value = evaluate_expression(value, environment)?;
            environment
                .borrow_mut()
                .assign(identifier.clone(), value.clone());
            value
        }
        Expression::BlockExpression { statements } => {
            let mut local_scope = Environment::new_scope(environment);

            let mut value = Value::Unit;
            for stm in statements {
                value = evaluate_expression(stm, &mut local_scope)?;
            }

            drop(local_scope);
            value
        }
        Expression::IfExpression {
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
                    return Err(EvaluationError::TypeError {
                        message: format!(
                            "mismatched types: expected bool, found {}",
                            ValueType::from(value)
                        ),
                    })
                }
            }
        }
        Expression::Statement(expression) => {
            evaluate_expression(expression, environment)?;
            Value::Unit
        }
        Expression::Print(expression) => {
            let value = evaluate_expression(expression, environment)?;
            let _ = environment
                .borrow_mut()
                .with_output(|output| writeln!(output, "{value}"));
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
                    return Err(EvaluationError::TypeError {
                        message: format!(
                            "Cannot apply logical operator to non bool value {}",
                            ValueType::from(value)
                        ),
                    })
                }
            }
        }
        Expression::WhileExpression {
            expression,
            loop_body,
        } => {
            let mut local_scope = Environment::new_scope(environment);
            loop {
                let lit = evaluate_expression(expression, &mut local_scope)?;
                if let Value::Bool(true) = lit {
                    evaluate_expression(loop_body, &mut local_scope)?;
                } else if let Value::Bool(false) = lit {
                    break;
                } else {
                    return Err(EvaluationError::TypeError {
                        message: "Expression in a while structure must return a bool".to_string(),
                    });
                }
            }
            drop(local_scope);
            Value::Unit
        }
        Expression::BoolLiteral(b) => Value::Bool(*b),
        Expression::StringLiteral(s) => Value::Sequence(Sequence::String(s.clone())),
        Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
        Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
        Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
        Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
        Expression::Call {
            function_identifier,
            arguments,
        } => {
            // TODO: Maybe check if the function exists before we evaluate the arguments.
            //       the reason we're doing it in this order is to not have to fight the borrowchecker
            let mut evaluated_args = Vec::new();

            let Expression::Tuple { ref values } = arguments.expression else {
                panic!("the parser must guarantee that arguments is a tuple");
            };

            for argument in values {
                evaluated_args.push(evaluate_expression(argument, environment)?);
            }

            let function_name = function_identifier.try_into_identifier()?;
            let function = if let Some(refcell) = environment.borrow().get(&function_name) {
                let value = &*refcell.borrow();
                if let Value::Function(function) = value {
                    function.clone()
                } else {
                    return Err(EvaluationError::UndefinedFunction {
                        identifier: function_name,
                        start: expression_location.start,
                        end: expression_location.end,
                    });
                }
            } else {
                return Err(EvaluationError::UndefinedFunction {
                    identifier: function_name,
                    start: expression_location.start,
                    end: expression_location.end,
                });
            };
            function.call(&evaluated_args, environment)?
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
                return Err(EvaluationError::UndefinedVariable {
                    identifier: identifier.clone(),
                    start: expression_location.start,
                    end: expression_location.end,
                });
            }
        }
    };

    Ok(literal)
}

pub fn apply_operator(
    left: Value,
    operator: Operator,
    right: Value,
) -> Result<Value, EvaluationError> {
    let literal: Value = match (left, operator, right) {
        // Integer
        (Value::Number(a), op, Value::Number(b)) => match op {
            Operator::Equality => (a == b).into(),
            Operator::Inequality => (a != b).into(),
            Operator::Greater => (a > b).into(),
            Operator::GreaterEquals => (a >= b).into(),
            Operator::Less => (a < b).into(),
            Operator::LessEquals => (a <= b).into(),
            // Math operations
            Operator::Plus => Value::Number(a + b),
            Operator::Minus => Value::Number(a - b),
            Operator::Multiply => Value::Number(a * b),
            Operator::Divide => Value::Number(a / b),
            Operator::CModulo => Value::Number(a.rem(b)),
            Operator::EuclideanModulo => Value::Number(a.checked_rem_euclid(b)?),
            Operator::Exponent => Value::Number(a.checked_pow(b)?),
        },
        // Boolean
        (Value::Bool(a), Operator::Equality, Value::Bool(b)) => (a == b).into(),
        (Value::Bool(a), Operator::Inequality, Value::Bool(b)) => (a != b).into(),

        // Some mixed memes
        (Value::Sequence(Sequence::String(a)), Operator::Multiply, Value::Number(b)) => {
            Value::Sequence(Sequence::String(Rc::new(a.repeat(
                usize::try_from(b).map_err(|()| EvaluationError::TypeError {
                    message: String::from("Cannot convert"),
                })?,
            ))))
        }
        (Value::Number(a), Operator::Multiply, Value::Sequence(Sequence::String(b))) => {
            Value::Sequence(Sequence::String(Rc::new(b.repeat(
                usize::try_from(a).map_err(|()| EvaluationError::TypeError {
                    message: String::from("Cannot convert"),
                })?,
            ))))
        }

        // String apply operators to string
        (Value::Sequence(Sequence::String(a)), op, Value::Sequence(Sequence::String(b))) => {
            let comp = a.cmp(&b);
            match op {
                Operator::Equality => (comp == Ordering::Equal).into(),
                Operator::Inequality => (comp != Ordering::Equal).into(),
                Operator::Greater => (comp == Ordering::Greater).into(),
                Operator::GreaterEquals => (comp != Ordering::Less).into(),
                Operator::Less => (comp == Ordering::Less).into(),
                Operator::LessEquals => (comp != Ordering::Greater).into(),
                Operator::Plus => {
                    Value::Sequence(Sequence::String(Rc::new(format!("{a}{b}").to_string())))
                }
                _ => {
                    return Err(EvaluationError::InvalidOperator {
                        operator,
                        type_a: ValueType::String,
                        type_b: ValueType::String,
                    });
                }
            }
        }

        (a, _op, b) => {
            return Err(EvaluationError::InvalidOperator {
                operator,
                type_a: a.into(),
                type_b: b.into(),
            });
        }
    };

    Ok(literal)
}

pub enum EvaluationError {
    TypeError {
        message: String,
    },
    InvalidOperator {
        operator: Operator,
        type_a: ValueType,
        type_b: ValueType,
    },
    IntegerOverflow {
        operator: Operator,
    },
    DivisionByZero {
        operator: Operator,
    },
    UndefinedVariable {
        identifier: String,
        start: Location,
        end: Location,
    },
    UndefinedFunction {
        identifier: String,
        start: Location,
        end: Location,
    },
    IO {
        cause: std::io::Error,
    },
    InvalidExpression {
        expected_type: String,
        start: Location,
        end: Location,
    },
}

impl From<std::io::Error> for EvaluationError {
    fn from(value: std::io::Error) -> Self {
        Self::IO { cause: value }
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // todo write a proper implementation
        match self {
            EvaluationError::TypeError { message } => write!(f, "{message}"),
            EvaluationError::InvalidOperator {
                operator: op,
                type_a,
                type_b,
            } => write!(
                f,
                "unable to apply the '{:?}' operator to {} and {} on line {} column {}",
                op,
                type_a,
                type_b,
                0,
                0 // TODO: fix this error
            ),
            EvaluationError::IntegerOverflow {
                operator: operator_token,
            } => write!(
                f,
                "integer overflow while applying the '{:?}' operator on line {} column {}",
                operator_token, 0, 0
            ),
            EvaluationError::DivisionByZero {
                operator: operator_token,
            } => write!(
                f,
                "division by zero when applying '{:?}' on line {} column {}",
                operator_token, 0, 0
            ),
            EvaluationError::UndefinedVariable {
                identifier, start, ..
            } => write!(f, "variable {identifier} is undefined on {start}",),
            EvaluationError::IO { cause } => write!(f, "IO error: {cause}"),
            EvaluationError::UndefinedFunction {
                identifier,
                start,
                end: _end,
            } => {
                write!(f, "undefined function '{identifier}' on {start}")
            }
            EvaluationError::InvalidExpression {
                expected_type,
                start,
                end: _end,
            } => write!(f, "invalid expression: expected {expected_type} on {start}"),
        }
    }
}

impl fmt::Debug for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Error for EvaluationError {}

impl From<TryFromIntError> for EvaluationError {
    fn from(err: TryFromIntError) -> Self {
        EvaluationError::TypeError {
            message: format!("cannot convert between integer types {err}"),
        }
    }
}
