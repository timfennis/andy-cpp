use crate::ast::Operator;
use crate::interpreter::{Sequence, Value, ValueType};
use crate::lexer::Location;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::Rem;
use std::rc::Rc;

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
    IO {
        cause: std::io::Error,
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
