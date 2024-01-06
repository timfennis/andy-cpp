use crate::ast::{Literal, Operator};
use crate::lexer::{IdentifierToken, OperatorToken};
use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;

pub fn apply_operator(
    left: Literal,
    operator_token: OperatorToken,
    right: Literal,
) -> Result<Literal, EvaluationError> {
    // Some temporary functions to make errors
    let mk_int_overflow = || EvaluationError::IntegerOverflow { operator_token };
    let mk_div_zero = || EvaluationError::DivisionByZero { operator_token };

    let literal = match (left, operator_token.operator, right) {
        // Integer
        (Literal::Integer(a), op, Literal::Integer(b)) => match op {
            Operator::Equality => (a == b).into(),
            Operator::Inequality => (a != b).into(),
            Operator::Greater => (a > b).into(),
            Operator::GreaterEquals => (a >= b).into(),
            Operator::Less => (a < b).into(),
            Operator::LessEquals => (a <= b).into(),
            // Math operations
            Operator::Plus => (a.checked_add(b)).ok_or_else(mk_int_overflow)?.into(),
            Operator::Minus => (a.checked_sub(b)).ok_or_else(mk_int_overflow)?.into(),
            Operator::Multiply => (a.checked_mul(b)).ok_or_else(mk_int_overflow)?.into(),
            Operator::Divide => (a.checked_div(b)).ok_or_else(mk_div_zero)?.into(),
            Operator::CModulo => a.checked_rem(b).ok_or_else(mk_div_zero)?.into(),
            Operator::EuclideanModulo => a.checked_rem_euclid(b).ok_or_else(mk_div_zero)?.into(),
            //TODO: better error handling when casting to u32
            Operator::Exponent => {
                let exponent = u32::try_from(b)?;
                a.checked_pow(exponent).ok_or_else(mk_int_overflow)?.into()
            }
            _ => {
                return Err(EvaluationError::InvalidOperator {
                    operator_token,
                    type_a: Literal::Integer(a),
                    type_b: Literal::Integer(b),
                });
            }
        },
        // Boolean
        (
            a @ (Literal::True | Literal::False),
            Operator::Equality,
            b @ (Literal::True | Literal::False),
        ) => (a == b).into(),
        (
            a @ (Literal::True | Literal::False),
            Operator::Inequality,
            b @ (Literal::True | Literal::False),
        ) => (a != b).into(),

        // Some mixed memes
        (Literal::String(a), Operator::Multiply, Literal::Integer(b)) => {
            Literal::String(a.repeat(usize::try_from(b)?))
        }
        (Literal::Integer(a), Operator::Multiply, Literal::String(b)) => {
            Literal::String(b.repeat(usize::try_from(a)?))
        }

        // String apply operators to strings
        (Literal::String(a), op, Literal::String(b)) => {
            let comp = a.cmp(&b);
            match op {
                Operator::Equality => (comp == Ordering::Equal).into(),
                Operator::Inequality => (comp != Ordering::Equal).into(),
                Operator::Greater => (comp == Ordering::Greater).into(),
                Operator::GreaterEquals => (comp != Ordering::Less).into(),
                Operator::Less => (comp == Ordering::Less).into(),
                Operator::LessEquals => (comp != Ordering::Greater).into(),
                Operator::Plus => Literal::String(format!("{a}{b}").to_string()),
                _ => {
                    return Err(EvaluationError::InvalidOperator {
                        operator_token,
                        type_a: Literal::String(a),
                        type_b: Literal::String(b),
                    });
                }
            }
        }

        (a, _op, b) => {
            return Err(EvaluationError::InvalidOperator {
                operator_token,
                type_a: a,
                type_b: b,
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
        operator_token: OperatorToken,
        type_a: Literal,
        type_b: Literal,
    },
    IntegerOverflow {
        operator_token: OperatorToken,
    },
    DivisionByZero {
        operator_token: OperatorToken,
    },
    UndefinedVariable {
        token: IdentifierToken,
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
                operator_token: op,
                type_a,
                type_b,
            } => write!(
                f,
                "unable to apply the '{}' operator to {} and {} on line {} column {}",
                op.operator,
                type_a.type_name(),
                type_b.type_name(),
                op.start.line,
                op.start.column
            ),
            EvaluationError::IntegerOverflow { operator_token } => write!(
                f,
                "integer overflow while applying the '{}' operator on line {} column {}",
                operator_token.operator, operator_token.start.line, operator_token.start.column
            ),
            EvaluationError::DivisionByZero { operator_token } => write!(
                f,
                "division by zero when applying '{}' on line {} column {}",
                operator_token.operator, operator_token.start.line, operator_token.start.column
            ),
            EvaluationError::UndefinedVariable { token } => write!(
                f,
                "variable {} is undefined on line {} column {}",
                token, token.start.line, token.start.column
            ),
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
