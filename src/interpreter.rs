use crate::ast::operator::Operator;
use crate::ast::{Expression, Literal};
use std::cmp::Ordering;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::Neg;
use crate::lexer::Token;

pub trait Evaluate {
    fn evaluate(&self) -> Result<Literal, EvaluationError>;
}

impl Evaluate for Expression {
    fn evaluate(&self) -> Result<Literal, EvaluationError> {
        let literal = match self {
            Expression::Literal(l) => l.clone(),
            Expression::Unary {
                expression,
                operator_token,
            } => {
                let value = expression.evaluate()?;
                let operator: Operator = operator_token.try_into()?;
                match (value, operator) {
                    (Literal::Integer(n), Operator::Minus) => Literal::Integer(n.neg()),
                    (Literal::True, Operator::Bang) => Literal::False,
                    (Literal::False, Operator::Bang) => Literal::True,
                    (_, Operator::Bang) => {
                        return Err(EvaluationError::TypeError {
                            message: "the '!' operator cannot be applied to this type".to_string(),
                        });
                    }
                    (_, Operator::Minus) => {
                        return Err(EvaluationError::TypeError {
                            message: "this type cannot be negated".to_string(),
                        });
                    }
                    _ => panic!("invalid unary operator encountered"),
                }
            }
            Expression::Binary {
                left,
                operator_token,
                right,
            } => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
                apply_operator(left, operator_token, right)?
            }
            Expression::Grouping(expr) => expr.evaluate()?,
        };

        Ok(literal)
    }
}

fn apply_operator(
    left: Literal,
    operator_token: &Token,
    right: Literal,
) -> Result<Literal, EvaluationError> {
    // Some temporary functions to make errors
    let mk_int_overflow = || EvaluationError::IntegerOverflow { operator_token: operator_token.clone() };
    let mk_div_zero = || EvaluationError::DivisionByZero { operator_token: operator_token.clone() };

    let operator: Operator = operator_token.try_into()?;
    let literal = match (left, operator, right) {
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
                let exponent  = u32::try_from(b)?;
                a.checked_pow(exponent)
                    .ok_or_else(mk_int_overflow)?
                    .into()
            },
            _ => {
                return Err(EvaluationError::InvalidOperator {
                    operator_token: operator_token.clone(),
                    type_a: Literal::Integer(a),
                    type_b: Literal::Integer(b),
                });
            }
        },
        // Boolean
        (
            a @ Literal::True | a @ Literal::False,
            Operator::Equality,
            b @ Literal::True | b @ Literal::False,
        ) => (a == b).into(),
        (
            a @ Literal::True | a @ Literal::False,
            Operator::Inequality,
            b @ Literal::True | b @ Literal::False,
        ) => (a != b).into(),

        // Some mixed memes
        (Literal::String(a), Operator::Multiply, Literal::Integer(b)) => {
            Literal::String(a.repeat(b as usize))
        }
        (Literal::Integer(a), Operator::Multiply, Literal::String(b)) => {
            Literal::String(b.repeat(a as usize))
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
                Operator::Plus => Literal::String(format!("{}{}", a, b).to_string()),
                _ => {
                    return Err(EvaluationError::InvalidOperator {
                        operator_token: operator_token.clone(),
                        type_a: Literal::String(a),
                        type_b: Literal::String(b),
                    });
                }
            }
        }

        (a, _op, b) => {
            return Err(EvaluationError::InvalidOperator {
                operator_token: operator_token.clone(),
                type_a: a,
                type_b: b,
            });
        }
    };

    Ok(literal)
}

#[derive(Debug)]
pub enum EvaluationError {
    TypeError {
        message: String,
    },
    OperatorExpected {
        got: Token,
    },
    InvalidOperator {
        operator_token: Token,
        type_a: Literal,
        type_b: Literal,
    },
    IntegerOverflow {
        operator_token: Token,
    },
    DivisionByZero {
        operator_token: Token,
    },
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // todo write a proper implementation
        match self {
            EvaluationError::TypeError { message } => write!(f, "{message}"),
            EvaluationError::OperatorExpected { got } => write!(
                f,
                "expected an operator during evaluation but got a {} on line {} column {}",
                got.typ, got.line, got.column
            ),
            EvaluationError::InvalidOperator {
                operator_token: op,
                type_a,
                type_b,
            } => write!(
                f,
                "unable to apply the '{}' operator to {} and {} on line {} column {}",
                op.typ, type_a.type_name(), type_b.type_name(), op.line, op.column
            ),
            EvaluationError::IntegerOverflow { operator_token } => write!(f, "integer overflow while applying the '{}' operator on line {} column {}", operator_token.typ, operator_token.line, operator_token.column),
            EvaluationError::DivisionByZero { operator_token } => write!(f, "division by zero when applying '{}' on line {} column {}", operator_token.typ, operator_token.line, operator_token.column)
        }
    }
}

impl Error for EvaluationError {}

impl From<TryFromIntError> for EvaluationError {
    fn from(_value: TryFromIntError) -> Self {
        EvaluationError::TypeError { message: "cannot convert between integer types, possibly because you're using a large exponent".to_string() }
    }
}
