use crate::ast::operator::{Operator, UnaryOperator};
use crate::ast::{Expression, Literal};
use std::cmp::Ordering;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Neg;

// struct Interpreter {}

pub trait Evaluate {
    fn evaluate(&self) -> Result<Literal, EvaluationError>;
}

impl Evaluate for Expression {
    fn evaluate(&self) -> Result<Literal, EvaluationError> {
        let literal = match self {
            Expression::Literal(l) => l.clone(),
            Expression::Unary {
                expression,
                operator,
            } => {
                let value = expression.evaluate()?;
                match (value, operator) {
                    (Literal::Integer(n), UnaryOperator::Neg) => Literal::Integer(n.neg()),
                    (Literal::True, UnaryOperator::Not) => Literal::False,
                    (Literal::False, UnaryOperator::Not) => Literal::True,
                    (_, UnaryOperator::Not) => {
                        return Err(EvaluationError::TypeError { message: "the '!' operator cannot be applied to this type".to_string() });
                    }
                    (_, UnaryOperator::Neg) => {
                        return Err(EvaluationError::TypeError { message: "this type cannot be negated".to_string() });
                    }
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
                apply_operator(left, *operator, right)?
            }
            Expression::Grouping(expr) => expr.evaluate()?,
        };

        Ok(literal)
    }
}

fn apply_operator(left: Literal, operator: Operator, right: Literal) -> Result<Literal, EvaluationError> {
    let literal = match (left, operator, right) {
        // Integer
        (Literal::Integer(a), op, Literal::Integer(b)) => match op {
            Operator::Equals => (a == b).into(),
            Operator::NotEquals => (a != b).into(),
            Operator::Greater => (a > b).into(),
            Operator::GreaterEquals => (a >= b).into(),
            Operator::Less => (a < b).into(),
            Operator::LessEquals => (a <= b).into(),
            Operator::Plus => (a + b).into(),
            Operator::Minus => (a - b).into(),
            Operator::Multiply => (a * b).into(),
            Operator::Divide => (a / b).into(),
            Operator::Modulo => a.rem_euclid(b).into(),
        },
        // Boolean
        (
            a @ Literal::True | a @ Literal::False,
            Operator::Equals,
            b @ Literal::True | b @ Literal::False,
        ) => (a == b).into(),
        (
            a @ Literal::True | a @ Literal::False,
            Operator::NotEquals,
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
                Operator::Equals => (comp == Ordering::Equal).into(),
                Operator::NotEquals => (comp != Ordering::Equal).into(),
                Operator::Greater => (comp == Ordering::Greater).into(),
                Operator::GreaterEquals => (comp != Ordering::Less).into(),
                Operator::Less => (comp == Ordering::Less).into(),
                Operator::LessEquals => (comp != Ordering::Greater).into(),
                Operator::Plus => Literal::String(format!("{}{}", a, b).to_string()),
                Operator::Modulo => {
                    return Err(EvaluationError::TypeError { message: "modulo operator is not defined on string".to_string() });
                }
                Operator::Minus => {
                    return Err(EvaluationError::TypeError { message: "subtraction is not defined on string".to_string() });
                }
                Operator::Multiply => {
                    return Err(EvaluationError::TypeError { message: "multiplication is not defined on two strings".to_string() });
                }
                Operator::Divide => {
                    return Err(EvaluationError::TypeError { message: "division is not defined on strings".to_string() });
                }
            }
        }

        (a, op, b) => {
            return Err(EvaluationError::TypeError { message: format!("operation {op} is not defined for {} and {}", a.type_name(), b.type_name()) });
        }
    };

    Ok(literal)
}

#[derive(Debug)]
pub enum EvaluationError {
    TypeError { message: String },
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // todo write a proper implementation
        match self {
            EvaluationError::TypeError { message } => write!(f, "{message}"),
        }
    }
}

impl Error for EvaluationError {}