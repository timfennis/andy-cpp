use crate::ast::operator::{Operator, UnaryOperator};
use crate::ast::{Expression, Literal};
use std::ops::Neg;

struct Interpreter {}

pub trait Evaluate {
    fn evaluate(&self) -> Literal;
}

impl Evaluate for Expression {
    fn evaluate(&self) -> Literal {
        match self {
            Expression::Literal(l) => l.clone(),
            Expression::Unary {
                expression,
                operator,
            } => {
                let value = expression.evaluate();
                match (value, operator) {
                    (Literal::Integer(n), UnaryOperator::Neg) => Literal::Integer(n.neg()),
                    (Literal::True, UnaryOperator::Not) => Literal::False,
                    (Literal::False, UnaryOperator::Not) => Literal::True,
                    (_, _) => panic!("case not implemented, probably error"),
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                fn int(n: i64) -> Literal {
                    Literal::Integer(n)
                }

                let left = left.evaluate();
                let right = right.evaluate();
                match (left, operator, right) {
                    // Integer
                    (Literal::Integer(a), Operator::Divide, Literal::Integer(b)) => a
                        .checked_div_euclid(b)
                        .expect("TODO: handle division by zero")
                        .into(),

                    (Literal::Integer(a), Operator::Multiply, Literal::Integer(b)) => a
                        .checked_mul(b)
                        .expect("TODO: handle integer overflow")
                        .into(),
                    (Literal::Integer(a), Operator::Minus, Literal::Integer(b)) => a
                        .checked_sub(b)
                        .expect("TODO: handle integer overflow")
                        .into(),
                    (Literal::Integer(a), Operator::Plus, Literal::Integer(b)) => a
                        .checked_add(b)
                        .expect("TODO: handle integer overflow")
                        .into(),
                    (Literal::Integer(a), Operator::Equals, Literal::Integer(b)) => (a == b).into(),
                    (Literal::Integer(a), Operator::NotEquals, Literal::Integer(b)) => {
                        (a != b).into()
                    }
                    (Literal::Integer(a), Operator::Greater, Literal::Integer(b)) => (a > b).into(),
                    (Literal::Integer(a), Operator::GreaterEquals, Literal::Integer(b)) => {
                        (a > b).into()
                    }
                    (Literal::Integer(a), Operator::Less, Literal::Integer(b)) => (a > b).into(),
                    (Literal::Integer(a), Operator::GreaterEquals, Literal::Integer(b)) => {
                        (a > b).into()
                    }
                    // String
                    (Literal::String(a), Operator::Plus, Literal::String(b)) => {
                        Literal::String(format!("{}{}", a, b).to_string())
                    } // TODO this can probably be optimized a lot
                    (Literal::String(a), Operator::Equals, Literal::String(b)) => (a == b).into(),
                    (Literal::String(a), Operator::NotEquals, Literal::String(b)) => {
                        (a != b).into()
                    }
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

                    _ => panic!("Type error, not implemented"),
                }
            }
            Expression::Grouping(expr) => expr.evaluate(),
        }
    }
}
impl Interpreter {
    pub fn evaluate(expr: Expression) -> Literal {
        expr.evaluate()
    }
}
