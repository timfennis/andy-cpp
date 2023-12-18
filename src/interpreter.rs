use crate::ast::operator::{Operator, UnaryOperator};
use crate::ast::{Expression, Literal};
use std::cmp::Ordering;
use std::ops::Neg;

// struct Interpreter {}

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
                let left = left.evaluate();
                let right = right.evaluate();
                apply_operator(left, *operator, right)
            }
            Expression::Grouping(expr) => expr.evaluate(),
        }
    }
}

fn apply_operator(left: Literal, operator: Operator, right: Literal) -> Literal {
    match (left, operator, right) {
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
                Operator::Modulo => panic!("syntax error modulo not supported on strings"),
                Operator::Minus => panic!("syntax error cannot subtract strings"),
                Operator::Multiply => panic!("cannot multiply a string with another string"),
                Operator::Divide => panic!("syntax error cannot divide strings"),
            }
        }

        _ => panic!("Type error, not implemented"),
    }
}
