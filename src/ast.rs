use crate::ast::operator::{Operator, UnaryOperator};
use std::fmt;

pub mod literal;
pub mod operator;
pub mod parser;

pub use crate::ast::literal::*;
pub use crate::ast::parser::{Parser, ParserError};

pub enum Expression {
    Literal(Literal),
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Expression::Literal(lit) => write!(f, "{:?}", lit),
            Expression::Unary {
                operator,
                expression,
            } => write!(f, "({:?} {:?})", operator, expression),
            Expression::Binary {
                left,
                operator,
                right,
            } => write!(f, "({:?} {:?} {:?})", operator, left, right),
            Expression::Grouping(expr) => write!(f, "(group {:?})", expr),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn printing() {
        let ast = Expression::Binary {
            left: Box::new(Expression::Unary {
                operator: UnaryOperator::Neg,
                expression: Box::new(Expression::Literal(Literal::Integer(123))),
            }),
            operator: Operator::Multiply,
            right: Box::new(Expression::Grouping(Box::new(Expression::Literal(
                Literal::Integer(69),
            )))),
        };

        assert_eq!(format!("{:?}", ast), "(* (- 123) (group 69))");
    }
}
