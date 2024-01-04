use crate::ast::literal::Literal;
use crate::lexer::{Token, TokenType};
use std::fmt;

pub enum Expression {
    Literal(Literal),
    Unary {
        operator_token: Token,
        expression: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator_token: Token,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Variable {
        token: Token,
    },
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Expression::Literal(lit) => write!(f, "{}", lit),
            Expression::Unary {
                operator_token: operator,
                expression,
            } => write!(f, "({} {:?})", operator, expression),
            Expression::Binary {
                left,
                operator_token: operator,
                right,
            } => write!(f, "({} {:?} {:?})", operator, left, right),
            Expression::Grouping(expr) => write!(f, "(group {:?})", expr),
            Expression::Variable { token } => {
                //TODO: fmt::Error::default() what does that even do?
                let identifier = token.identifier_or(fmt::Error::default())?;
                write!(f, "{identifier}")
            }
        }
    }
}
