use crate::ast::parser::ParserError;
use crate::lexer;
use crate::lexer::Token;
use std::fmt;

pub mod parser;

enum Operator {
    Equals,
    NotEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl TryFrom<&Token> for Operator {
    type Error = ParserError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match &value.typ {
            lexer::TokenType::Minus => Ok(Operator::Minus),
            lexer::TokenType::Plus => Ok(Operator::Plus),
            lexer::TokenType::Star => Ok(Operator::Multiply),
            lexer::TokenType::Slash => Ok(Operator::Divide),
            lexer::TokenType::EqualEqual => Ok(Operator::Equals),
            lexer::TokenType::BangEqual => Ok(Operator::NotEquals),
            _ => Err(ParserError::ExpectedOperator {
                token: value.clone(),
            }),
        }
    }
}

impl TryFrom<&Token> for UnaryOperator {
    type Error = ParserError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match &value.typ {
            lexer::TokenType::Bang => Ok(UnaryOperator::Not),
            lexer::TokenType::Minus => Ok(UnaryOperator::Neg),
            _ => Err(ParserError::ExpectedOperator {
                token: value.clone(),
            }),
        }
    }
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Operator::Equals => "==",
                Operator::NotEquals => "!=",
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
            }
        )
    }
}

enum Literal {
    Integer(i64),
    String(String),
    True,
    False,
    Nil,
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Literal::Integer(n) => write!(f, "{n}"),
            Literal::String(val) => write!(f, "\"{val}\""),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

enum UnaryOperator {
    Neg,
    Not,
}

impl fmt::Debug for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

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
