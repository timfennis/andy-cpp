use crate::ast::ParserError;
use crate::lexer;
use crate::lexer::Token;
use std::fmt;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Operator {
    // Comparison
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    // Math
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
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
            lexer::TokenType::Greater => Ok(Operator::Greater),
            lexer::TokenType::GreaterEqual => Ok(Operator::GreaterEquals),
            lexer::TokenType::Less => Ok(Operator::Less),
            lexer::TokenType::LessEqual => Ok(Operator::LessEquals),
            lexer::TokenType::Percent => Ok(Operator::Modulo),
            _ => Err(ParserError::ExpectedOperator {
                token: value.clone(),
            }),
        }
    }
}

impl fmt::Display for Operator {
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
                Operator::Greater => ">",
                Operator::GreaterEquals => ">=",
                Operator::Less => "<",
                Operator::LessEquals => "<=",
                Operator::Modulo => "%",
            }
        )
    }
}
impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Operator({})", self)
    }
}

pub enum UnaryOperator {
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
