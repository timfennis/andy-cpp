use crate::ast::Error as ParseError;
use crate::lexer::{Token, TokenLocation};
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Not,
    Neg,
}

impl TryFrom<TokenLocation> for UnaryOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Minus => Self::Neg,
            Token::Bang | Token::LogicNot => Self::Not,
            _ => {
                return Err(ParseError::ExpectedToken {
                    expected_tokens: vec![Token::Minus, Token::Bang],
                    actual_token: value,
                })
            }
        })
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Equality,
    Inequality,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
    CModulo,
    EuclideanModulo,
    Exponent,
    And,
    Or,
    Concat,
    In,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

impl TryFrom<TokenLocation> for LogicalOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::LogicAnd => Self::And,
            Token::LogicOr => Self::Or,
            _ => {
                return Err(ParseError::ExpectedToken {
                    actual_token: value,
                    expected_tokens: vec![Token::LogicAnd, Token::LogicOr],
                })
            }
        })
    }
}

impl TryFrom<TokenLocation> for BinaryOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Equality => Self::Equality,
            Token::Inequality => Self::Inequality,
            Token::Greater => Self::Greater,
            Token::GreaterEquals => Self::GreaterEquals,
            Token::Less => Self::Less,
            Token::LessEquals => Self::LessEquals,
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Multiply => Self::Multiply,
            Token::Divide => Self::Divide,
            Token::CModulo => Self::CModulo,
            Token::EuclideanModulo => Self::EuclideanModulo,
            Token::Caret => Self::Exponent,
            Token::Ampersand => Self::And,
            Token::Pipe => Self::Or,
            Token::In => Self::In,
            Token::Concat => Self::Concat,
            _ => {
                return Err(ParseError::ExpectedToken {
                    actual_token: value,
                    expected_tokens: vec![
                        Token::Equality,
                        Token::Inequality,
                        Token::Greater,
                        Token::GreaterEquals,
                        Token::Less,
                        Token::LessEquals,
                        Token::Plus,
                        Token::Minus,
                        Token::Multiply,
                        Token::Divide,
                        Token::CModulo,
                        Token::EuclideanModulo,
                        Token::Caret,
                        Token::Ampersand,
                        Token::Pipe,
                        Token::In,
                        Token::Concat,
                    ],
                })
            }
        })
    }
}

impl fmt::Debug for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op = match self {
            BinaryOperator::Equality => "==",
            BinaryOperator::Inequality => "!=",
            BinaryOperator::Greater => ">",
            BinaryOperator::GreaterEquals => ">=",
            BinaryOperator::Less => "<",
            BinaryOperator::LessEquals => "<=",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::CModulo => "%",
            BinaryOperator::EuclideanModulo => "%%",
            BinaryOperator::Exponent => "^",
            BinaryOperator::And => "&",
            BinaryOperator::Or => "|",
            BinaryOperator::Concat => "++",
            BinaryOperator::In => "in",
        };
        write!(f, "{op}")
    }
}
