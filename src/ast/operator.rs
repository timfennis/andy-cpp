use crate::ast::Error as ParseError;
use crate::lexer::{Token, TokenLocation};

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Bang,
    Neg,
}

impl TryFrom<TokenLocation> for UnaryOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Minus => Self::Neg,
            Token::Bang => Self::Bang,
            _ => {
                return Err(ParseError::ExpectedToken {
                    expected_tokens: vec![Token::Minus, Token::Bang],
                    actual_token: value,
                })
            }
        })
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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
            Token::Exponent => Self::Exponent,
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
                        Token::Exponent,
                        Token::In,
                        Token::Concat,
                    ],
                })
            }
        })
    }
}
