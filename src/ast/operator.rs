use crate::ast::Error as ParseError;
use crate::ast::Error::ExpectedToken;
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
            Token::Minus => UnaryOperator::Neg,
            Token::Bang => UnaryOperator::Bang,
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
pub enum Operator {
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
            Token::LogicAnd => LogicalOperator::And,
            Token::LogicOr => LogicalOperator::Or,
            _ => {
                return Err(ExpectedToken {
                    actual_token: value,
                    expected_tokens: vec![Token::LogicAnd, Token::LogicOr],
                })
            }
        })
    }
}

impl TryFrom<TokenLocation> for Operator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Equality => Operator::Equality,
            Token::Inequality => Operator::Inequality,
            Token::Greater => Operator::Greater,
            Token::GreaterEquals => Operator::GreaterEquals,
            Token::Less => Operator::Less,
            Token::LessEquals => Operator::LessEquals,
            Token::Plus => Operator::Plus,
            Token::Minus => Operator::Minus,
            Token::Multiply => Operator::Multiply,
            Token::Divide => Operator::Divide,
            Token::CModulo => Operator::CModulo,
            Token::EuclideanModulo => Operator::EuclideanModulo,
            Token::Exponent => Operator::Exponent,
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
                    ],
                })
            }
        })
    }
}
