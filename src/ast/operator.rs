use crate::interpreter::EvaluationError;
use crate::lexer::{Token, TokenType};
use std::fmt;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Operator {
    // Assignment
    CreateVar,
    EqualsSign,

    // Comparison
    Equality,
    Inequality,
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
    // Unary
    Bang,
}

impl From<Operator> for TokenType {
    fn from(value: Operator) -> Self {
        Self::Operator(value)
    }
}

impl TryFrom<&Token> for Operator {
    type Error = EvaluationError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token {
                typ: TokenType::Operator(op),
                ..
            } => Ok(*op),
            token => Err(EvaluationError::OperatorExpected { got: token.clone() }),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Operator::Equality => "==",
                Operator::Inequality => "!=",
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
                Operator::Greater => ">",
                Operator::GreaterEquals => ">=",
                Operator::Less => "<",
                Operator::LessEquals => "<=",
                Operator::Modulo => "%",
                Operator::CreateVar => ":=",
                Operator::EqualsSign => "=",
                Operator::Bang => "!",
            }
        )
    }
}
impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Operator({})", self)
    }
}
