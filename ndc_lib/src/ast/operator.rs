use crate::ast::Error as ParseError;
use crate::lexer::{Token, TokenLocation};
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Not,
    Neg,
    BitNot,
}

impl TryFrom<TokenLocation> for UnaryOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Tilde => Self::BitNot,
            Token::Minus => Self::Neg,
            Token::Bang | Token::LogicNot => Self::Not,
            _ => {
                // This is essentially an internal error since the parser should check the token before trying to convert it into a UnaryOperator
                // maybe in the future it would be better to use From and panic!
                return Err(ParseError::text(
                    format!("Expected '-' or '!' but got {} instead", value.token),
                    value.span,
                ));
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
    Spaceship,
    InverseSpaceship,
    Plus,
    Minus,
    Multiply,
    Divide,
    FloorDivide,
    CModulo,
    EuclideanModulo,
    Exponent,
    And,
    Or,
    Xor,
    Concat,
    StringConcat,
    In,
    ShiftRight,
    ShiftLeft,
}

impl BinaryOperator {
    pub fn supports_vectorization(&self) -> bool {
        matches!(
            self,
            Self::Plus
                | Self::Minus
                | Self::Multiply
                | Self::Divide
                | Self::FloorDivide
                | Self::CModulo
                | Self::EuclideanModulo
                | Self::Exponent
        )
    }
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
                // This is more of an internal parser error than a parser error caused by the user
                return Err(ParseError::text(
                    format!(
                        "Expected either 'and' or 'or' but got {} instead",
                        value.token
                    ),
                    value.span,
                ));
            }
        })
    }
}

impl TryFrom<TokenLocation> for BinaryOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::EqualsEquals => Self::Equality,
            Token::BangEquals => Self::Inequality,
            Token::Greater => Self::Greater,
            Token::GreaterEquals => Self::GreaterEquals,
            Token::Less => Self::Less,
            Token::LessEquals => Self::LessEquals,
            Token::Spaceship => Self::Spaceship,
            Token::InverseSpaceship => Self::InverseSpaceship,
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Asterix => Self::Multiply,
            Token::ForwardSlash => Self::Divide,
            Token::Backslash => Self::FloorDivide,
            Token::Percent => Self::CModulo,
            Token::PercentPercent => Self::EuclideanModulo,
            Token::Caret => Self::Exponent,
            Token::Ampersand => Self::And,
            Token::Tilde => Self::Xor,
            Token::Pipe => Self::Or,
            Token::In => Self::In,
            Token::PlusPlus => Self::Concat,
            Token::Diamond => Self::StringConcat,
            Token::GreaterGreater => Self::ShiftRight,
            Token::LessLess => Self::ShiftLeft,
            _ => {
                // NOTE: this is more of an internal error than a user caused error since the parser should check the token prior to converting it.
                return Err(ParseError::text(
                    format!(
                        "Expected a valid binary operator but got {} instead",
                        value.token
                    ),
                    value.span,
                ));
            }
        })
    }
}

impl fmt::Debug for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

// TODO: This actually makes no sense, we should convert it back into a token and then print that?
impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Equality => "==",
            Self::Inequality => "!=",
            Self::Greater => ">",
            Self::GreaterEquals => ">=",
            Self::Less => "<",
            Self::LessEquals => "<=",
            Self::Spaceship => "<=>",
            Self::InverseSpaceship => ">=<",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::FloorDivide => "\\",
            Self::CModulo => "%",
            Self::EuclideanModulo => "%%",
            Self::Exponent => "^",
            Self::And => "&",
            Self::Or => "|",
            Self::Xor => "~",
            Self::Concat => "++",
            Self::StringConcat => "<>",
            Self::In => "in",
            Self::ShiftRight => ">>",
            Self::ShiftLeft => "<<",
        };
        write!(f, "{op}")
    }
}
