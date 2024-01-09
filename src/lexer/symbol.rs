use std::fmt::{Display, Formatter};
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Symbol {
    LeftParentheses,
    RightParentheses,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
    Semicolon,
    Comma,
}

impl TryFrom<char> for Symbol {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '(' => Ok(Symbol::LeftParentheses),
            ')' => Ok(Symbol::RightParentheses),
            '{' => Ok(Symbol::LeftCurlyBracket),
            '}' => Ok(Symbol::RightCurlyBracket),
            '[' => Ok(Symbol::LeftSquareBracket),
            ']' => Ok(Symbol::RightSquareBracket),
            ',' => Ok(Symbol::Comma),
            ';' => Ok(Symbol::Semicolon),
            _ => Err(()),
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::LeftParentheses => write!(f, "("),
            Symbol::RightParentheses => write!(f, ")"),
            Symbol::LeftSquareBracket => write!(f, "["),
            Symbol::RightSquareBracket => write!(f, "]"),
            Symbol::LeftCurlyBracket => write!(f, "{{"),
            Symbol::RightCurlyBracket => write!(f, "}}"),
            Symbol::Semicolon => write!(f, ";"),
            Symbol::Comma => write!(f, ","),
        }
    }
}
