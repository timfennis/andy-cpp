use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Symbol {
    LeftParentheses,
    RightParentheses,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::LeftParentheses => write!(f, "("),
            Symbol::RightParentheses => write!(f, ")"),
            Symbol::LeftBracket => write!(f, "["),
            Symbol::RightBracket => write!(f, "]"),
            Symbol::LeftBrace => write!(f, "{{"),
            Symbol::RightBrace => write!(f, "}}"),
            Symbol::Semicolon => write!(f, ";"),
            Symbol::Comma => write!(f, ","),
        }
    }
}
