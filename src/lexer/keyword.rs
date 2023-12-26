use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Keyword {
    Fn,
    If,
    Else,
    Return,
    For,
    While,
    True,
    False,
    _Self,
    Null,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fn => write!(f, "fn"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Return => write!(f, "return"),
            Keyword::For => write!(f, "for"),
            Keyword::While => write!(f, "while"),
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::_Self => write!(f, "self"),
            Keyword::Null => write!(f, "null"),
        }
    }
}
