use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Clone)]
pub enum Token {
    String(String),
    Number(i64),
    Identifier(String),

    // Operator - Assignment
    CreateVar,
    EqualsSign,
    // Operator - Logic
    LogicAnd,
    LogicOr,
    // Operator - Comparison
    Equality,
    Inequality,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    // Operator - Math
    Plus,
    Minus,
    Multiply,
    Divide,
    CModulo,
    EuclideanModulo,
    Exponent,
    // Operator - Unary
    Bang,
    // Keywords
    Fn,
    If,
    Else,
    Return,
    For,
    While,
    True,
    False,
    _Self,
    // Symbols
    LeftParentheses,
    RightParentheses,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
    Semicolon,
    Comma,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: &str = match self {
            Token::String(str) => str,
            Token::Number(n) => {
                return write!(f, "{n}");
            }
            Token::Identifier(ident) => ident,
            Token::CreateVar => ":=",
            Token::EqualsSign => "=",
            Token::Equality => "==",
            Token::Inequality => "!=",
            Token::Greater => ">",
            Token::GreaterEquals => ">=",
            Token::Less => "<",
            Token::LessEquals => "<=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Multiply => "*",
            Token::Divide => "/",
            Token::CModulo => "%",
            Token::EuclideanModulo => "%%",
            Token::Exponent => "^",
            Token::Bang => "!",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::For => "for",
            Token::While => "while",
            Token::True => "true",
            Token::False => "false",
            Token::_Self => "self",
            Token::LeftParentheses => "(",
            Token::RightParentheses => ")",
            Token::LeftSquareBracket => "[",
            Token::RightSquareBracket => "]",
            Token::LeftCurlyBracket => "{",
            Token::RightCurlyBracket => "}",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::LogicAnd => "&&",
            Token::LogicOr => "||",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenLocation {
    pub token: Token,
    pub location: Location,
}

impl Display for TokenLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.token, self.location)
    }
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "line {} column {}", self.line, self.column)
    }
}

impl TryFrom<(char, Option<char>)> for Token {
    type Error = ();

    fn try_from((c1, next): (char, Option<char>)) -> Result<Self, Self::Error> {
        if let Some(c2) = next {
            (c1, c2).try_into()
        } else {
            c1.try_into()
        }
    }
}
impl TryFrom<(char, char)> for Token {
    type Error = ();

    fn try_from((c1, c2): (char, char)) -> Result<Self, Self::Error> {
        match (c1, c2) {
            ('&', '&') => Ok(Token::LogicAnd),
            ('|', '|') => Ok(Token::LogicOr),
            ('%', '%') => Ok(Token::EuclideanModulo),
            (':', '=') => Ok(Token::CreateVar),
            ('=', '=') => Ok(Token::Equality),
            ('!', '=') => Ok(Token::Inequality),
            ('>', '=') => Ok(Token::GreaterEquals),
            ('<', '=') => Ok(Token::LessEquals),
            _ => Err(()),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '-' => Ok(Token::Minus),
            '+' => Ok(Token::Plus),
            '*' => Ok(Token::Multiply),
            '^' => Ok(Token::Exponent),
            '%' => Ok(Token::CModulo),
            '!' => Ok(Token::Bang),
            '=' => Ok(Token::EqualsSign),
            '>' => Ok(Token::Greater),
            '<' => Ok(Token::Less),
            '/' => Ok(Token::Divide),
            '(' => Ok(Token::LeftParentheses),
            ')' => Ok(Token::RightParentheses),
            '[' => Ok(Token::LeftSquareBracket),
            ']' => Ok(Token::RightSquareBracket),
            '{' => Ok(Token::LeftCurlyBracket),
            '}' => Ok(Token::RightCurlyBracket),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),
            _ => Err(()),
        }
    }
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            "while" => Token::While,
            "if" => Token::If,
            "else" => Token::Else,
            "fn" => Token::Fn,
            "for" => Token::For,
            "true" => Token::True,
            "false" => Token::False,
            "return" => Token::Return,
            "self" => Token::_Self,
            // "null" => Token::Null,
            _ => Token::Identifier(value),
        }
    }
}
