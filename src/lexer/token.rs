use num::complex::Complex64;
use num::BigInt;
use std::fmt;

use super::Span;

#[derive(PartialEq, Clone)]
pub enum Token {
    String(String),
    Int64(i64),
    Float64(f64),
    BigInt(BigInt),
    Complex(Complex64),
    Infinity,

    Identifier(String),
    OpAssign(Box<TokenLocation>),

    // Operator - Assignment
    // DeclareVar,
    EqualsSign, // =
    // Operator - Logic
    LogicAnd, // and
    LogicOr,  // or
    LogicNot, // not
    // Operator - Comparison
    EqualsEquals,  // ==
    BangEquals,    // !=
    Greater,       // >
    GreaterEquals, // >=
    Less,          // <
    LessEquals,    // <=
    // Operator - Math
    Plus,           // +
    Minus,          // -
    Asterix,        // *
    Divide,         // /
    Percent,        // %
    PercentPercent, // %%
    Caret,          // ^
    Ampersand,      // &
    Pipe,           // |
    Tilde,          // ~
    LessLess,       // <<
    GreaterGreater, // >>
    // Operator - Other
    Bang,         // !
    Dot,          // .
    DotDot,       // range builder
    DotDotEquals, // inclusive range builder
    PlusPlus,     // ++ operator will be for concatenation
    Diamond,      // <> operator will be for string concatenation with coersion
    RightArrow,   // ->
    // Keywords
    Let,
    Fn,
    If,
    Else,
    Return,
    Break,
    For,
    In,
    While,
    True,
    False,
    Pure,
    // Symbols
    LeftParentheses,    // (
    RightParentheses,   // )
    LeftSquareBracket,  // [
    RightSquareBracket, // ]
    LeftCurlyBracket,   // {
    RightCurlyBracket,  // }
    Semicolon,          // ;
    Comma,              // ,
    Colon,              // :
    // Lmao syntax
    MapOpen, // %{ operator stolen from elixir
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, "\"{string}\""),
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: &str = match self {
            Self::String(str) => str,
            Self::Int64(n) => {
                return write!(f, "{n}");
            }
            Self::Float64(n) => {
                let mut buffer = ryu::Buffer::new();
                return write!(f, "{}", buffer.format(*n));
            }
            Self::BigInt(n) => {
                return write!(f, "{n}");
            }
            Self::Complex(n) => {
                return write!(f, "{n}");
            }
            Self::Infinity => "Inf",
            Self::Identifier(ident) => ident,
            // Self::DeclareVar => ":=",
            Self::EqualsSign => "=",
            Self::EqualsEquals => "==",
            Self::BangEquals => "!=",
            Self::Greater => ">",
            Self::GreaterEquals => ">=",
            Self::Less => "<",
            Self::LessEquals => "<=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Asterix => "*",
            Self::Divide => "/",
            Self::Percent => "%",
            Self::PercentPercent => "%%",
            Self::Caret => "^",
            Self::Ampersand => "&",
            Self::Pipe => "|",
            Self::Tilde => "~",
            Self::LessLess => "<<",
            Self::GreaterGreater => ">>",
            Self::Bang => "!",
            Self::Let => "let",
            Self::Fn => "fn",
            Self::If => "if",
            Self::Else => "else",
            Self::Return => "return",
            Self::Break => "break",
            Self::For => "for",
            Self::In => "in",
            Self::While => "while",
            Self::True => "true",
            Self::False => "false",
            Self::Pure => "pure",
            Self::LeftParentheses => "(",
            Self::RightParentheses => ")",
            Self::LeftSquareBracket => "[",
            Self::RightSquareBracket => "]",
            Self::LeftCurlyBracket => "{",
            Self::RightCurlyBracket => "}",
            Self::Semicolon => ";",
            Self::Comma => ",",
            Self::LogicNot => "not",
            Self::LogicAnd => "and",
            Self::LogicOr => "or",
            Self::DotDot => "..",
            Self::DotDotEquals => "..=",
            Self::PlusPlus => "++",
            Self::Diamond => "<>",
            Self::Dot => ".",
            Self::OpAssign(inner) => {
                return write!(f, "{}=", inner.token);
            }
            Token::Colon => ":",
            Token::MapOpen => "%{",
            Token::RightArrow => "->",
        };
        write!(f, "{s}")
    }
}

impl Token {
    /// Returns true if this token can be augmented into an `OpAssign` token
    #[must_use]
    pub fn is_augmentable(&self) -> bool {
        matches!(
            self,
            Self::Plus
                | Self::Minus
                | Self::Asterix
                | Self::Divide
                | Self::PercentPercent
                | Self::Percent
                | Self::Identifier(_)
                | Self::Caret
                | Self::PlusPlus
                | Self::Ampersand
                | Self::Pipe
                | Self::Diamond
        )
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TokenLocation {
    pub token: Token,
    pub span: Span,
}

impl TryFrom<(char, Option<char>, Option<char>)> for Token {
    type Error = ();

    fn try_from((c1, c2, c3): (char, Option<char>, Option<char>)) -> Result<Self, Self::Error> {
        let options = || Some((c1, c2?, c3?));
        if let Some(three_tup) = options() {
            Token::try_from(three_tup)
        } else {
            Err(())
        }
    }
}
impl TryFrom<(char, char, char)> for Token {
    type Error = ();

    fn try_from((c1, c2, c3): (char, char, char)) -> Result<Self, Self::Error> {
        match (c1, c2, c3) {
            ('.', '.', '=') => Ok(Self::DotDotEquals),
            _ => Err(()),
        }
    }
}
impl TryFrom<(char, Option<char>)> for Token {
    type Error = ();

    fn try_from((c1, next): (char, Option<char>)) -> Result<Self, Self::Error> {
        if let Some(c2) = next {
            (c1, c2).try_into()
        } else {
            Err(())
        }
    }
}

impl TryFrom<(char, char)> for Token {
    type Error = ();

    fn try_from((c1, c2): (char, char)) -> Result<Self, Self::Error> {
        match (c1, c2) {
            ('.', '.') => Ok(Self::DotDot),
            ('%', '{') => Ok(Self::MapOpen),
            ('+', '+') => Ok(Self::PlusPlus),
            ('%', '%') => Ok(Self::PercentPercent),
            ('=', '=') => Ok(Self::EqualsEquals),
            ('!', '=') => Ok(Self::BangEquals),
            ('>', '=') => Ok(Self::GreaterEquals),
            ('<', '=') => Ok(Self::LessEquals),
            ('<', '>') => Ok(Self::Diamond),
            ('-', '>') => Ok(Self::RightArrow),
            ('<', '<') => Ok(Self::LessLess),
            ('>', '>') => Ok(Self::GreaterGreater),
            _ => Err(()),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '-' => Ok(Self::Minus),
            '+' => Ok(Self::Plus),
            '*' => Ok(Self::Asterix),
            '^' => Ok(Self::Caret),
            '%' => Ok(Self::Percent),
            '&' => Ok(Self::Ampersand),
            '~' => Ok(Self::Tilde),
            '|' => Ok(Self::Pipe),
            '!' => Ok(Self::Bang),
            '=' => Ok(Self::EqualsSign),
            '>' => Ok(Self::Greater),
            '<' => Ok(Self::Less),
            '/' => Ok(Self::Divide),
            '(' => Ok(Self::LeftParentheses),
            ')' => Ok(Self::RightParentheses),
            '[' => Ok(Self::LeftSquareBracket),
            ']' => Ok(Self::RightSquareBracket),
            '{' => Ok(Self::LeftCurlyBracket),
            '}' => Ok(Self::RightCurlyBracket),
            ',' => Ok(Self::Comma),
            ';' => Ok(Self::Semicolon),
            ':' => Ok(Self::Colon),
            '.' => Ok(Self::Dot),
            _ => Err(()),
        }
    }
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            // YOLO for now just have Inf and NaN here
            "Inf" => Self::Float64(f64::INFINITY),
            "NaN" => Self::Float64(f64::NAN),
            // Normal keywords
            "let" => Self::Let,
            "and" => Self::LogicAnd,
            "or" => Self::LogicOr,
            "not" => Self::LogicNot,
            "while" => Self::While,
            "if" => Self::If,
            "else" => Self::Else,
            "fn" => Self::Fn,
            "for" => Self::For,
            "in" => Self::In,
            "true" => Self::True,
            "false" => Self::False,
            "return" => Self::Return,
            "break" => Self::Break,
            "pure" => Self::Pure,
            _ => Self::Identifier(value),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Span;

    #[test]
    fn test_merge() {
        let a = Span::new(0, 1);
        let b = Span::new(3, 1);
        assert_eq!(Span::new(0, 4), a.merge(b));

        let a = Span::new(3, 100);
        let b = Span::new(5, 10);
        assert_eq!(Span::new(3, 100), a.merge(b));
        assert_eq!(Span::new(3, 100), a.merge(a));
    }
}
