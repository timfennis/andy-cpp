use num::complex::Complex64;
use num::BigInt;
use std::fmt;

#[derive(PartialEq, Clone)]
pub enum Token {
    String(String),
    Int64(i64),
    Float64(f64),
    BigInt(BigInt),
    Complex(Complex64),

    Identifier(String),
    OpAssign(Box<TokenLocation>),
    Unit,

    // Operator - Assignment
    DeclareVar,
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
    And, // &
    Or,  // |
    // Operator - Unary
    Bang,
    // Operator - Call
    Dot,
    // Operator - Other
    DotDot, // range builder
    Concat, // ++ operator will be for concatenation
    // Keywords
    Fn,
    If,
    Else,
    Return,
    For,
    In,
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
    Colon, // : operator is used in building dictionaries and specifying type hints (in the future)
    // Lmao syntax
    MapOpen, // %{ operator stolen from elixir
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
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
            Self::Identifier(ident) => ident,
            Self::DeclareVar => ":=",
            Self::EqualsSign => "=",
            Self::Equality => "==",
            Self::Inequality => "!=",
            Self::Greater => ">",
            Self::GreaterEquals => ">=",
            Self::Less => "<",
            Self::LessEquals => "<=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::CModulo => "%",
            Self::EuclideanModulo => "%%",
            Self::Exponent => "^",
            Self::And => "&",
            Self::Or => "|",
            Self::Bang => "!",
            Self::Fn => "fn",
            Self::If => "if",
            Self::Else => "else",
            Self::Return => "return",
            Self::For => "for",
            Self::In => "in",
            Self::While => "while",
            Self::True => "true",
            Self::False => "false",
            Self::_Self => "self",
            Self::LeftParentheses => "(",
            Self::RightParentheses => ")",
            Self::LeftSquareBracket => "[",
            Self::RightSquareBracket => "]",
            Self::LeftCurlyBracket => "{",
            Self::RightCurlyBracket => "}",
            Self::Semicolon => ";",
            Self::Comma => ",",
            Self::LogicAnd => "&&",
            Self::LogicOr => "||",
            Self::Unit => "()",
            Self::DotDot => "..",
            Self::Concat => "++",
            Self::Dot => ".",
            Self::OpAssign(inner) => {
                return write!(f, "{}=", inner.token);
            }
            Token::Colon => ":",
            Token::MapOpen => "%{",
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
                | Self::Multiply
                | Self::Divide
                | Self::EuclideanModulo
                | Self::CModulo
                | Self::Identifier(_)
                | Self::Exponent
                | Self::Concat
                | Self::And
                | Self::Or
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct TokenLocation {
    pub token: Token,
    pub location: Location,
}

impl fmt::Display for TokenLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.token, self.location)
    }
}

impl fmt::Debug for TokenLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "token='{}' on {}", self.token, self.location)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {} column {}", self.line, self.column)
    }
}

impl TryFrom<(char, Option<char>)> for Token {
    type Error = ();

    fn try_from((c1, next): (char, Option<char>)) -> Result<Self, Self::Error> {
        next.map_or_else(|| c1.try_into(), |c2| (c1, c2).try_into())
    }
}

impl TryFrom<(char, char)> for Token {
    type Error = ();

    fn try_from((c1, c2): (char, char)) -> Result<Self, Self::Error> {
        match (c1, c2) {
            ('%', '{') => Ok(Self::MapOpen),
            ('+', '+') => Ok(Self::Concat),
            ('&', '&') => Ok(Self::LogicAnd),
            ('|', '|') => Ok(Self::LogicOr),
            ('%', '%') => Ok(Self::EuclideanModulo),
            (':', '=') => Ok(Self::DeclareVar),
            ('=', '=') => Ok(Self::Equality),
            ('!', '=') => Ok(Self::Inequality),
            ('>', '=') => Ok(Self::GreaterEquals),
            ('<', '=') => Ok(Self::LessEquals),
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
            '*' => Ok(Self::Multiply),
            '^' => Ok(Self::Exponent),
            '%' => Ok(Self::CModulo),
            '&' => Ok(Self::And),
            '|' => Ok(Self::Or),
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
            "while" => Self::While,
            "if" => Self::If,
            "else" => Self::Else,
            "fn" => Self::Fn,
            "for" => Self::For,
            "in" => Self::In,
            "true" => Self::True,
            "false" => Self::False,
            "return" => Self::Return,
            "self" => Self::_Self,
            _ => Self::Identifier(value),
        }
    }
}
