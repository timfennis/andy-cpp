use crate::ast::Operator;
use crate::lexer::keyword::Keyword;
use crate::lexer::Symbol;
use std::fmt;
use std::fmt::Formatter;

#[derive(Eq, PartialEq, Clone)]
pub struct StringToken {
    pub value: String,
    pub start: Position,
    pub end: Position,
}

impl fmt::Display for StringToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Eq, PartialEq, Clone)]
pub struct IdentifierToken {
    pub name: String,
    pub start: Position,
}

impl fmt::Display for IdentifierToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct OperatorToken {
    pub operator: Operator,
    pub start: Position,
}

impl fmt::Display for OperatorToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.operator)
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct NumberToken {
    pub value: i64,
    pub start: Position,
}

impl fmt::Display for NumberToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct KeywordToken {
    pub keyword: Keyword,
    pub start: Position,
}

impl fmt::Display for KeywordToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.keyword)
    }
}
#[derive(Eq, PartialEq, Copy, Clone)]
pub struct SymbolToken {
    pub symbol: Symbol,
    pub start: Position,
}

impl fmt::Display for SymbolToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol)
    }
}

#[derive(Eq, PartialEq, Clone)]
pub enum Token {
    String(StringToken),
    Identifier(IdentifierToken),
    Operator(OperatorToken),
    Number(NumberToken),
    Keyword(KeywordToken),
    Symbol(SymbolToken),
}

impl Token {
    /// Returns the position of the token. Currently only the start position of the token is returned, this should
    /// probably be a range at some point.
    /// ```
    /// # let start = Position { line: 30, column: 10 };
    /// let token = Token::String(StringToken { value: "", start, end: start });
    /// assert_eq!(token.position(), start);
    /// ```
    pub fn position(&self) -> Position {
        match self {
            Token::String(t) => t.start,
            Token::Identifier(t) => t.start,
            Token::Operator(t) => t.start,
            Token::Number(t) => t.start,
            Token::Keyword(t) => t.start,
            Token::Symbol(t) => t.start,
        }
    }
}

impl TryFrom<Token> for OperatorToken {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Operator(token) => Ok(token),
            _ => Err(()),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::String(t) => write!(f, "{}", t),
            Token::Identifier(t) => write!(f, "{}", t),
            Token::Operator(t) => write!(f, "{}", t),
            Token::Number(t) => write!(f, "{}", t),
            Token::Keyword(t) => write!(f, "{}", t),
            Token::Symbol(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "line {} column {}", self.line, self.column)
    }
}
