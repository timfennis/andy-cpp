use crate::ast::Operator;
use crate::lexer::keyword::Keyword;
use crate::lexer::symbol::Symbol;
use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.typ)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenType {
    Operator(Operator),
    Symbol(Symbol),

    Identifier(String),
    // Literal
    String(String),
    Integer(i64),

    // Keywords
    Keyword(Keyword),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Operator(op) => write!(f, "{}", op),
            TokenType::Symbol(s) => write!(f, "{}", s),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::String(s) => write!(f, "\"{}\"", s),
            TokenType::Integer(i) => write!(f, "{}", i),
            TokenType::Keyword(k) => write!(f, "{}", k),
        }
    }
}

impl From<Symbol> for TokenType {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

impl From<Keyword> for TokenType {
    fn from(value: Keyword) -> Self {
        TokenType::Keyword(value)
    }
}
