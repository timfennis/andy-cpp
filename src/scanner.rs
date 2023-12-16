// lexeme == blob of characters like '=' or 'while' or ';'

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Eq, PartialEq)]
enum TokenType {
    // Single character
    LeftParentheses,
    RightParentheses,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or more
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literal
    Identifier,
    String,
    Integer,

    // Keywords
    Fn,
    If,
    Else,
    Return,
    For,
    While,
    True,
    False,
    This,

}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    kind: TokenType,
    line: usize,
    column: usize,
}


pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    column: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn from_str(source: &'a str) -> Self {
        Self { source: source.chars().peekable(), line: 1, column: 0 }
    }
}


impl Iterator for Scanner<'_> {
    type Item = Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: possibly use peekmore https://docs.rs/peekmore/latest/peekmore/ to look ahead further
        'iterator: while let Some(char) = self.source.next() {
            self.column += 1;
            let peek = self.source.peek();
            let (token_type, consume_next) = match (char, peek) {
                ('(', _) => (TokenType::LeftParentheses, false),
                (')', _) => (TokenType::RightParentheses, false),
                ('{', _) => (TokenType::LeftBrace, false),
                ('}', _) => (TokenType::RightBrace, false),
                (',', _) => (TokenType::Comma, false),
                ('-', _) => (TokenType::Minus, false),
                ('+', _) => (TokenType::Plus, false),
                (';', _) => (TokenType::Semicolon, false),
                ('*', _) => (TokenType::Star, false),
                ('!', Some('=')) => (TokenType::BangEqual, true),
                ('!', _) => (TokenType::Bang, false),
                ('=', Some('=')) => (TokenType::EqualEqual, true),
                ('=', _) => (TokenType::Equal, false),
                ('>', Some('=')) => (TokenType::GreaterEqual, true),
                ('>', _) => (TokenType::Greater, false),
                ('<', Some('=')) => (TokenType::LessEqual, true),
                ('<', _) => (TokenType::Less, false),
                ('/', Some('/')) => {
                    // We ran into a doc comment and we just keep consuming characters as long as we
                    // don't encounter a linebreak
                    while let Some(ch) = self.source.peek() {
                        if *ch == '\n' {
                            continue 'iterator;
                        }

                        self.source.next();
                    }

                    continue 'iterator;
                }
                ('/', Some(_)) => (TokenType::Slash, false),
                (' ' | '\t' | '\r', _) => {
                    continue 'iterator;
                }
                ('\n', _) => {
                    self.line += 1;
                    self.column = 0;
                    continue 'iterator;
                }
                (c, _) => {
                    // In case of an error we still push back the next character so we can continue parsing
                    return Some(Err(ScannerError::UnexpectedCharacter { char: c, line: self.line, column: self.column }));
                }
            };

            if consume_next {
                // swallow the next char
                self.source.next();
            }

            return Some(Ok(Token {
                kind: token_type,
                line: self.line,
                column: self.column,
            }));
        }

        None
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ScannerError {
    UnexpectedCharacter { char: char, line: usize, column: usize },
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScannerError::UnexpectedCharacter { char, line, column } => write!(f, "Unexpected character '{char}' at line {line} column {column}"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::scanner::{Scanner, Token};

    #[test]
    fn test_that_it_produces_no_errors() {
        let scanner = Scanner::from_str(include_str!("../tests/scanner.andy"));
        assert!(scanner.collect::<Result<Vec<Token>, _>>().is_ok());
    }
}
