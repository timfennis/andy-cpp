// lexeme == blob of characters like '=' or 'while' or ';'

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
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

#[derive(Debug)]
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
            let token_type = match (char, peek) {
                ('(', _) => TokenType::LeftParentheses,
                (')', _) => TokenType::RightParentheses,
                ('{', _) => TokenType::LeftBrace,
                ('}', _) => TokenType::RightBrace,
                (',', _) => TokenType::Comma,
                ('-', _) => TokenType::Minus,
                ('+', _) => TokenType::Plus,
                (';', _) => TokenType::Semicolon,
                ('*', _) => TokenType::Star,
                ('!', Some('=')) => TokenType::BangEqual,
                ('!', Some(_)) => TokenType::Bang,
                ('=', Some('=')) => TokenType::EqualEqual,
                ('=', Some(_)) => TokenType::Equal,
                ('>', Some('=')) => TokenType::GreaterEqual,
                ('>', Some(_)) => TokenType::Greater,
                ('<', Some('=')) => TokenType::LessEqual,
                ('<', Some(_)) => TokenType::Less,
                ('/', Some('/')) => {
                    // We ran into a doc comment and we just keep consuming characters as long as we
                    // don't encounter a linebreak
                    while let Some(ch) = self.source.peek() {
                        if *ch != '\n' {
                            self.source.next();
                        }
                    }

                    continue 'iterator;
                }
                ('/', Some(_)) => TokenType::Slash,
                (' ' | '\t' | '\r', _) => {
                    continue 'iterator;
                },
                ('\n', _) => {
                    self.line += 1;
                    self.column = 0;
                    continue 'iterator;
                },
                (c, _) => {
                    // In case of an error we still push back the next character so we can continue parsing
                    return Some(Err(ScannerError::UnexpectedCharacter(c)));
                }
            };

            return Some(Ok(Token {
                kind: token_type,
                line: self.line,
                column: self.column,
            }));
        }

        None
    }
}

#[derive(Debug)]
pub enum ScannerError {
    UnexpectedCharacter(char),
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScannerError::UnexpectedCharacter(c) => write!(f, "Unexpected character '{}'", c),
        }
    }
}
