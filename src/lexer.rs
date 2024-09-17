mod number;
mod span;
mod string;
mod token;

use miette::{Diagnostic, SourceSpan};
use number::NumberLexer;
use std::collections::VecDeque;
use std::str::Chars;
use string::StringLexer;

pub use span::Span;
pub use token::{Token, TokenLocation};

pub struct Lexer<'a> {
    source: SourceIterator<'a>,
}

impl<'a> Lexer<'a> {
    /// Takes a `TokenLocation` and tries to upgrade it's inner `Token` into a `Token::OpAssign` token.
    /// if the operator is followed by an `=` we wrap the token in an `OpAssign` token
    /// example:
    /// ```ndc
    /// foo := false
    /// foo &&= true
    /// ```
    fn lex_op_assign(
        &mut self,
        operation_token: TokenLocation,
        start_offset: usize,
    ) -> TokenLocation {
        match self.source.peek_one() {
            Some('=') if operation_token.token.is_augmentable() => {
                self.source.next();
                TokenLocation {
                    token: Token::OpAssign(Box::new(operation_token)),
                    span: self.source.create_span(start_offset),
                }
            }
            _ => operation_token,
        }
    }

    #[must_use]
    pub fn new(source: &'a str) -> Self {
        Self {
            source: SourceIterator {
                inner: source.chars(),
                buffer: VecDeque::default(),
                offset: 0,
            },
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenLocation, Error>;

    #[allow(clippy::too_many_lines)]
    fn next(&mut self) -> Option<Self::Item> {
        'iterator: while let Some(char) = self.source.next() {
            let start_offset = self.source.current_offset();
            let next = self.source.peek_one();
            let next2 = self.source.peek_n(1);

            // Exclude // docs
            if matches!((char, next), ('/', Some('/'))) {
                // We ran into a doc comment, and we just keep consuming characters as long as we
                // don't encounter a linebreak
                while let Some(ch) = self.source.peek_one() {
                    if ch == '\n' {
                        continue 'iterator;
                    }

                    // Just consume the tokens
                    self.source.next();
                }

                continue 'iterator;
            }

            if let Ok(token) = Token::try_from((char, next, next2)) {
                self.source.next();
                self.source.next();

                let operator_token = TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start_offset)));
            }
            // Check for double character tokens
            if let Ok(token) = Token::try_from((char, next)) {
                self.source.next();

                let operator_token = TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start_offset)));
            }

            // Check for single character tokens
            if let Ok(token) = Token::try_from(char) {
                let operator_token = TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start_offset)));
            }

            match (char, next) {
                (' ' | '\t' | '\r' | '\n', _) => {
                    continue 'iterator;
                }
                ('r', Some('"')) => {
                    self.source.next();
                    return Some(self.lex_string_literal(start_offset, 0));
                }
                ('r', Some('#')) => {
                    self.source.next();
                    let mut cnt = 1;
                    loop {
                        match self.source.next() {
                            Some('#') => cnt += 1,
                            Some('"') => return Some(self.lex_string_literal(start_offset, cnt)),
                            Some(char) => {
                                // TODO: include char,
                                return Some(Err(Error::text(
                                    format!("Unexpected chracter '{char}' in string literal"),
                                    self.source.span(),
                                )));
                            }
                            None => {
                                return Some(Err(Error::text(
                                    "Unexpected end of input while lexing string literal"
                                        .to_string(),
                                    self.source.create_span(start_offset),
                                )));
                            }
                        }
                    }
                }
                // lex string
                ('"', _) => return Some(self.lex_string(start_offset)),
                // lex float & int
                (char, _) if char.is_ascii_digit() => {
                    return Some(self.lex_number(char, start_offset))
                } // Lex identifiers and keywords
                (char, _) if char.is_alphabetic() || char == '_' => {
                    // Parse an identifier, or not
                    let mut buf = String::new();
                    buf.push(char);
                    while let Some(next_char) = self.source.peek_one() {
                        if next_char.is_alphanumeric() || next_char == '_' {
                            // advance iterator for next
                            buf.push(next_char);
                            self.source.next();
                        } else {
                            break;
                        }
                    }

                    let ident_end_span = self.source.span();
                    // If the identifier is followed by a single `=` we construct an OpAssign
                    if self.source.peek_one() == Some('=') && self.source.peek_n(1) != Some('=') {
                        self.source.next();
                        return Some(Ok(TokenLocation {
                            token: Token::OpAssign(Box::new(TokenLocation {
                                token: buf.into(),
                                span: self.source.create_span(start_offset).merge(ident_end_span), // TODO test is
                            })),
                            span: self.source.create_span(start_offset),
                        }));
                    }
                    return Some(Ok(TokenLocation {
                        token: buf.into(),
                        span: self.source.create_span(start_offset),
                    }));
                }
                (char, _) => {
                    return Some(Err(Error::text(
                        format!("Unexpected character '{char}'"),
                        self.source.span(),
                    )));
                }
            };
        }

        None
    }
}

struct SourceIterator<'a> {
    inner: Chars<'a>,
    buffer: VecDeque<char>,
    offset: usize,
}

impl SourceIterator<'_> {
    pub fn current_offset(&self) -> usize {
        self.offset - 1
    }

    pub fn create_span(&self, start: usize) -> Span {
        Span::new(start, (self.current_offset() + 1) - start)
    }

    pub fn span(&self) -> Span {
        Span::new(self.current_offset(), 1)
    }
}

impl<'a> Iterator for SourceIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.buffer.pop_front() {
            Some(ch) => Some(ch),
            _ => self.inner.next(),
        };

        if next.is_some() {
            self.offset += 1;
        }

        next
    }
}

impl<'a> SourceIterator<'a> {
    pub fn peek_n(&mut self, n: usize) -> Option<char> {
        while self.buffer.get(n).is_none() {
            self.buffer.push_back(self.inner.next()?);
        }

        self.buffer.get(n).copied()
    }

    pub fn peek_one(&mut self) -> Option<char> {
        self.peek_n(0)
    }
}

#[derive(Diagnostic, thiserror::Error, Debug)]
#[error("{text}")]
#[diagnostic()]
pub struct Error {
    text: String,

    label: String,

    #[label("{label}")]
    location: SourceSpan,
}

impl Error {
    #[must_use]
    pub fn text(text: String, source: Span) -> Self {
        Self {
            text,
            label: "here".to_string(),
            location: source.into(),
        }
    }

    #[must_use]
    pub fn unterminated_string(span: Span) -> Self {
        Self {
            text: "Unterminated string".to_string(),
            label: "here".to_string(),
            location: span.into(),
        }
    }
}
