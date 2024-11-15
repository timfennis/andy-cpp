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
        match self.source.peek() {
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
        'iterator: while let Some(first) = self.source.peek() {
            let start_offset = self.source.current_offset();
            let second = self.source.peek_n(1);
            let third = self.source.peek_n(2);

            // Exclude // docs
            if matches!((first, second), ('/', Some('/'))) {
                self.source.consume(2);
                // We ran into a doc comment, and we just keep consuming characters as long as we
                // don't encounter a linebreak
                while let Some(ch) = self.source.peek() {
                    if ch == '\n' {
                        // NOTE: can we consume this '\n' now that we don't count line breaks anymore
                        continue 'iterator;
                    }

                    // Just consume the tokens
                    self.source.next();
                }

                continue 'iterator;
            }

            if let Ok(token) = Token::try_from((first, second, third)) {
                self.source.consume(3);

                let operator_token = TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start_offset)));
            }
            // Check for double character tokens
            if let Ok(token) = Token::try_from((first, second)) {
                self.source.consume(2);

                let operator_token = TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start_offset)));
            }

            // Check for single character tokens
            if let Ok(token) = Token::try_from(first) {
                self.source.consume(1);

                let operator_token = TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start_offset)));
            }

            match (first, second) {
                (' ' | '\t' | '\r' | '\n', _) => {
                    self.source.consume(1);
                    continue 'iterator;
                }
                ('r', Some('"' | '#')) => {
                    return Some(self.lex_string_literal());
                }
                // lex string
                ('"', _) => return Some(self.lex_string()),
                // lex float & int
                (char, _) if char.is_ascii_digit() => return Some(self.lex_number()), // Lex identifiers and keywords
                // Identifier or keyword
                (char, _) if char.is_alphabetic() || char == '_' => {
                    // Because we use the peeked char we consume it from the iterator
                    self.source.consume(1);

                    let mut buf = String::new();
                    buf.push(char);

                    while let Some(next_char) = self.source.peek() {
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
                    if self.source.peek() == Some('=') && self.source.peek_n(1) != Some('=') {
                        self.source.next();
                        return Some(Ok(TokenLocation {
                            token: Token::OpAssign(Box::new(TokenLocation {
                                token: buf.into(),
                                span: self.source.create_span(start_offset).merge(ident_end_span), // TODO test is
                            })),
                            span: self.source.create_span(start_offset),
                        }));
                    }

                    // `buf.into()` takes care of creating a Keyword or Identifier depending on the string content
                    return Some(Ok(TokenLocation {
                        token: buf.into(),
                        span: self.source.create_span(start_offset),
                    }));
                }
                (char, _) => {
                    self.source.consume(1);
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
        self.offset
    }

    pub fn create_span(&self, start: usize) -> Span {
        Span::new(start, (self.current_offset()) - start)
    }

    pub fn span(&self) -> Span {
        Span::new(self.current_offset(), 1)
    }

    pub fn consume(&mut self, count: usize) {
        for _ in 0..count {
            // TODO: this is an internal error how should we handle these?
            self.next()
                .expect("tried to consume but iterator was empty");
        }
    }
}

impl<'a> Iterator for SourceIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.buffer.pop_front() {
            Some(ch) => Some(ch),
            _ => self.inner.next(),
        };

        if let Some(c) = next {
            self.offset += c.len_utf8();
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

    pub fn peek(&mut self) -> Option<char> {
        self.peek_n(0)
    }
}

#[derive(Diagnostic, thiserror::Error, Debug)]
#[error("{text}")]
#[diagnostic()]
pub struct Error {
    text: String,

    #[label("here")]
    location: SourceSpan,

    #[help]
    help_text: Option<String>,
}

impl Error {
    #[must_use]
    pub fn text(text: String, source: Span) -> Self {
        Self {
            text,
            location: source.into(),
            help_text: None,
        }
    }

    #[must_use]
    pub fn unterminated_string(span: Span) -> Self {
        Self {
            text: "Unterminated string".to_string(),
            location: span.into(),
            help_text: None,
        }
    }

    #[must_use]
    pub fn help(text: String, source: Span, help_text: String) -> Self {
        Self {
            text,
            location: source.into(),
            help_text: Some(help_text),
        }
    }
}
