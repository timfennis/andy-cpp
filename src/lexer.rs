use miette::{Diagnostic, SourceSpan};
use num::{BigInt, Complex};
use std::collections::VecDeque;
use std::str::Chars;

mod span;
mod token;

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

    fn lex_string_literal(
        &mut self,
        start_offset: usize,
        pounds: usize,
    ) -> Result<TokenLocation, Error> {
        let mut buf = String::new();
        let end_pattern = format!("{}{}", '"', "#".repeat(pounds));
        // eprintln!("end pattern: {end_pattern}");
        for char in self.source.by_ref() {
            buf.push(char);

            if buf.ends_with(&end_pattern) {
                buf.truncate(buf.len() - end_pattern.len());
                return Ok(TokenLocation {
                    token: Token::String(buf),
                    span: self.source.create_span(start_offset),
                });
            }
        }

        Err(Error::unterminated_string(
            self.source.create_span(start_offset),
        ))
    }

    fn lex_string(&mut self, start_offset: usize) -> Result<TokenLocation, Error> {
        // TODO: support \u8080 type escape sequences
        // TODO: should we handle bytes like \xFF? Probably not for strings because they aren't valid UTF-8
        let mut buf = String::new();
        #[allow(clippy::while_let_on_iterator)]
        while let Some(next_ch) = self.source.next() {
            match next_ch {
                '\\' => {
                    let escape_span = self.source.span();
                    match self.source.next() {
                        Some('n') => {
                            buf.push('\n');
                        }
                        Some('r') => {
                            buf.push('\r');
                        }
                        Some('t') => {
                            buf.push('\t');
                        }
                        Some('0') => {
                            buf.push('\0');
                        }
                        Some('\\') => {
                            buf.push('\\');
                        }
                        Some('"') => {
                            buf.push('"');
                        }
                        Some(_x) => {
                            return Err(Error::text(
                                "Invalid escapes equence".to_string(),
                                escape_span.merge(self.source.span()),
                            ));
                        }
                        None => {
                            return Err(Error::unterminated_string(
                                self.source.create_span(start_offset),
                            ))
                        }
                    }
                }
                '"' => {
                    // advance iterator and end the string
                    return Ok(TokenLocation {
                        token: Token::String(buf),
                        span: self.source.create_span(start_offset),
                    });
                }
                _ => {
                    buf.push(next_ch);
                }
            }
        }

        // THIS one is the most likely to hit
        Err(Error::unterminated_string(
            self.source.create_span(start_offset),
        ))
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
                    let mut buf = String::new();
                    buf.push(char);

                    let mut float = false;
                    while let Some(next_char) = self.source.peek_one() {
                        match next_char {
                            c if c.is_ascii_digit() => {
                                self.source.next();
                                buf.push(c);
                            }
                            '_' => {
                                // TODO: Maybe disallow `_` after `.`
                                self.source.next();
                                // ignore underscore for nice number formatting
                            }
                            '.' if !float => {
                                // if we find a dot we're likely dealing with a float, but it could
                                // also be an integer followed by a method call eg: 1.add(2)
                                // in this match we look ahead one step further to figure out if the
                                // dot is followed by a number in which case it's a float, otherwise
                                // we stop and just return the int and leave the dot for later
                                match self.source.peek_n(1) {
                                    // it's truly a num
                                    Some(n) if n.is_ascii_digit() => {
                                        float = true;
                                        self.source.next();
                                        buf.push('.');
                                    }
                                    // It's actually an int followed by dot or some weird error
                                    _ => {
                                        break;
                                    }
                                }
                            }
                            'j' | 'i' => {
                                self.source.next();

                                let Ok(num) = buf.parse::<f64>() else {
                                    return Some(Err(Error::text(
                                        format!("invalid float '{buf}'"),
                                        self.source.create_span(start_offset),
                                    )));
                                };

                                return Some(Ok(TokenLocation {
                                    token: Token::Complex(Complex::new(0.0, num)),
                                    span: self.source.create_span(start_offset),
                                }));
                            }

                            _ => break,
                        }
                    }

                    let token = if let Ok(num) = buf.parse::<i64>() {
                        Token::Int64(num)
                    } else if float {
                        if let Ok(num) = buf.parse::<f64>() {
                            Token::Float64(num)
                        } else {
                            return Some(Err(Error::text(
                                format!("invalid float '{buf}' "),
                                self.source.create_span(start_offset),
                            )));
                        }
                    } else {
                        let i = buf
                            .parse::<BigInt>()
                            .expect("must be a valid big int at this point");
                        Token::BigInt(i)
                    };
                    return Some(Ok(TokenLocation {
                        token,
                        span: self.source.create_span(start_offset),
                    }));
                }
                // Lex identifiers and keywords
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
