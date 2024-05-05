use num::{BigInt, Complex};
use std::collections::VecDeque;
use std::str::Chars;

mod token;

pub use token::{Location, Token, TokenLocation};

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
    fn lex_op_assign(&mut self, operation_token: TokenLocation, start: Location) -> TokenLocation {
        match self.source.peek_one() {
            Some('=') if operation_token.token.is_augmentable() => {
                self.source.next();
                TokenLocation {
                    token: Token::OpAssign(Box::new(operation_token)),
                    location: start,
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
                line: 1,
                column: 0,
            },
        }
    }

    fn lex_string_literal(
        &mut self,
        start: Location,
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
                    location: start,
                });
            }
        }

        Err(Error::UnterminatedString { location: start })
    }
    fn lex_string(&mut self, start: Location) -> Result<TokenLocation, Error> {
        // TODO: support \u8080 type escape sequences
        // TODO: should we handle bytes like \xFF? Probably not for strings because they aren't valid UTF-8
        let mut buf = String::new();
        #[allow(clippy::while_let_on_iterator)]
        while let Some(next_ch) = self.source.next() {
            match next_ch {
                '\\' => match self.source.next() {
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
                    Some(x) => {
                        return Err(Error::InvalidEscapeSequence {
                            sequence: format!("\\{x}"),
                            location: self.source.location(),
                        });
                    }
                    None => {
                        return Err(Error::UnterminatedString { location: start });
                    }
                },
                '"' => {
                    // advance iterator and end the string
                    return Ok(TokenLocation {
                        token: Token::String(buf),
                        location: start,
                    });
                }
                _ => {
                    buf.push(next_ch);
                }
            }
        }

        Err(Error::UnterminatedString { location: start })
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenLocation, Error>;

    #[allow(clippy::too_many_lines)]
    fn next(&mut self) -> Option<Self::Item> {
        'iterator: while let Some(char) = self.source.next() {
            let start = Location {
                column: self.source.column,
                line: self.source.line,
            };

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
                let operator_token = TokenLocation {
                    token,
                    location: self.source.location(),
                };

                self.source.next();
                self.source.next();

                return Some(Ok(self.lex_op_assign(operator_token, start)));
            }
            // Check for double character tokens
            if let Ok(token) = Token::try_from((char, next)) {
                let operator_token = TokenLocation {
                    token,
                    location: self.source.location(),
                };

                self.source.next();

                return Some(Ok(self.lex_op_assign(operator_token, start)));
            }

            // Check for single character tokens
            if let Ok(token) = Token::try_from(char) {
                let operator_token = TokenLocation {
                    token,
                    location: self.source.location(),
                };

                return Some(Ok(self.lex_op_assign(operator_token, start)));
            }

            match (char, next) {
                (' ' | '\t' | '\r' | '\n', _) => {
                    continue 'iterator;
                }
                ('r', Some('"')) => {
                    self.source.next();
                    return Some(self.lex_string_literal(start, 0));
                }
                ('r', Some('#')) => {
                    self.source.next();
                    let mut cnt = 1;
                    loop {
                        match self.source.next() {
                            Some('#') => cnt += 1,
                            Some('"') => return Some(self.lex_string_literal(start, cnt)),
                            Some(char) => {
                                return Some(Err(Error::UnexpectedCharacter {
                                    char,
                                    location: self.source.location(),
                                }))
                            }
                            None => {
                                return Some(Err(Error::UnterminatedString { location: start }))
                            }
                        }
                    }
                }
                ('"', _) => return Some(self.lex_string(start)),
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
                                    return Some(Err(Error::InvalidFloat {
                                        string: buf,
                                        location: start,
                                    }));
                                };

                                return Some(Ok(TokenLocation {
                                    token: Token::Complex(Complex::new(0.0, num)),
                                    location: start,
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
                            return Some(Err(Error::InvalidFloat {
                                string: buf,
                                location: start,
                            }));
                        }
                    } else {
                        let i = buf
                            .parse::<BigInt>()
                            .expect("must be a valid big int at this point");
                        Token::BigInt(i)
                    };
                    return Some(Ok(TokenLocation {
                        token,
                        location: start,
                    }));
                }
                //TODO: For now we just support boring c-style identifiers but maybe allowing Emoji or other characters
                //      could be cool too
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

                    // If the identifier is followed by a single `=` we construct an OpAssign
                    if self.source.peek_one() == Some('=') && self.source.peek_n(1) != Some('=') {
                        self.source.next();
                        return Some(Ok(TokenLocation {
                            token: Token::OpAssign(Box::new(TokenLocation {
                                token: buf.into(),
                                location: start,
                            })),
                            location: start,
                        }));
                    }
                    return Some(Ok(TokenLocation {
                        token: buf.into(),
                        location: start,
                    }));
                }
                (char, _) => {
                    return Some(Err(Error::UnexpectedCharacter {
                        char,
                        location: self.source.location(),
                    }));
                }
            };
        }

        None
    }
}

struct SourceIterator<'a> {
    inner: Chars<'a>,
    buffer: VecDeque<char>,
    line: usize,
    column: usize,
}

impl SourceIterator<'_> {
    pub fn location(&self) -> Location {
        Location {
            column: self.column,
            line: self.line,
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

        match next {
            Some('\n') => {
                self.line += 1;
                self.column = 0;
            }
            Some(_) => {
                self.column += 1;
            }
            None => {}
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

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error("invalid float '{string}' at {location}")]
    InvalidFloat { string: String, location: Location },
    #[error("unexpected character '{char}' at {location}")]
    UnexpectedCharacter { char: char, location: Location },
    #[error("unterminated string starting at {location}")]
    UnterminatedString { location: Location },
    #[error("invalid escape sequence '{sequence}' at {location}")]
    InvalidEscapeSequence {
        sequence: String,
        location: Location,
    },
}
