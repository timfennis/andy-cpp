use num::{BigInt, Complex};
use std::collections::VecDeque;
use std::str::Chars;

mod token;

pub use token::{Location, Token, TokenLocation};

pub struct Lexer<'a> {
    source: SourceIterator<'a>,
}

impl<'a> Lexer<'a> {
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

            // Exclude // docs
            if matches!((char, next), ('/', Some('/'))) {
                // We ran into a doc comment and we just keep consuming characters as long as we
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

            // Check for double character tokens
            if let Ok(token) = Token::try_from((char, next)) {
                self.source.next();
                return Some(Ok(TokenLocation {
                    token,
                    location: start,
                }));
            }

            // Check for single character tokens
            if let Ok(token) = Token::try_from(char) {
                return Some(Ok(TokenLocation {
                    token,
                    location: start,
                }));
            }

            match (char, next) {
                (' ' | '\t' | '\r' | '\n', _) => {
                    continue 'iterator;
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
                                match self.source.peek_next() {
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
    pub fn peek_next(&mut self) -> Option<char> {
        let next = self.inner.next()?;
        self.buffer.push_back(next);
        Some(next)
    }

    pub fn peek_one(&mut self) -> Option<char> {
        return if let Some(head) = self.buffer.front() {
            Some(*head)
        } else {
            self.peek_next()
        };
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
