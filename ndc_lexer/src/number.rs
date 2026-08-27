use num::{BigInt, Complex, Num};

use super::Error;
use super::{Lexer, Token, TokenLocation};

pub trait NumberLexer {
    fn lex_number(&mut self) -> Result<TokenLocation, Error>;
}

trait NumberLexerHelper {
    fn lex_to_buffer(&mut self, buf: &mut String, is_valid: impl Fn(char) -> bool);
}

impl NumberLexerHelper for Lexer<'_> {
    fn lex_to_buffer(&mut self, buf: &mut String, is_valid: impl Fn(char) -> bool) {
        while let Some(next_char) = self.source.peek() {
            match next_char {
                c if is_valid(c) => {
                    self.source.next();
                    buf.push(next_char);
                }
                '_' => {
                    self.source.next();
                }
                _ => break,
            }
        }
    }
}

impl NumberLexer for Lexer<'_> {
    #[allow(clippy::too_many_lines)]
    fn lex_number(&mut self) -> Result<TokenLocation, Error> {
        let mut buf = String::new();

        let start_offset = self.source.current_offset();
        let first_char = self
            .source
            .next()
            .expect("the existance of the first char was guaranteed by the caller");

        // If the string starts with `0b` it must be a binary literal
        if first_char == '0' && matches!(self.source.peek(), Some('b')) {
            self.source.next(); // eat the 'b'

            self.lex_to_buffer(&mut buf, |c| c == '1' || c == '0');

            let is_number = if matches!(self.source.peek(), Some('n')) {
                self.source.next();
                true
            } else {
                false
            };

            match self.source.peek() {
                Some(c) if c.is_ascii_digit() => {
                    self.source.next();
                    return Err(Error::text(
                        "invalid digit for base 2 literal".to_string(),
                        self.source.span(),
                    ));
                }
                Some(c) if c.is_ascii_alphabetic() => {
                    self.source.next();
                    return Err(Error::text(
                        "invalid suffix for base 2 literal".to_string(),
                        self.source.span(),
                    ));
                }
                _ => {}
            }

            let token = if is_number {
                buf_to_number_token_with_radix(&buf, 2)
            } else {
                buf_to_primitive_token_with_radix(&buf, 2, self.source.create_span(start_offset))?
            };
            return match token {
                Some(token) => Ok(TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                }),
                None => Err(Error::text(
                    "invalid base 2 number".to_string(),
                    self.source.create_span(start_offset),
                )),
            };
        }

        if first_char == '0' && matches!(self.source.peek(), Some('x')) {
            self.source.next();

            self.lex_to_buffer(&mut buf, |c| c.is_ascii_hexdigit());

            let is_number = if matches!(self.source.peek(), Some('n')) {
                self.source.next();
                true
            } else {
                false
            };

            let token = if is_number {
                buf_to_number_token_with_radix(&buf, 16)
            } else {
                buf_to_primitive_token_with_radix(&buf, 16, self.source.create_span(start_offset))?
            };
            return match token {
                Some(token) => Ok(TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                }),
                None => Err(Error::text(
                    "invalid base 16 number".to_string(),
                    self.source.create_span(start_offset),
                )),
            };
        }

        if first_char == '0' && matches!(self.source.peek(), Some('o')) {
            self.source.next();

            self.lex_to_buffer(&mut buf, |c| matches!(c, '0'..='7'));

            let is_number = if matches!(self.source.peek(), Some('n')) {
                self.source.next();
                true
            } else {
                false
            };

            let token = if is_number {
                buf_to_number_token_with_radix(&buf, 8)
            } else {
                buf_to_primitive_token_with_radix(&buf, 8, self.source.create_span(start_offset))?
            };
            return match token {
                Some(token) => Ok(TokenLocation {
                    token,
                    span: self.source.create_span(start_offset),
                }),
                None => Err(Error::text(
                    "invalid base 16 number".to_string(),
                    self.source.create_span(start_offset),
                )),
            };
        }

        // The first digit of the literal is not part of a radix but it's part of the number
        buf.push(first_char);

        let mut is_float = false;
        while let Some(next_char) = self.source.peek() {
            match next_char {
                c if c.is_ascii_digit() => {
                    self.source.next();
                    buf.push(c);
                }
                // A `_` inside a number is ignored unless it's after a `.`
                '_' => {
                    self.source.next();
                    // ignore underscore for nice number formatting
                }
                '.' if !is_float => {
                    // if we find a dot we're likely dealing with a float, but it could
                    // also be an integer followed by a method call eg: 1.add(2)
                    // in this match we look ahead one step further to figure out if the
                    // dot is followed by a number in which case it's a float, otherwise
                    // we stop and just return the int and leave the dot for later
                    match self.source.peek_n(1) {
                        // it's truly a num
                        Some(n) if n.is_ascii_digit() => {
                            is_float = true;
                            self.source.next();
                            buf.push('.');
                        }
                        // It's actually an int followed by dot or some weird error
                        _ => {
                            break;
                        }
                    }
                }
                // RADIX FUN!
                'r' if !is_float => {
                    self.source.next(); // eat the 'r'
                    let Ok(radix) = buf.parse::<u8>() else {
                        return Err(Error::text(
                            "that's not a radix".to_string(),
                            self.source.create_span(start_offset),
                        ));
                    };

                    match radix {
                        2..=36 => {
                            let mut buf = String::new();
                            self.lex_to_buffer(&mut buf, validator_for_radix(usize::from(radix)));

                            if matches!(self.source.peek(), Some('n')) {
                                return Err(Error::text(
                                    "the `n` suffix is not supported on arbitrary-radix literals"
                                        .to_string(),
                                    self.source.create_span(start_offset),
                                ));
                            }

                            return match buf_to_primitive_token_with_radix(
                                &buf,
                                u32::from(radix),
                                self.source.create_span(start_offset),
                            )? {
                                Some(token) => Ok(TokenLocation {
                                    token,
                                    span: self.source.create_span(start_offset),
                                }),
                                None => Err(Error::text(
                                    "invalid base 16 number".to_string(),
                                    self.source.create_span(start_offset),
                                )),
                            };
                        }
                        _ => {
                            return Err(Error::text(
                                "invalid radix, must be between 2 and 36 OR 64".to_string(),
                                self.source.create_span(start_offset),
                            ));
                        }
                    }
                }
                'n' => {
                    self.source.next();
                    let token = if is_float {
                        buf.parse::<f64>()
                            .map(Token::NumberFloat)
                            .map_err(|_error| {
                                Error::text(
                                    format!("invalid Number literal '{buf}n'"),
                                    self.source.create_span(start_offset),
                                )
                            })?
                    } else {
                        buf_to_number_token_with_radix(&buf, 10).ok_or_else(|| {
                            Error::text(
                                format!("invalid Number literal '{buf}n'"),
                                self.source.create_span(start_offset),
                            )
                        })?
                    };
                    return Ok(TokenLocation {
                        token,
                        span: self.source.create_span(start_offset),
                    });
                }
                'j' | 'i' => {
                    self.source.next();

                    let Ok(num) = buf.parse::<f64>() else {
                        return Err(Error::text(
                            format!("invalid float '{buf}'"),
                            self.source.create_span(start_offset),
                        ));
                    };

                    return Ok(TokenLocation {
                        token: Token::Complex(Complex::new(0.0, num)),
                        span: self.source.create_span(start_offset),
                    });
                }

                _ => break,
            }
        }

        let Some(token) =
            buf_to_primitive_token_with_radix(&buf, 10, self.source.create_span(start_offset))?
                .or_else(|| buf.parse::<f64>().map(Token::Float64).ok())
        else {
            // If we've lexed the int/float correctly this error should never happen, that's why it's probably safe to panic
            panic!("unable to convert buffer into Token");
        };

        Ok(TokenLocation {
            token,
            span: self.source.create_span(start_offset),
        })
    }
}

fn buf_to_primitive_token_with_radix(
    buf: &str,
    radix: u32,
    span: crate::Span,
) -> Result<Option<Token>, Error> {
    if let Ok(num) = i64::from_str_radix(buf, radix) {
        return Ok(Some(Token::Int64(num)));
    }

    let Ok(value) = BigInt::from_str_radix(buf, radix) else {
        return Ok(None);
    };

    Err(Error::text(
        format!("integer literal does not fit in Int; use the advanced literal `{value}n`"),
        span,
    ))
}

fn buf_to_number_token_with_radix(buf: &str, radix: u32) -> Option<Token> {
    BigInt::from_str_radix(buf, radix)
        .ok()
        .map(Token::NumberInt)
}

fn validator_for_radix(radix: usize) -> impl Fn(char) -> bool {
    move |c| "0123456789abcdefghijlkmnopqrstuvwxyz"[0..radix].contains(c.to_ascii_lowercase())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceId;

    #[test]
    fn oversized_bare_integer_literals_are_rejected_by_the_lexer() {
        for literal in [
            "9223372036854775808",
            "0b1000000000000000000000000000000000000000000000000000000000000000",
            "0o1000000000000000000000",
            "0x8000000000000000",
            "16r8000000000000000",
        ] {
            let error = Lexer::new(literal, SourceId::SYNTHETIC)
                .next()
                .expect("literal should produce a lexer result")
                .expect_err("oversized bare literal should fail lexing");

            assert_eq!(
                error.to_string(),
                "integer literal does not fit in Int; use the advanced literal `9223372036854775808n`",
                "unexpected diagnostic for {literal}"
            );
        }
    }
}
