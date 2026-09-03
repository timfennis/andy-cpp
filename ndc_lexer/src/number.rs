use num::{BigInt, Complex, Num};

use super::Error;
use super::{Lexer, NumericLiteral, Token, TokenLocation};

pub trait NumberLexer {
    fn lex_number(&mut self) -> Result<TokenLocation, Error>;
}

trait NumberLexerHelper {
    fn lex_to_buffer(&mut self, buf: &mut String, is_valid: impl Fn(char) -> bool);
    fn lex_integer_with_radix(
        &mut self,
        start_offset: usize,
        radix: u32,
        allow_number_suffix: bool,
    ) -> Result<TokenLocation, Error>;
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

    fn lex_integer_with_radix(
        &mut self,
        start_offset: usize,
        radix: u32,
        allow_number_suffix: bool,
    ) -> Result<TokenLocation, Error> {
        let mut buf = String::new();
        self.lex_to_buffer(&mut buf, |c| c.is_digit(radix));

        let is_number = if matches!(self.source.peek(), Some('n')) {
            if !allow_number_suffix {
                return Err(Error::text(
                    "the `n` suffix is not supported on arbitrary-radix literals".to_string(),
                    self.source.create_span(start_offset),
                ));
            }
            self.source.next();
            true
        } else {
            false
        };

        match self.source.peek() {
            Some(c) if c.is_ascii_digit() => {
                let span = self.source.span();
                self.source.next();
                return Err(Error::text(
                    format!("invalid digit for base {radix} literal"),
                    span,
                ));
            }
            Some(c) if c.is_ascii_alphabetic() => {
                let span = self.source.span();
                self.source.next();
                return Err(Error::text(
                    format!("invalid suffix for base {radix} literal"),
                    span,
                ));
            }
            _ => {}
        }

        let span = self.source.create_span(start_offset);
        let literal = if is_number {
            buf_to_number_literal_with_radix(&buf, radix)
        } else {
            buf_to_primitive_literal_with_radix(&buf, radix, span)?
        }
        .ok_or_else(|| Error::text(format!("invalid base {radix} number"), span))?;

        Ok(TokenLocation {
            token: Token::NumericLiteral(literal),
            span,
        })
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
            .expect("the existence of the first char was guaranteed by the caller");

        if first_char == '0' {
            let radix = match self.source.peek() {
                Some('b') => Some(2),
                Some('o') => Some(8),
                Some('x') => Some(16),
                _ => None,
            };
            if let Some(radix) = radix {
                self.source.next();
                return self.lex_integer_with_radix(start_offset, radix, true);
            }
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

                    return match radix {
                        2..=36 => {
                            self.lex_integer_with_radix(start_offset, u32::from(radix), false)
                        }
                        _ => Err(Error::text(
                            "invalid radix, must be between 2 and 36 OR 64".to_string(),
                            self.source.create_span(start_offset),
                        )),
                    };
                }
                'n' => {
                    self.source.next();
                    let token = if is_float {
                        buf.parse::<f64>()
                            .map(NumericLiteral::NumberFloat)
                            .map_err(|_error| {
                                Error::text(
                                    format!("invalid Number literal '{buf}n'"),
                                    self.source.create_span(start_offset),
                                )
                            })?
                    } else {
                        buf_to_number_literal_with_radix(&buf, 10).ok_or_else(|| {
                            Error::text(
                                format!("invalid Number literal '{buf}n'"),
                                self.source.create_span(start_offset),
                            )
                        })?
                    };
                    return Ok(TokenLocation {
                        token: Token::NumericLiteral(token),
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
                        token: Token::NumericLiteral(NumericLiteral::Complex(Complex::new(
                            0.0, num,
                        ))),
                        span: self.source.create_span(start_offset),
                    });
                }

                _ => break,
            }
        }

        let Some(token) =
            buf_to_primitive_literal_with_radix(&buf, 10, self.source.create_span(start_offset))?
                .or_else(|| buf.parse::<f64>().map(NumericLiteral::Float64).ok())
        else {
            // If we've lexed the int/float correctly this error should never happen, that's why it's probably safe to panic
            panic!("unable to convert buffer into Token");
        };

        Ok(TokenLocation {
            token: Token::NumericLiteral(token),
            span: self.source.create_span(start_offset),
        })
    }
}

fn buf_to_primitive_literal_with_radix(
    buf: &str,
    radix: u32,
    span: crate::Span,
) -> Result<Option<NumericLiteral>, Error> {
    if let Ok(num) = i64::from_str_radix(buf, radix) {
        return Ok(Some(NumericLiteral::Int64(num)));
    }

    let Ok(value) = BigInt::from_str_radix(buf, radix) else {
        return Ok(None);
    };

    Err(Error::text(
        format!("integer literal does not fit in Int; use the advanced literal `{value}n`"),
        span,
    ))
}

fn buf_to_number_literal_with_radix(buf: &str, radix: u32) -> Option<NumericLiteral> {
    BigInt::from_str_radix(buf, radix)
        .ok()
        .map(NumericLiteral::NumberInt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceId;

    fn lex_one(source: &str) -> Result<TokenLocation, Error> {
        Lexer::new(source, SourceId::SYNTHETIC)
            .next()
            .expect("literal should produce a lexer result")
    }

    #[test]
    fn prefixed_integer_literals_share_number_suffix_handling() {
        for (source, value) in [
            ("0b101010n", 42.into()),
            ("0o52n", 42.into()),
            ("0x2an", 42.into()),
        ] {
            let token = lex_one(source).expect("literal should be valid");

            assert_eq!(
                token.token,
                Token::NumericLiteral(NumericLiteral::NumberInt(value)),
                "unexpected token for {source}"
            );
            assert_eq!(token.span.range(), 0..source.len());
        }
    }

    #[test]
    fn radix_errors_report_the_actual_base() {
        for (source, expected) in [
            ("0b", "invalid base 2 number"),
            ("0o", "invalid base 8 number"),
            ("0x", "invalid base 16 number"),
            ("8r", "invalid base 8 number"),
        ] {
            let error = lex_one(source).expect_err("literal should be invalid");

            assert_eq!(
                error.to_string(),
                expected,
                "unexpected diagnostic for {source}"
            );
        }
    }

    #[test]
    fn prefixed_integer_literals_reject_invalid_digits_and_suffixes() {
        for (source, expected, expected_range) in [
            ("0b2", "invalid digit for base 2 literal", 2..3),
            ("0o8", "invalid digit for base 8 literal", 2..3),
            ("0xg", "invalid suffix for base 16 literal", 2..3),
            ("0b1n2", "invalid digit for base 2 literal", 4..5),
            ("0o7nq", "invalid suffix for base 8 literal", 4..5),
            ("0xffnq", "invalid suffix for base 16 literal", 5..6),
        ] {
            let error = lex_one(source).expect_err("literal should be invalid");

            assert_eq!(
                error.to_string(),
                expected,
                "unexpected diagnostic for {source}"
            );
            assert_eq!(
                error.location().range(),
                expected_range,
                "unexpected diagnostic span for {source}"
            );
        }
    }

    #[test]
    fn oversized_bare_integer_literals_are_rejected_by_the_lexer() {
        for literal in [
            "9223372036854775808",
            "0b1000000000000000000000000000000000000000000000000000000000000000",
            "0o1000000000000000000000",
            "0x8000000000000000",
            "16r8000000000000000",
        ] {
            let error = lex_one(literal).expect_err("oversized bare literal should fail lexing");

            assert_eq!(
                error.to_string(),
                "integer literal does not fit in Int; use the advanced literal `9223372036854775808n`",
                "unexpected diagnostic for {literal}"
            );
        }
    }
}
