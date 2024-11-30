use super::{Error, Lexer, Token, TokenLocation};

pub trait StringLexer {
    /// Lexes a literal string that needs to be terminated by `pounds` number of poundsigns
    ///
    /// # Example:
    ///
    /// ```ndc
    /// r##"this is some text that "can contain double quotes" and even "#."##
    /// ```
    ///
    /// # Assumptions:
    ///  - Assumes the starting `r#"` has already been consumed from the lexer
    fn lex_string_literal(&mut self) -> Result<TokenLocation, Error>;

    /// Lexes a literal string that can contain escape sequences
    ///
    /// # Assumptions:
    ///  - Assumes the starting `"` has already been consumed from the lexer
    fn lex_string(&mut self) -> Result<TokenLocation, Error>;
}

impl<'a> StringLexer for Lexer<'a> {
    fn lex_string_literal(&mut self) -> Result<TokenLocation, Error> {
        let start_offset: usize = self.source.current_offset();
        let mut pounds: usize = 0;

        // Note: the caller should have checked that the first character is an 'r'
        assert_eq!(self.source.next(), Some('r'));

        while let Some('#') = self.source.peek() {
            self.source.next();
            pounds += 1;
        }

        let Some('"') = self.source.peek() else {
            return Err(Error::help(
                "Invalid raw string".to_string(),
                self.source.create_span(start_offset),
                "Raw string delimitation ended prematurely or contained an invalid character. Only '#' is allowed.".to_string(),
            ));
        };

        self.source.consume(1);

        let mut buf = String::new();
        let end_pattern = format!("{}{}", '"', "#".repeat(pounds));

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

    fn lex_string(&mut self) -> Result<TokenLocation, Error> {
        let start_offset = self.source.current_offset();

        // This was guaranteed by the caller, but we could make a nice error?
        assert_eq!(self.source.next(), Some('"'));

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
