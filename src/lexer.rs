use crate::ast::Operator;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::str::Chars;

mod keyword;
mod symbol;
#[allow(clippy::module_name_repetitions)]
mod token;

pub use keyword::Keyword;
pub use symbol::Symbol;
pub use token::{
    IdentifierToken, KeywordToken, NumberToken, OperatorToken, Position, StringToken, SymbolToken,
    Token,
};

pub struct Lexer<'a> {
    source: SourceIterator<'a>,
}

impl<'a> Lexer<'a> {
    pub fn from_str(source: &'a str) -> Self {
        Self {
            source: SourceIterator {
                inner: source.chars(),
                buffer: VecDeque::default(),
                line: 1,
                column: 0,
            },
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, Error>;

    #[allow(clippy::too_many_lines)]
    fn next(&mut self) -> Option<Self::Item> {
        'iterator: while let Some(char) = self.source.next() {
            let start = Position {
                column: self.source.column,
                line: self.source.line,
            };
            let next = self.source.peek_one();

            // TODO check for comments starting with //

            // Check for double character operators
            if let Ok(operator) = Operator::try_from((char, next)) {
                self.source.next();
                return Some(Ok(Token::Operator(OperatorToken { operator, start })));
            }
            // Check for single character symbols
            if let Ok(symbol) = Symbol::try_from(char) {
                return Some(Ok(Token::Symbol(SymbolToken { symbol, start })));
            }

            // Check for single character operators
            if let Ok(operator) = Operator::try_from(char) {
                return Some(Ok(Token::Operator(OperatorToken { operator, start })));
            }

            let token: Token = match (char, next) {
                ('/', Some('/')) => {
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
                (' ' | '\t' | '\r' | '\n', _) => {
                    continue 'iterator;
                }
                ('"', _) => {
                    let mut buf = String::new();
                    let mut valid = false;
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(next_ch) = self.source.next() {
                        match next_ch {
                            '"' => {
                                // advance iterator and end the string
                                valid = true;
                                break;
                            }
                            _ => {
                                buf.push(next_ch);
                            }
                        }
                    }

                    if valid {
                        Token::String(StringToken {
                            value: buf,
                            start,
                            end: Position {
                                column: self.source.column,
                                line: self.source.line,
                            },
                        })
                    } else {
                        return Some(Err(Error::UnterminatedString {
                            line: self.source.line,
                            column: start.column,
                        }));
                    }
                }
                (char, _) if char.is_ascii_digit() => {
                    let mut num =
                        i64::from(char.to_digit(10).expect("has to be a digit at this point"));
                    while let Some(next_char) = self.source.peek_one() {
                        match next_char.to_digit(10) {
                            Some(digit) => {
                                num *= 10;
                                num += i64::from(digit);
                                self.source.next();
                            }
                            None => {
                                // Something we dont' want we just give back
                                break;
                            }
                        }
                    }
                    Token::Number(NumberToken { value: num, start })
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

                    let token = match buf.as_str() {
                        "while" => Keyword::While,
                        "if" => Keyword::If,
                        "else" => Keyword::Else,
                        "fn" => Keyword::Fn,
                        "for" => Keyword::For,
                        "true" => Keyword::True,
                        "false" => Keyword::False,
                        "return" => Keyword::Return,
                        "self" => Keyword::_Self,
                        "null" => Keyword::Null,
                        _ => {
                            return Some(Ok(Token::Identifier(IdentifierToken {
                                name: buf,
                                start,
                            })))
                        }
                    };

                    Token::Keyword(KeywordToken {
                        keyword: token,
                        start,
                    })
                }
                (char, _) => {
                    return Some(Err(Error::UnexpectedCharacter {
                        char,
                        line: self.source.line,
                        column: self.source.column,
                    }));
                }
            };

            return Some(Ok(token));
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

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnexpectedCharacter {
        char: char,
        line: usize,
        column: usize,
    },
    UnterminatedString {
        line: usize,
        column: usize,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedCharacter { char, line, column } => write!(
                f,
                "Unexpected character '{char}' at line {line} column {column}"
            ),
            Error::UnterminatedString { line, column } => {
                write!(
                    f,
                    "File ended with unterminated string starting at line {line} column {column}"
                )
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::token::Token;
    use crate::lexer::Lexer;

    #[test]
    fn load_file_with_loads_of_tokens() {
        let scanner = Lexer::from_str(include_str!("../tests/lex.andy"));
        assert!(scanner.collect::<Result<Vec<Token>, _>>().is_ok());
    }
}
