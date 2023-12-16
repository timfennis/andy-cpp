use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Eq, PartialEq)]
enum TokenType {
    // Random operators
    Dot,

    // Control
    Semicolon,
    Comma,
    LeftParentheses,
    RightParentheses,
    LeftBrace,
    RightBrace,
    ColonEquals,

    // Math
    Minus,
    Plus,
    Star,
    Slash,
    Percent,

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
    Identifier(String),
    String(String),
    Integer(i64),

    // Keywords
    Fn,
    If,
    Else,
    Return,
    For,
    While,
    True,
    False,
    _Self,
}

#[derive(Debug, Eq, PartialEq)]
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
        Self {
            source: source.chars().peekable(),
            line: 1,
            column: 1,
        }
    }
}

impl Iterator for Scanner<'_> {
    type Item = Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: possibly use peekmore https://docs.rs/peekmore/latest/peekmore/ to look ahead further
        'iterator: while let Some(char) = self.source.next() {
            let start_column = self.column;
            self.column += 1;
            let peek = self.source.peek();
            let (token_type, consume_next) = match (char, peek) {
                ('(', _) => (TokenType::LeftParentheses, false),
                (')', _) => (TokenType::RightParentheses, false),
                ('{', _) => (TokenType::LeftBrace, false),
                ('}', _) => (TokenType::RightBrace, false),
                (',', _) => (TokenType::Comma, false),
                ('-', _) => (TokenType::Minus, false),
                ('+', _) => (TokenType::Plus, false),
                (';', _) => (TokenType::Semicolon, false),
                ('*', _) => (TokenType::Star, false),
                ('%', _) => (TokenType::Percent, false),
                ('.', _) => (TokenType::Dot, false),
                (':', Some('=')) => (TokenType::ColonEquals, true),
                ('!', Some('=')) => (TokenType::BangEqual, true),
                ('!', _) => (TokenType::Bang, false),
                ('=', Some('=')) => (TokenType::EqualEqual, true),
                ('=', _) => (TokenType::Equal, false),
                ('>', Some('=')) => (TokenType::GreaterEqual, true),
                ('>', _) => (TokenType::Greater, false),
                ('<', Some('=')) => (TokenType::LessEqual, true),
                ('<', _) => (TokenType::Less, false),
                ('/', Some('/')) => {
                    // We ran into a doc comment and we just keep consuming characters as long as we
                    // don't encounter a linebreak
                    while let Some(ch) = self.source.peek() {
                        if *ch == '\n' {
                            continue 'iterator;
                        }

                        self.source.next();
                        self.column += 1;
                    }

                    continue 'iterator;
                }
                ('/', Some(_)) => (TokenType::Slash, false),
                (' ' | '\t' | '\r', _) => {
                    continue 'iterator;
                }
                ('\n', _) => {
                    self.line += 1;
                    self.column = 1;
                    continue 'iterator;
                }
                ('"', _) => {
                    let mut buf = String::new();
                    let mut valid = false;
                    while let Some(next_ch) = self.source.next() {
                        self.column += 1;
                        match next_ch {
                            '\n' => {
                                self.line += 1;
                            }
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
                        (TokenType::String(buf), false)
                    } else {
                        return Some(Err(ScannerError::UnterminatedString {
                            line: self.line,
                            column: start_column,
                        }));
                    }
                }
                (char, _) if char.is_ascii_digit() => {
                    let mut num =
                        char.to_digit(10).expect("has to be a digit at this point") as i64;
                    while let Some(next_char) = self.source.peek() {
                        match next_char {
                            next_char if next_char.is_ascii_digit() => {
                                num *= 10;
                                num += self
                                    .source
                                    .next()
                                    .and_then(|ch| ch.to_digit(10))
                                    .expect("has to be a digit at this point")
                                    as i64;
                                self.column += 1;
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    (TokenType::Integer(num), false)
                }
                //TODO: For now we just support boring c-style identifiers but maybe allowing Emoji or other characters
                //      could be cool too
                (char, _) if char.is_alphabetic() || char == '_' => {
                    // Parse an identifier, or not
                    let mut buf = String::new();
                    buf.push(char);
                    while let Some(next_char) = self.source.peek() {
                        if next_char.is_alphanumeric() {
                            // advance iterator for next
                            buf.push(self.source.next().expect("has to be Some"));
                            self.column += 1;
                        } else {
                            break;
                        }
                    }

                    let token_type = match buf.as_str() {
                        "while" => TokenType::While,
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        "fn" => TokenType::Fn,
                        "for" => TokenType::For,
                        "true" => TokenType::True,
                        "false" => TokenType::False,
                        "return" => TokenType::Return,
                        "self" => TokenType::_Self,
                        _ => TokenType::Identifier(buf),
                    };

                    (token_type, false)
                }
                (char, _) => {
                    // In case of an error we still push back the next character so we can continue parsing
                    return Some(Err(ScannerError::UnexpectedCharacter {
                        char,
                        line: self.line,
                        column: start_column,
                    }));
                }
            };

            if consume_next {
                // swallow the next char
                self.source.next();
                self.column += 1;
            }

            return Some(Ok(Token {
                kind: token_type,
                line: self.line,
                column: start_column,
            }));
        }

        None
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ScannerError {
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

impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScannerError::UnexpectedCharacter { char, line, column } => write!(
                f,
                "Unexpected character '{char}' at line {line} column {column}"
            ),
            ScannerError::UnterminatedString { line, column } => {
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
    use crate::scanner::{Scanner, Token, TokenType};

    #[test]
    fn loads_of_operators() {
        let scanner = Scanner::from_str(include_str!("../tests/scanner.andy"));
        assert!(scanner.collect::<Result<Vec<Token>, _>>().is_ok());
    }

    #[test]
    fn simple_script() {
        let script = r#"while foo := bar() { print("foobar"); }"#;
        let scanner = Scanner::from_str(script);
        let scanned = scanner.collect::<Result<Vec<Token>, _>>().unwrap();
        let expected = vec![
            Token {
                kind: TokenType::While,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenType::Identifier("foo".to_owned()),
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenType::ColonEquals,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenType::Identifier("bar".to_owned()),
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenType::LeftParentheses,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenType::RightParentheses,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenType::LeftBrace,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenType::Identifier("print".to_owned()),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenType::LeftParentheses,
                line: 1,
                column: 27,
            },
            Token {
                kind: TokenType::String("foobar".to_owned()),
                line: 1,
                column: 28,
            },
            Token {
                kind: TokenType::RightParentheses,
                line: 1,
                column: 36,
            },
            Token {
                kind: TokenType::Semicolon,
                line: 1,
                column: 37,
            },
            Token {
                kind: TokenType::RightBrace,
                line: 1,
                column: 39,
            },
        ];

        for (a, b) in scanned.iter().zip(&expected) {
            assert_eq!(a, b);
        }
    }
}
