use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::str::Chars;

#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
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
    Nil,
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
        return if let Some(head) = self.buffer.get(0) {
            Some(*head)
        } else {
            self.peek_next()
        };
    }

    pub fn push_front(&mut self, ch: char) {
        if ch != '\n' {
            self.column -= 1;
        } else {
            self.line -= 1;
            // TODO: we can't reset the column number here, what to do, just assume it's never a problem?
        }
        self.buffer.push_front(ch)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    kind: TokenKind,
    line: usize,
    column: usize,
}

pub struct Lexer<'a> {
    source: SourceIterator<'a>,
}

impl<'a> Lexer<'a> {
    pub fn from_str(source: &'a str) -> Self {
        Self {
            source: SourceIterator {
                inner: source.chars(),
                buffer: Default::default(),
                line: 1,
                column: 0,
            },
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        'iterator: while let Some(char) = self.source.next() {
            let start_column = self.source.column;
            let next = self.source.peek_one();

            let (token_type, consume_next) = match (char, next) {
                ('(', _) => (TokenKind::LeftParentheses, false),
                (')', _) => (TokenKind::RightParentheses, false),
                ('{', _) => (TokenKind::LeftBrace, false),
                ('}', _) => (TokenKind::RightBrace, false),
                (',', _) => (TokenKind::Comma, false),
                ('-', _) => (TokenKind::Minus, false),
                ('+', _) => (TokenKind::Plus, false),
                (';', _) => (TokenKind::Semicolon, false),
                ('*', _) => (TokenKind::Star, false),
                ('%', _) => (TokenKind::Percent, false),
                ('.', _) => (TokenKind::Dot, false),
                (':', Some('=')) => (TokenKind::ColonEquals, true),
                ('!', Some('=')) => (TokenKind::BangEqual, true),
                ('!', _) => (TokenKind::Bang, false),
                ('=', Some('=')) => (TokenKind::EqualEqual, true),
                ('=', _) => (TokenKind::Equal, false),
                ('>', Some('=')) => (TokenKind::GreaterEqual, true),
                ('>', _) => (TokenKind::Greater, false),
                ('<', Some('=')) => (TokenKind::LessEqual, true),
                ('<', _) => (TokenKind::Less, false),
                ('/', Some('/')) => {
                    // We ran into a doc comment and we just keep consuming characters as long as we
                    // don't encounter a linebreak
                    while let Some(ch) = self.source.next() {
                        if ch == '\n' {
                            self.source.push_front('\n');
                            continue 'iterator;
                        }

                        self.source.next();
                    }

                    continue 'iterator;
                }
                ('/', Some(_)) => (TokenKind::Slash, false),
                (' ' | '\t' | '\r', _) => {
                    continue 'iterator;
                }
                ('\n', _) => {
                    continue 'iterator;
                }
                ('"', _) => {
                    let mut buf = String::new();
                    let mut valid = false;
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
                        (TokenKind::String(buf), false)
                    } else {
                        return Some(Err(LexerError::UnterminatedString {
                            line: self.source.line,
                            column: start_column,
                        }));
                    }
                }
                (char, _) if char.is_ascii_digit() => {
                    let mut num =
                        char.to_digit(10).expect("has to be a digit at this point") as i64;
                    while let Some(next_char) = self.source.next() {
                        match next_char.to_digit(10) {
                            Some(digit) => {
                                num *= 10;
                                num += digit as i64;
                            }
                            None => {
                                // Something we dont' want we just give back
                                self.source.push_front(next_char);
                                break;
                            }
                        }
                    }
                    (TokenKind::Integer(num), false)
                }
                //TODO: For now we just support boring c-style identifiers but maybe allowing Emoji or other characters
                //      could be cool too
                (char, _) if char.is_alphabetic() || char == '_' => {
                    // Parse an identifier, or not
                    let mut buf = String::new();
                    buf.push(char);
                    while let Some(next_char) = self.source.next() {
                        if next_char.is_alphanumeric() || next_char == '_' {
                            // advance iterator for next
                            buf.push(next_char);
                        } else {
                            self.source.push_front(next_char);
                            break;
                        }
                    }

                    let token_type = match buf.as_str() {
                        "while" => TokenKind::While,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "fn" => TokenKind::Fn,
                        "for" => TokenKind::For,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        "return" => TokenKind::Return,
                        "self" => TokenKind::_Self,
                        "nil" => TokenKind::Nil,
                        _ => TokenKind::Identifier(buf),
                    };

                    (token_type, false)
                }
                (char, _) => {
                    // In case of an error we still push back the next character so we can continue parsing
                    return Some(Err(LexerError::UnexpectedCharacter {
                        char,
                        line: self.source.line,
                        column: start_column,
                    }));
                }
            };

            if consume_next {
                // swallow the next char
                self.source.next();
            }

            return Some(Ok(Token {
                kind: token_type,
                line: self.source.line,
                column: start_column,
            }));
        }

        None
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexerError {
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

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedCharacter { char, line, column } => write!(
                f,
                "Unexpected character '{char}' at line {line} column {column}"
            ),
            LexerError::UnterminatedString { line, column } => {
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
    use crate::lexer::{Lexer, Token, TokenKind};

    #[test]
    fn load_file_with_loads_of_tokens() {
        let scanner = Lexer::from_str(include_str!("../tests/lex.andy"));
        assert!(scanner.collect::<Result<Vec<Token>, _>>().is_ok());
    }

    #[test]
    fn simple_arithmetic_assignment() {
        let lexer = Lexer::from_str("foo := 33");
        let tokens = lexer
            .collect::<Result<Vec<Token>, _>>()
            .expect("must have only valid tokens");

        assert_eq!(
            Token {
                kind: TokenKind::Integer(33),
                line: 1,
                column: 8
            },
            tokens[2]
        );
    }
    #[test]
    fn identifiers_with_underscores() {
        let lexer = Lexer::from_str("_this_is_a_single_1dentifi3r");
        let tokens = lexer.collect::<Result<Vec<Token>, _>>().unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            *tokens.first().unwrap(),
            Token {
                kind: TokenKind::Identifier("_this_is_a_single_1dentifi3r".to_owned()),
                column: 1,
                line: 1
            }
        )
    }

    #[test]
    fn simple_script() {
        let script = r#"while foo := bar() { print("foobar"); }"#;
        let scanner = Lexer::from_str(script);
        let scanned = scanner.collect::<Result<Vec<Token>, _>>().unwrap();
        let expected = vec![
            Token {
                kind: TokenKind::While,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("foo".to_owned()),
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenKind::ColonEquals,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("bar".to_owned()),
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::LeftParentheses,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::RightParentheses,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::LeftBrace,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Identifier("print".to_owned()),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::LeftParentheses,
                line: 1,
                column: 27,
            },
            Token {
                kind: TokenKind::String("foobar".to_owned()),
                line: 1,
                column: 28,
            },
            Token {
                kind: TokenKind::RightParentheses,
                line: 1,
                column: 36,
            },
            Token {
                kind: TokenKind::Semicolon,
                line: 1,
                column: 37,
            },
            Token {
                kind: TokenKind::RightBrace,
                line: 1,
                column: 39,
            },
        ];

        for (a, b) in scanned.iter().zip(&expected) {
            assert_eq!(a, b);
        }
    }
}
