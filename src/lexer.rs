use crate::ast::operator::Operator;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::str::Chars;

mod token;
pub use token::{TokenType, Token};


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Symbol {
    LeftParentheses,
    RightParentheses,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::LeftParentheses => write!(f, "("),
            Symbol::RightParentheses => write!(f, ")"),
            Symbol::LeftBracket => write!(f, "["),
            Symbol::RightBracket => write!(f, "]"),
            Symbol::LeftBrace => write!(f, "{{"),
            Symbol::RightBrace => write!(f, "}}"),
            Symbol::Semicolon => write!(f, ";"),
            Symbol::Comma => write!(f, ","),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Keyword {
    Fn,
    If,
    Else,
    Return,
    For,
    While,
    True,
    False,
    _Self,
    Null,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fn => write!(f, "fn"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Return => write!(f, "return"),
            Keyword::For => write!(f, "for"),
            Keyword::While => write!(f, "while"),
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::_Self => write!(f, "self"),
            Keyword::Null => write!(f, "null"),
        }
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
        return if let Some(head) = self.buffer.get(0) {
            Some(*head)
        } else {
            self.peek_next()
        };
    }
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

            let (token_type, consume_next): (TokenType, bool) = match (char, next) {
                ('(', _) => (Symbol::LeftParentheses.into(), false),
                (')', _) => (Symbol::RightParentheses.into(), false),
                ('{', _) => (Symbol::LeftBrace.into(), false),
                ('}', _) => (Symbol::RightBrace.into(), false),
                ('[', _) => (Symbol::LeftBracket.into(), false),
                (']', _) => (Symbol::RightBracket.into(), false),
                (',', _) => (Symbol::Comma.into(), false),
                (';', _) => (Symbol::Semicolon.into(), false),
                ('-', _) => (Operator::Minus.into(), false),
                ('+', _) => (Operator::Plus.into(), false),
                ('*', _) => (Operator::Multiply.into(), false),
                ('^', _) => (Operator::Exponent.into(), false),
                ('%', Some('%')) => (Operator::EuclideanModulo.into(), true),
                ('%', _) => (Operator::CModulo.into(), false),
                // ('.', _) => (Symbol::Dot, false),
                (':', Some('=')) => (Operator::CreateVar.into(), true),
                ('!', Some('=')) => (Operator::Inequality.into(), true),
                ('!', _) => (Operator::Bang.into(), false),
                ('=', Some('=')) => (Operator::Equality.into(), true),
                ('=', _) => (Operator::EqualsSign.into(), false),
                ('>', Some('=')) => (Operator::GreaterEquals.into(), true),
                ('>', _) => (Operator::Greater.into(), false),
                ('<', Some('=')) => (Operator::LessEquals.into(), true),
                ('<', _) => (Operator::Less.into(), false),
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
                ('/', Some(_)) => (Operator::Divide.into(), false),
                (' ' | '\t' | '\r', _) => {
                    continue 'iterator;
                }
                ('\n', _) => {
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
                        (TokenType::String(buf), false)
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
                    while let Some(next_char) = self.source.peek_one() {
                        match next_char.to_digit(10) {
                            Some(digit) => {
                                num *= 10;
                                num += digit as i64;
                                self.source.next();
                            }
                            None => {
                                // Something we dont' want we just give back
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
                    while let Some(next_char) = self.source.peek_one() {
                        if next_char.is_alphanumeric() || next_char == '_' {
                            // advance iterator for next
                            buf.push(next_char);
                            self.source.next();
                        } else {
                            break;
                        }
                    }

                    let token_type = match buf.as_str() {
                        "while" => Keyword::While.into(),
                        "if" => Keyword::If.into(),
                        "else" => Keyword::Else.into(),
                        "fn" => Keyword::Fn.into(),
                        "for" => Keyword::For.into(),
                        "true" => Keyword::True.into(),
                        "false" => Keyword::False.into(),
                        "return" => Keyword::Return.into(),
                        "self" => Keyword::_Self.into(),
                        "null" => Keyword::Null.into(),
                        _ => TokenType::Identifier(buf),
                    };

                    (token_type, false)
                }
                (char, _) => {
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
                typ: token_type,
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
    use crate::lexer::Lexer;
    use crate::lexer::token::{Token, TokenType};

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
                typ: TokenType::Integer(33),
                line: 1,
                column: 8,
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
                typ: TokenType::Identifier("_this_is_a_single_1dentifi3r".to_owned()),
                column: 1,
                line: 1,
            }
        )
    }
}
