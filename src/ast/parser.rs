use crate::ast::operator::Operator;
use crate::ast::{Expression, Literal, Statement};
use crate::lexer::Keyword;
use crate::lexer::Symbol;
use crate::lexer::{Token, TokenType};
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub fn parse(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut v = Vec::new();
        while self.current_token().is_some() {
            // Right now we just break when we se any error but maybe we should handle this differently
            v.push(self.statement()?);
        }
        Ok(v)
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    /// Returns the token at a given position relative to the current token. token_at(0) is equal to current_token()
    fn token_at(&self, idx: usize) -> Option<&Token> {
        self.tokens.get(self.current + idx)
    }

    fn previous_token(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    // If the next token is one of the given tokens they are discarded from the stream and the function returns true
    // if the next token is not equal to one of the provided tokens this function returns false.
    fn consume_token_if(&mut self, types: &[impl Into<TokenType> + Clone]) -> bool {
        for typ in types {
            if let Some(token) = self.current_token() {
                let typ: TokenType = typ.clone().into();
                if token.typ == typ {
                    self.advance();
                    return true;
                }
            }
        }
        false
    }

    // This is a helper function probably mostly for the REPL but maybe useful for the last line in a function or expression body
    fn require_semicolon_or_end_of_stream(&mut self) -> Result<(), ParserError> {
        match self.current_token() {
            // No token is fine
            None => Ok(()),
            // If there is a token it must be a semicolon
            Some(token) => {
                if token.typ == TokenType::Symbol(Symbol::Semicolon) {
                    self.advance();
                    Ok(())
                } else {
                    Err(ParserError::MissingExpectedToken {
                        actual_token: token.clone(),
                        expected_token: TokenType::Symbol(Symbol::Semicolon),
                    })
                }
            }
        }
    }
    /// Requires that the current token matches a type, if it doesn't an error is thrown.
    /// If the token does match the expected type we advance the iterator
    fn require_current_token_matches(
        &mut self,
        typ: impl Into<TokenType>,
    ) -> Result<&Token, ParserError> {
        let typ = typ.into();
        return if let Some(token) = self.current_token() {
            if token.typ == typ {
                self.advance();
                self.previous_token()
                    .ok_or(ParserError::UnexpectedEndOfStream)
            } else {
                Err(ParserError::MissingExpectedToken {
                    expected_token: typ,
                    actual_token: token.clone(),
                })
            }
        } else {
            Err(ParserError::UnexpectedEndOfStream)
        };
    }

    // Requires that there is a valid current token, otherwise it returns Err(ParserError)
    fn require_current_token(&mut self) -> Result<Token, ParserError> {
        let token = self
            .current_token()
            .ok_or(ParserError::UnexpectedEndOfStream)?
            .clone();
        self.advance();
        Ok(token)
    }

    fn require_identifier(&mut self) -> Result<Token, ParserError> {
        match self.require_current_token()? {
            token @ Token {
                typ: TokenType::Identifier(_),
                ..
            } => Ok(token),
            token => Err(ParserError::ExpectedIdentifier {
                actual_token: token,
            }),
        }
    }

    fn consume_binary_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<Expression, ParserError>,
        valid_tokens: &[impl Into<TokenType> + Clone],
    ) -> Result<Expression, ParserError> {
        let mut expr = next(self)?;
        while self.consume_token_if(valid_tokens) {
            let operator_token = self
                .previous_token()
                .ok_or(ParserError::UnexpectedEndOfStream)?
                .clone();
            let right = next(self)?;

            expr = Expression::Binary {
                left: Box::new(expr),
                // TODO: maybe we can get rid of this clone if we consume the stream of tokens in a different way
                operator_token,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn advance(&mut self) {
        if self.current < self.tokens.len() {
            self.current += 1;
        }
    }

    /************************************************* PARSER *************************************************/

    fn statement(&mut self) -> Result<Statement, ParserError> {
        // match print statements (which is a temporary construct until we add functions)
        if matches!(
            self.current_token(),
            Some(Token {
                typ: TokenType::Identifier(ident),
                ..
            }) if ident == "print"
        ) {
            self.advance();
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Statement, ParserError> {
        let value = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        match (self.token_at(0), self.token_at(1)) {
            // If the current token is an identifier followed by the create var operator we create a new variable
            (
                Some(Token {
                    typ: TokenType::Identifier(_),
                    ..
                }),
                Some(Token {
                    typ: TokenType::Operator(Operator::CreateVar),
                    ..
                }),
            ) => return self.variable_declaration(),
            // If the current token is an identifier followed by an equals sign we assign the variable
            (
                Some(Token {
                    typ: TokenType::Identifier(_),
                    ..
                }),
                Some(Token {
                    typ: TokenType::Operator(Operator::EqualsSign),
                    ..
                }),
            ) => {
                return self.variable_assignment();
            }
            _ => {}
        }

        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.equality()
    }

    fn variable_declaration(&mut self) -> Result<Statement, ParserError> {
        let identifier = self.require_identifier()?;
        self.require_current_token_matches(Operator::CreateVar)?;

        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::VariableDeclaration { identifier, expr })
    }

    fn variable_assignment(&mut self) -> Result<Statement, ParserError> {
        let identifier = self.require_identifier()?;
        self.require_current_token_matches(Operator::EqualsSign)?;

        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::VariableAssignment { identifier, expr })
    }

    fn equality(&mut self) -> Result<Expression, ParserError> {
        self.consume_binary_expression_left_associative(
            Self::comparison,
            &[Operator::Equality, Operator::Inequality],
        )
    }

    fn comparison(&mut self) -> Result<Expression, ParserError> {
        self.consume_binary_expression_left_associative(
            Self::term,
            &[
                Operator::Greater,
                Operator::GreaterEquals,
                Operator::Less,
                Operator::LessEquals,
            ],
        )
    }

    fn term(&mut self) -> Result<Expression, ParserError> {
        self.consume_binary_expression_left_associative(
            Self::factor,
            &[Operator::Plus, Operator::Minus],
        )
    }

    fn factor(&mut self) -> Result<Expression, ParserError> {
        self.consume_binary_expression_left_associative(
            Self::exponent,
            &[
                Operator::Divide,
                Operator::Multiply,
                Operator::CModulo,
                Operator::EuclideanModulo,
            ],
        )
    }

    fn exponent(&mut self) -> Result<Expression, ParserError> {
        self.consume_binary_expression_left_associative(Self::unary, &[Operator::Exponent])
    }

    fn unary(&mut self) -> Result<Expression, ParserError> {
        if self.consume_token_if(&[Operator::Bang, Operator::Minus]) {
            // TODO: maybe we can get rid of this clone if we consume the token stream in a different way
            let operator_token = self
                .previous_token()
                .ok_or(ParserError::UnexpectedEndOfStream)?
                .clone();

            let right = self.unary()?;
            return Ok(Expression::Unary {
                operator_token,
                expression: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        let token = self.require_current_token()?;

        let expression = match token.typ {
            TokenType::Keyword(Keyword::False) => Expression::Literal(Literal::False),
            TokenType::Keyword(Keyword::True) => Expression::Literal(Literal::True),
            TokenType::Keyword(Keyword::Null) => Expression::Literal(Literal::Null),
            TokenType::Integer(num) => Expression::Literal(Literal::Integer(num)),
            TokenType::String(string) => Expression::Literal(Literal::String(string.clone())),
            TokenType::Symbol(Symbol::LeftParentheses) => {
                let expr = self.expression()?;
                self.require_current_token_matches(Symbol::RightParentheses)?;
                Expression::Grouping(Box::new(expr))
            }
            TokenType::Identifier(_) => Expression::Variable { token },
            _ => {
                return Err(ParserError::ExpectedExpression {
                    actual_token: token,
                });
            }
        };

        Ok(expression)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParserError {
    MissingExpectedToken {
        expected_token: TokenType,
        actual_token: Token,
    },
    UnexpectedEndOfStream,
    ExpectedExpression {
        actual_token: Token,
    },
    ExpectedIdentifier {
        actual_token: Token,
    },
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: Stop using debug formatting for token types
        match self {
            ParserError::MissingExpectedToken {
                actual_token: token,
                expected_token: typ,
            } => write!(
                f,
                "Missing expected token {} on line {} column {}",
                typ, token.line, token.column
            ),
            ParserError::UnexpectedEndOfStream => write!(f, "Unexpected end of stream"),
            ParserError::ExpectedExpression {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{:?}' expected expression on line {} column {}",
                token.typ, token.line, token.column
            ),
            ParserError::ExpectedIdentifier {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{:?}' expected identifier on line {} column {}",
                token.typ, token.line, token.column
            ),
        }
    }
}
