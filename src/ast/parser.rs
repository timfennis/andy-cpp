use crate::ast::operator::Operator;
use crate::ast::{Expression, Literal};
use crate::lexer::{Keyword, Symbol};
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use crate::lexer::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }
    fn consume_token(&mut self, types: &[impl Into<TokenType> + Clone]) -> bool {
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

    fn advance(&mut self) {
        if self.current < self.tokens.len() {
            self.current += 1;
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParserError> {
        self.expression()
    }
    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.equality()
    }

    fn get_previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn consume_binary_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<Expression, ParserError>,
        valid_tokens: &[impl Into<TokenType> + Clone],
    ) -> Result<Expression, ParserError> {
        let mut expr = next(self)?;
        while self.consume_token(valid_tokens) {
            let operator_token = self
                .get_previous()
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
            &[Operator::Divide, Operator::Multiply, Operator::CModulo, Operator::EuclideanModulo],
        )
    }

    fn exponent(&mut self) -> Result<Expression, ParserError> {
        self.consume_binary_expression_left_associative(
            Self::unary,
            &[Operator::Exponent],
        )
    }

    fn unary(&mut self) -> Result<Expression, ParserError> {
        if self.consume_token(&[Operator::Bang, Operator::Minus]) {
            // TODO: maybe we can get rid of this clone if we consume the token stream in a different way
            let operator_token = self
                .get_previous()
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
        let token = self
            .current_token()
            .ok_or(ParserError::UnexpectedEndOfStream)?
            .clone();

        self.advance();

        Ok(match &token.typ {
            TokenType::Keyword(Keyword::False) => Expression::Literal(Literal::False),
            TokenType::Keyword(Keyword::True) => Expression::Literal(Literal::True),
            TokenType::Keyword(Keyword::Null) => Expression::Literal(Literal::Null),
            TokenType::Integer(num) => Expression::Literal(Literal::Integer(*num)),
            TokenType::String(string) => Expression::Literal(Literal::String(string.clone())),
            TokenType::Symbol(Symbol::LeftParentheses) => {
                let expr = self.expression()?;
                self.consume(Symbol::RightParentheses)?;
                Expression::Grouping(Box::new(expr))
            }
            _ => {
                return Err(ParserError::ExpectedExpression {
                    token: token.clone(),
                });
            }
        })
    }

    fn consume(&mut self, typ: impl Into<TokenType>) -> Result<&Token, ParserError> {
        let typ = typ.into();
        return if let Some(token) = self.current_token() {
            if token.typ == typ {
                self.advance();
                self.get_previous()
                    .ok_or(ParserError::UnexpectedEndOfStream)
            } else {
                Err(ParserError::MissingExpectedToken {
                    typ,
                    token: token.clone(),
                })
            }
        } else {
            Err(ParserError::UnexpectedEndOfStream)
        };
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParserError {
    MissingExpectedToken { typ: TokenType, token: Token },
    UnexpectedEndOfStream,
    ExpectedExpression { token: Token },
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: Stop using debug formatting for token types
        match self {
            ParserError::MissingExpectedToken { token, typ } => write!(
                f,
                "Missing expected token {:?} on line {} column {}",
                typ, token.line, token.column
            ),
            ParserError::UnexpectedEndOfStream => write!(f, "Unexpected end of stream"),
            ParserError::ExpectedExpression { token } => write!(
                f,
                "Unexpected token '{:?}' expected expression on line {} column {}",
                token.typ, token.line, token.column
            ),
        }
    }
}
