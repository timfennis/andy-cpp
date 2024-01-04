use crate::ast::expression::Expression;
use crate::ast::literal::Literal;
use crate::ast::operator::Operator;
use crate::ast::Statement;
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
        return match self.current_token() {
            Some(token) if token.typ == TokenType::Identifier(String::from("print")) => {
                self.advance();
                self.print_statement()
            }
            // For now we can just treat any kind of identifier as the start of a declaration?
            // TODO: this fails we should check if there is an identifier followed by an assignment operator
            // Some(Token {
            //     typ: TokenType::Identifier(_),
            //     ..
            // }) => self.variable_declaration(),
            _ => self.expression_statement(),
        };
    }

    // TODO
    fn variable_declaration(&mut self) -> Result<Statement, ParserError> {
        let identifier = self.require_current_token()?;
        let identifier = match identifier.typ {
            TokenType::Identifier(identifier) => identifier,
            _ => panic!("we are guaranteed to get an identifier here, but maybe handle this case in the future"),
        };

        // TODO: possibly temporarily handle this special case here (mostly for the REPL)
        //       this case is triggered if the entire statement is JUST an identifier with no assignment following it
        if self.current_token().is_none() {
            return Ok(Statement::Expression(Expression::Variable(identifier)));
        }

        self.require_current_token_matches(Operator::CreateVar)?;

        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::VariableDeclaration { identifier, expr })
    }

    fn print_statement(&mut self) -> Result<Statement, ParserError> {
        let value = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::Expression(expr))
    }
    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.equality()
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
            TokenType::Identifier(name) => Expression::Variable(name),
            _ => {
                return Err(ParserError::ExpectedExpression { token });
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
        token: Token,
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
            ParserError::ExpectedExpression { token } => write!(
                f,
                "Unexpected token '{:?}' expected expression on line {} column {}",
                token.typ, token.line, token.column
            ),
        }
    }
}
