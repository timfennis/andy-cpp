use crate::ast::operator::Operator;
use crate::ast::{Expression, Literal, Statement};
use crate::lexer::{IdentifierToken, KeywordToken, NumberToken, Symbol};
use crate::lexer::{Keyword, OperatorToken};
use crate::lexer::{SymbolToken, Token};
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
    fn consume_operator_if(&mut self, operators: &[Operator]) -> bool {
        for search_operator in operators {
            match self.current_token() {
                Some(Token::Operator(OperatorToken { operator, .. }))
                    if operator == search_operator =>
                {
                    self.advance();
                    return true;
                }
                _ => {}
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
            Some(Token::Symbol(SymbolToken {
                symbol: Symbol::Semicolon,
                ..
            })) => {
                self.advance();
                Ok(())
            }
            Some(_) => Err(ParserError::ExpectedSymbol {
                actual_token: self.require_current_token()?,
                expected_symbol: Symbol::Semicolon,
            }),
        }
    }

    fn require_operator(&mut self, match_operator: Operator) -> Result<OperatorToken, ParserError> {
        let current_token = self.require_current_token()?;
        match current_token {
            Token::Operator(operator_token @ OperatorToken { operator, .. })
                if match_operator == operator =>
            {
                Ok(operator_token)
            }
            _ => Err(ParserError::ExpectedOperator {
                actual_token: current_token,
                expected_operator: match_operator,
            }),
        }
    }
    /// Requires that the current token matches a type, if it doesn't an error is thrown.
    /// If the token does match the expected type we advance the iterator
    fn require_current_token_matches_symbol(
        &mut self,
        match_symbol: Symbol,
    ) -> Result<&Token, ParserError> {
        let token = self
            .current_token()
            .ok_or(ParserError::UnexpectedEndOfStream)?;

        return if let Token::Symbol(SymbolToken { symbol: value, .. }) = token {
            if match_symbol == *value {
                self.advance();
                self.previous_token()
                    .ok_or(ParserError::UnexpectedEndOfStream)
            } else {
                Err(ParserError::ExpectedSymbol {
                    expected_symbol: match_symbol,
                    actual_token: token.clone(),
                })
            }
        } else {
            Err(ParserError::UnexpectedEndOfStream)
        };
    }

    fn require_current_token_matches_operator(
        &mut self,
        match_operator: Operator,
    ) -> Result<&Token, ParserError> {
        // match (self.current_token(), typ.into()) {};
        let token = self
            .current_token()
            .ok_or(ParserError::UnexpectedEndOfStream)?;

        return if let Token::Operator(OperatorToken { operator, .. }) = token {
            if match_operator == *operator {
                self.advance();
                self.previous_token()
                    .ok_or(ParserError::UnexpectedEndOfStream)
            } else {
                Err(ParserError::ExpectedOperator {
                    expected_operator: match_operator,
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

    fn require_identifier(&mut self) -> Result<IdentifierToken, ParserError> {
        match self.require_current_token()? {
            Token::Identifier(identifier_token) => Ok(identifier_token),
            token => Err(ParserError::ExpectedIdentifier {
                actual_token: token,
            }),
        }
    }

    fn consume_binary_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<Expression, ParserError>,
        valid_operators: &[Operator],
    ) -> Result<Expression, ParserError> {
        let mut expr = next(self)?;
        while self.consume_operator_if(valid_operators) {
            let operator_token: OperatorToken = self
                .previous_token()
                .expect("consume_operator_if that there exists a previous token")
                .clone()
                .try_into()
                .expect("consume_operator_if guaranteed us that this is an OperatorToken");
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
            Some(Token::Identifier(IdentifierToken { name, .. })) if name == "print"
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
                Some(Token::Identifier(_)),
                Some(Token::Operator(OperatorToken {
                    operator: Operator::CreateVar,
                    ..
                })),
            ) => return self.variable_declaration(),
            // If the current token is an identifier followed by an equals sign we assign the variable
            (
                Some(Token::Identifier(_)),
                Some(Token::Operator(OperatorToken {
                    operator: Operator::EqualsSign,
                    ..
                })),
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
        self.require_current_token_matches_operator(Operator::CreateVar)?;

        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::VariableDeclaration { identifier, expr })
    }

    fn variable_assignment(&mut self) -> Result<Statement, ParserError> {
        let identifier = self.require_identifier()?;
        self.require_current_token_matches_operator(Operator::EqualsSign)?;

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
        if self.consume_operator_if(&[Operator::Bang, Operator::Minus]) {
            // TODO: maybe we can get rid of this clone if we consume the token stream in a different way
            let operator_token: OperatorToken = self
                .previous_token()
                .ok_or(ParserError::UnexpectedEndOfStream)?
                .clone()
                .try_into()
                .expect("consume_operator_if guaranteed us that the next token is an Operator");

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

        let expression = match token {
            Token::Keyword(KeywordToken {
                keyword: Keyword::False,
                ..
            }) => Expression::Literal(Literal::False),
            Token::Keyword(KeywordToken {
                keyword: Keyword::True,
                ..
            }) => Expression::Literal(Literal::True),
            Token::Keyword(KeywordToken {
                keyword: Keyword::Null,
                ..
            }) => Expression::Literal(Literal::Null),
            Token::Number(number_token) => {
                Expression::Literal(Literal::Integer(number_token.value))
            }
            Token::String(string_token) => Expression::Literal(Literal::String(string_token.value)),
            Token::Symbol(SymbolToken {
                symbol: Symbol::LeftParentheses,
                ..
            }) => {
                let expr = self.expression()?;
                self.require_current_token_matches_symbol(Symbol::RightParentheses)?;
                Expression::Grouping(Box::new(expr))
            }
            Token::Identifier(identifier) => Expression::Variable { token: identifier },
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
    UnexpectedEndOfStream,
    ExpectedExpression {
        actual_token: Token,
    },
    ExpectedIdentifier {
        actual_token: Token,
    },
    ExpectedOperator {
        actual_token: Token,
        expected_operator: Operator,
    },
    ExpectedSymbol {
        actual_token: Token,
        expected_symbol: Symbol,
    },
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: Stop using debug formatting for token types
        match self {
            ParserError::UnexpectedEndOfStream => write!(f, "Unexpected end of stream"),
            ParserError::ExpectedExpression {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{:?}' expected expression on {}",
                token,
                token.position()
            ),
            ParserError::ExpectedIdentifier {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{:?}' expected identifier on {}",
                token,
                token.position()
            ),
            ParserError::ExpectedOperator {
                actual_token,
                expected_operator,
            } => write!(
                f,
                "Unexpected token '{:?}' expected operator {} on {}",
                actual_token,
                expected_operator,
                actual_token.position(),
            ),
            ParserError::ExpectedSymbol {
                actual_token,
                expected_symbol,
            } => write!(
                f,
                "Unexpected token '{:?}' expected symbol {} on {}",
                actual_token,
                expected_symbol,
                actual_token.position()
            ),
        }
    }
}
