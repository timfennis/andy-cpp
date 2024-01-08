use crate::ast::operator::Operator;
use crate::ast::{Expression, Literal, Statement};
use crate::lexer::{IdentifierToken, KeywordToken, Symbol};
use crate::lexer::{Keyword, OperatorToken};
use crate::lexer::{SymbolToken, Token};
use std::fmt;
use std::fmt::Formatter;
use std::string::ParseError;

pub(crate) struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub(crate) fn from_tokens(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    /// Parse a Vec of Tokens into a Vec of statements
    /// # Errors
    /// If the parsing fails which it could do for many different reasons it will return an [Error]. See the variants of
    /// this type for more information about the different kinds of errors.
    pub(crate) fn parse(&mut self) -> Result<Vec<Statement>, Error> {
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

    /// Returns the token at a given position relative to the current token. `token_at(0`) is equal to `current_token`()
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

    fn is_end_of_stream(&self) -> bool {
        self.current_token().is_none()
    }

    // This is a helper function probably mostly for the REPL but maybe useful for the last line in a function or expression body
    fn require_semicolon_or_end_of_stream(&mut self) -> Result<(), Error> {
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
            Some(_) => Err(Error::ExpectedSymbol {
                actual_token: self.require_current_token()?,
                expected_symbol: Symbol::Semicolon,
            }),
        }
    }

    /// Requires that the current token matches a type, if it doesn't an error is thrown.
    /// If the token does match the expected type we advance the iterator
    fn require_current_token_matches_symbol(
        &mut self,
        match_symbol: Symbol,
    ) -> Result<&Token, Error> {
        let token = self.current_token().ok_or(Error::UnexpectedEndOfStream {
            help_text: format!("Expected a {match_symbol} but got end of stream instead"),
        })?;

        return if let Token::Symbol(SymbolToken { symbol: value, .. }) = token {
            if match_symbol == *value {
                self.advance();
                Ok(self
                    .previous_token()
                    .expect("We are guaranteed to have a previous token here"))
            } else {
                Err(Error::ExpectedSymbol {
                    expected_symbol: match_symbol,
                    actual_token: token.clone(),
                })
            }
        } else {
            dbg!(&match_symbol, &token);
            panic!("TODO:  handle this case better, I think we're expecting a symbol but we're getting some other token");
            // Err(Error::UnexpectedEndOfStream)
        };
    }

    fn require_current_token_matches_operator(
        &mut self,
        match_operator: Operator,
    ) -> Result<&Token, Error> {
        let token = self.current_token().ok_or(Error::UnexpectedEndOfStream {
            help_text: format!("expected a {match_operator} but got end of stream instead"),
        })?;

        return if let Token::Operator(OperatorToken { operator, .. }) = token {
            if match_operator == *operator {
                self.advance();
                Ok(self
                    .previous_token()
                    .expect("we are guaranteed to have a previous token at this point"))
            } else {
                Err(Error::ExpectedOperator {
                    expected_operator: match_operator,
                    actual_token: token.clone(),
                })
            }
        } else {
            // Err(Error::UnexpectedEndOfStream)
            panic!("TODO: handle this case better, I think we expected an operator but got a different type of token")
        };
    }

    // Requires that there is a valid current token, otherwise it returns Err(ParserError)
    fn require_current_token(&mut self) -> Result<Token, Error> {
        let token = self
            .current_token()
            .ok_or_else(|| Error::UnexpectedEndOfStream { help_text: String::from("a token was required but an end of stream was found instead (require_current_token)")})?
            .clone();
        self.advance();
        Ok(token)
    }

    fn require_identifier(&mut self) -> Result<IdentifierToken, Error> {
        match self.require_current_token()? {
            Token::Identifier(identifier_token) => Ok(identifier_token),
            token => Err(Error::ExpectedIdentifier {
                actual_token: token,
            }),
        }
    }

    fn consume_binary_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<Expression, Error>,
        valid_operators: &[Operator],
    ) -> Result<Expression, Error> {
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

    fn statement(&mut self) -> Result<Statement, Error> {
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

    fn print_statement(&mut self) -> Result<Statement, Error> {
        let value = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Statement, Error> {
        let expr = self.expression()?;
        self.require_semicolon_or_end_of_stream()?;

        Ok(Statement::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, Error> {
        self.variable_declaration_or_assignment()
    }

    fn variable_declaration_or_assignment(&mut self) -> Result<Expression, Error> {
        // NOTE: I think the way we implemented this method makes it right associative
        // self.equality() is the next order of precedence at this point
        let maybe_identifier = self.equality()?;

        // If the token we parsed is some kind of variable we can potentially assign to it. Next we'll check if the next
        // token matches either the declaration operator or the assignment operator. If neither of those matches we are
        // just returning the variable expression as is.
        if let Expression::Variable { token } = maybe_identifier {
            return if self.consume_operator_if(&[Operator::CreateVar]) {
                Ok(Expression::VariableDeclaration {
                    token,
                    value: Box::new(self.variable_declaration_or_assignment()?),
                })
            } else if self.consume_operator_if(&[Operator::EqualsSign]) {
                Ok(Expression::VariableAssignment {
                    token,
                    value: Box::new(self.variable_declaration_or_assignment()?),
                })
            } else {
                // Does this m ake any sense???????
                Ok(Expression::Variable { token })
            };
        }

        // In this case we got some kind of expression that we can't assign to. We can just return the expression as is.
        // But to improve error handling and stuff it would be nice if we could check if the next token matches one
        // of the assignment operator and throw an appropriate error.
        match self.current_token() {
            Some(Token::Operator(OperatorToken {
                operator: Operator::CreateVar,
                ..
            }))
            | Some(Token::Operator(OperatorToken {
                operator: Operator::EqualsSign,
                ..
            })) => Err(Error::InvalidAssignmentTarget {
                target: maybe_identifier,
            }),
            _ => Ok(maybe_identifier),
        }
    }

    fn equality(&mut self) -> Result<Expression, Error> {
        self.consume_binary_expression_left_associative(
            Self::comparison,
            &[Operator::Equality, Operator::Inequality],
        )
    }

    fn comparison(&mut self) -> Result<Expression, Error> {
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

    fn term(&mut self) -> Result<Expression, Error> {
        self.consume_binary_expression_left_associative(
            Self::factor,
            &[Operator::Plus, Operator::Minus],
        )
    }

    fn factor(&mut self) -> Result<Expression, Error> {
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

    fn exponent(&mut self) -> Result<Expression, Error> {
        self.consume_binary_expression_left_associative(Self::unary, &[Operator::Exponent])
    }

    fn unary(&mut self) -> Result<Expression, Error> {
        if self.consume_operator_if(&[Operator::Bang, Operator::Minus]) {
            // TODO: maybe we can get rid of this clone if we consume the token stream in a different way
            let operator_token: OperatorToken = self
                .previous_token()
                .expect("we're guaranteed to have an operator token by the previous call to self.consume_operator_if")
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

    fn primary(&mut self) -> Result<Expression, Error> {
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
                return Err(Error::ExpectedExpression {
                    actual_token: token,
                });
            }
        };

        Ok(expression)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnexpectedEndOfStream {
        help_text: String,
    },
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
    InvalidAssignmentTarget {
        target: Expression,
    },
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: Stop using debug formatting for token types
        match self {
            Error::UnexpectedEndOfStream { help_text } => {
                write!(f, "Unexpected end of stream: {help_text}")
            }
            Error::ExpectedExpression {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{:?}' expected expression on {}",
                token,
                token.position()
            ),
            Error::ExpectedIdentifier {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{:?}' expected identifier on {}",
                token,
                token.position()
            ),
            Error::ExpectedOperator {
                actual_token,
                expected_operator,
            } => write!(
                f,
                "Unexpected token '{:?}' expected operator {} on {}",
                actual_token,
                expected_operator,
                actual_token.position(),
            ),
            Error::ExpectedSymbol {
                actual_token,
                expected_symbol,
            } => write!(
                f,
                "Unexpected token '{:?}' expected symbol {} on {}",
                actual_token,
                expected_symbol,
                actual_token.position()
            ),
            Error::InvalidAssignmentTarget { target } => write!(f, "Invalid variable declaration or assignment. Cannot assign a value to expression: {target:?}")
        }
    }
}
