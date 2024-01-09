use crate::ast::operator::Operator;
use crate::ast::{Expression, Literal, Statement};
use crate::lexer::{IdentifierToken, KeywordToken, Symbol};
use crate::lexer::{Keyword, OperatorToken};
use crate::lexer::{SymbolToken, Token};
use std::fmt;
use std::fmt::{Formatter, Write};

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
            v.push(self.statement(false)?);
        }
        Ok(v)
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous_token(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn consume_symbol_if(&mut self, symbols: &[Symbol]) -> bool {
        for search_symbol in symbols {
            match self.current_token() {
                Some(Token::Symbol(SymbolToken { symbol, .. })) if symbol == search_symbol => {
                    self.advance();
                    return true;
                }
                _ => {}
            }
        }
        false
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

    /// This method checks the current token and requires that it terminates a statement or an expression.
    /// It handles the following cases
    ///  * If there are no more tokens it returns `Ok(())`
    ///  * If there is a semicolon `;` it consumes the semicolon and returns `Ok(())`
    ///  * If there is a right curly bracket `}` and `in_block` is `true` it doesn't consume the token but returns Ok()
    ///    basically treating this like it's an end of stream (for the current block).
    ///  * If there is a another token it returns an `Err`
    fn require_end_of_statement_or_expression(&mut self, in_block: bool) -> Result<(), Error> {
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
            // If we run into a closing curly bracket
            Some(Token::Symbol(SymbolToken {
                symbol: Symbol::RightCurlyBracket,
                ..
            })) if in_block => Ok(()),
            Some(_) => {
                let valid_symbols = if in_block {
                    vec![Symbol::Semicolon, Symbol::RightCurlyBracket]
                } else {
                    vec![Symbol::Semicolon]
                };

                Err(Error::ExpectedSymbol {
                    actual_token: self.require_current_token()?,
                    expected_symbols: valid_symbols,
                })
            }
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
                    expected_symbols: vec![match_symbol],
                    actual_token: token.clone(),
                })
            }
        } else {
            dbg!(&match_symbol, &token);
            panic!("TODO:  handle this case better, I think we're expecting a symbol but we're getting some other token");
            // Err(Error::UnexpectedEndOfStream)
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

    fn statement(&mut self, in_block: bool) -> Result<Statement, Error> {
        // match print statements (which is a temporary construct until we add functions)
        if matches!(
            self.current_token(),
            Some(Token::Identifier(IdentifierToken { name, .. })) if name == "print"
        ) {
            self.advance();
            return self.print_statement(in_block);
        }

        self.expression_statement(in_block)
    }

    fn print_statement(&mut self, in_block: bool) -> Result<Statement, Error> {
        let value = self.expression()?;
        self.require_end_of_statement_or_expression(in_block)?;

        Ok(Statement::Print(value))
    }

    fn expression_statement(&mut self, in_block: bool) -> Result<Statement, Error> {
        let expr = self.expression()?;

        // TODO: having to add this logic is pretty ugly. What this method currently does is basically decide
        //       whether a semicolon is required to continue parsing. If a block just ended we done have to require
        //       a semicolon
        if let Expression::BlockExpression { .. } = expr {
        } else {
            self.require_end_of_statement_or_expression(in_block)?;
        }

        Ok(Statement::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, Error> {
        if self.consume_symbol_if(&[Symbol::LeftCurlyBracket]) {
            return self.block();
        }

        self.variable_declaration_or_assignment()
    }

    fn block(&mut self) -> Result<Expression, Error> {
        let mut statements = Vec::new();
        // let mut expression = Expression::Literal(Literal::Unit);

        loop {
            if self.consume_symbol_if(&[Symbol::RightCurlyBracket]) {
                break;
            }

            if self.current_token().is_some() {
                statements.push(self.statement(true)?);
            } else {
                return Err(Error::UnexpectedEndOfStream {
                    help_text: String::from(
                        "Unexpected end of stream while parsing a block, expected '}'",
                    ),
                });
            }
        }

        Ok(Expression::BlockExpression { statements })
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
                    value: Box::new(self.expression()?),
                })
            } else if self.consume_operator_if(&[Operator::EqualsSign]) {
                Ok(Expression::VariableAssignment {
                    token,
                    value: Box::new(self.expression()?),
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
                operator: Operator::CreateVar | Operator::EqualsSign,
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
                // TODO: this error might not be the best way to describe what's happening here
                //       figure out if there is a better way to handle errors here.
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
        expected_symbols: Vec<Symbol>,
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
                expected_symbols,
            } => write!(
                f,
                "Unexpected token '{:?}' expected symbol {} on {}",
                actual_token,
                symbols_to_string(expected_symbols),
                actual_token.position()
            ),
            Error::InvalidAssignmentTarget { target } => write!(f, "Invalid variable declaration or assignment. Cannot assign a value to expression: {target:?}")
        }
    }
}

fn symbols_to_string(symbols: &[Symbol]) -> String {
    let mut buf = String::new();
    for sym in symbols {
        write!(buf, "{sym}").expect("serializing symbols must succeed");
    }
    buf
}
