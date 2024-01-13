use crate::ast::expression::{
    ExpressionLocation, LogicalOperator, Lvalue, Operator, UnaryOperator,
};
use crate::ast::Expression;
use crate::lexer::{Token, TokenLocation};
use std::fmt;
use std::fmt::{Formatter, Write};
use std::rc::Rc;

pub(crate) struct Parser {
    tokens: Vec<TokenLocation>,
    current: usize,
}

impl Parser {
    pub(crate) fn from_tokens(tokens: Vec<TokenLocation>) -> Self {
        Self { tokens, current: 0 }
    }
    /// Parse a Vec of Tokens into a Vec of statements
    /// # Errors
    /// If the parsing fails which it could do for many different reasons it will return an [Error]. See the variants of
    /// this type for more information about the different kinds of errors.
    pub(crate) fn parse(&mut self) -> Result<Vec<ExpressionLocation>, Error> {
        let is_valid_statement = |expr: &Expression| -> bool {
            matches!(
                expr,
                Expression::BlockExpression { .. }
                    | Expression::Statement(_)
                    | Expression::IfExpression { .. }
                    | Expression::WhileExpression { .. }
            )
        };
        let mut v = Vec::new();
        while self.current_token().is_some() {
            let expr_loc = self.expression_or_statement()?;

            if is_valid_statement(&expr_loc.expression) {
                // we can continue safely
                v.push(expr_loc);
            } else {
                v.push(expr_loc);
                break;
            }
            // Right now we just break when we se any error but maybe we should handle this differently
        }

        if let Some(token) = self.current_token() {
            return Err(Error::ExpectedToken {
                expected_tokens: vec![Token::Semicolon],
                actual_token: token.clone(),
            });
        }

        Ok(v)
    }

    fn current_token(&self) -> Option<&TokenLocation> {
        self.tokens.get(self.current)
    }

    fn previous_token(&self) -> Option<&TokenLocation> {
        self.tokens.get(self.current - 1)
    }

    fn match_token(&self, tokens: &[Token]) -> Option<&TokenLocation> {
        for search_token in tokens {
            if self.current_token().map(|it| &it.token) == Some(search_token) {
                return self.current_token();
            }
        }
        None
    }

    fn require_token(&mut self, tokens: &[Token]) -> Result<TokenLocation, Error> {
        if let Some(token) = self.match_token(tokens).cloned() {
            self.advance();
            return Ok(token);
        }

        if self.current_token().is_some() {
            Err(Error::ExpectedToken {
                expected_tokens: Vec::from(tokens),
                actual_token: self
                    .current_token()
                    .cloned()
                    .expect("Guaranteed to be some"),
            })
        } else {
            Err(Error::UnexpectedEndOfStream {
                help_text: String::new(),
            })
        }
    }

    fn consume_token_if(&mut self, tokens: &[Token]) -> Option<TokenLocation> {
        if let Some(token) = self.match_token(tokens) {
            let token = token.clone();
            self.advance();
            Some(token)
        } else {
            None
        }
    }

    /// Requires that the current token matches a type, if it doesn't an error is thrown.
    /// If the token does match the expected type we advance the iterator
    fn require_current_token_matches(
        &mut self,
        match_token: Token,
    ) -> Result<TokenLocation, Error> {
        let token_location = self.require_current_token()?;

        if match_token == token_location.token {
            Ok(token_location)
        } else {
            Err(Error::ExpectedToken {
                expected_tokens: vec![match_token],
                actual_token: token_location.clone(),
            })
        }
    }

    // Requires that there is a valid current token, otherwise it returns Err(ParserError)
    fn require_current_token(&mut self) -> Result<TokenLocation, Error> {
        let token = self
            .current_token()
            .ok_or_else(|| Error::UnexpectedEndOfStream { help_text: String::from("a token was required but an end of stream was found instead (require_current_token)")})?
            .clone();
        self.advance();
        Ok(token)
    }

    fn consume_binary_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
        valid_tokens: &[Token],
    ) -> Result<ExpressionLocation, Error> {
        let mut left = next(self)?;
        while let Some(token_location) = self.consume_token_if(valid_tokens) {
            let operator: Operator = token_location
                .clone()
                .try_into()
                .expect("consume_operator_if guaranteed us that this is an operator");
            let right = next(self)?;
            let start = left.start;
            let end = right.end;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .to_location(start, end);
        }
        Ok(left)
    }

    fn consume_logical_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
        valid_tokens: &[Token],
    ) -> Result<ExpressionLocation, Error> {
        let mut left = next(self)?;
        while let Some(token_location) = self.consume_token_if(valid_tokens) {
            let operator: LogicalOperator = token_location
                .clone()
                .try_into()
                .expect("consume_operator_if guaranteed us that this is an operator");
            let right = next(self)?;
            let start = left.start;
            let end = right.end;
            left = Expression::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .to_location(start, end);
        }
        Ok(left)
    }

    fn advance(&mut self) {
        if self.current < self.tokens.len() {
            self.current += 1;
        }
    }

    /************************************************* PARSER *************************************************/

    fn expression_or_statement(&mut self) -> Result<ExpressionLocation, Error> {
        // match print statements (which is a temporary construct until we add functions)
        let mut expression = if matches!(
            self.current_token().map(|it| &it.token),
            Some(Token::Identifier(name)) if name == "print"
        ) {
            self.advance();
            self.print_expression()?
        } else {
            self.variable_declaration_or_assignment()?
        };

        if self.match_token(&[Token::Semicolon]).is_some() {
            self.advance();
            expression = expression.to_statement();
        }

        Ok(expression)
    }

    fn expression(&mut self) -> Result<ExpressionLocation, Error> {
        self.variable_declaration_or_assignment()
    }
    fn print_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let value = self.expression()?;
        let (start, end) = (value.start, value.end);
        Ok(Expression::Print(Box::new(value)).to_location(start, end))
    }

    fn variable_declaration_or_assignment(&mut self) -> Result<ExpressionLocation, Error> {
        // NOTE: I think the way we implemented this method makes it right associative
        // self.equality() is the next order of precedence at this point
        let maybe_identifier = self.logic_or()?;

        // If the token we parsed is some kind of variable we can potentially assign to it. Next we'll check if the next
        // token matches either the declaration operator or the assignment operator. If neither of those matches we are
        // just returning the variable expression as is.
        if let Expression::Variable { identifier } = maybe_identifier.expression {
            return if self.consume_token_if(&[Token::CreateVar]).is_some() {
                let expression = self.expression()?;
                let (start, end) = (maybe_identifier.start, expression.end);
                Ok(Expression::VariableDeclaration {
                    l_value: Lvalue::Variable { identifier },
                    value: Box::new(expression),
                }
                .to_location(start, end))
            } else if self.consume_token_if(&[Token::EqualsSign]).is_some() {
                let expression = self.expression()?;
                let (start, end) = (maybe_identifier.start, expression.end);
                Ok(Expression::VariableAssignment {
                    l_value: Lvalue::Variable { identifier },
                    value: Box::new(expression),
                }
                .to_location(start, end))
            } else {
                // Does this m ake any sense???????
                Ok(Expression::Variable { identifier }
                    .to_location(maybe_identifier.start, maybe_identifier.end))
            };
        }

        // In this case we got some kind of expression that we can't assign to. We can just return the expression as is.
        // But to improve error handling and stuff it would be nice if we could check if the next token matches one
        // of the assignment operator and throw an appropriate error.
        match self.current_token().map(|it| &it.token) {
            Some(Token::CreateVar | Token::EqualsSign) => Err(Error::InvalidAssignmentTarget {
                target: maybe_identifier,
            }),
            _ => Ok(maybe_identifier),
        }
    }

    fn logic_or(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_logical_expression_left_associative(Self::logic_and, &[Token::LogicOr])
    }

    fn logic_and(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_logical_expression_left_associative(Self::equality, &[Token::LogicAnd])
    }

    fn equality(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::comparison,
            &[Token::Equality, Token::Inequality],
        )
    }

    fn comparison(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::term,
            &[
                Token::Greater,
                Token::GreaterEquals,
                Token::Less,
                Token::LessEquals,
            ],
        )
    }

    fn term(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::factor, &[Token::Plus, Token::Minus])
    }

    fn factor(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::exponent,
            &[
                Token::Divide,
                Token::Multiply,
                Token::CModulo,
                Token::EuclideanModulo,
            ],
        )
    }

    fn exponent(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::unary, &[Token::Exponent])
    }

    fn unary(&mut self) -> Result<ExpressionLocation, Error> {
        if self
            .consume_token_if(&[Token::Bang, Token::Minus])
            .is_some()
        {
            let operator: UnaryOperator = self
                .previous_token()
                .expect("we're guaranteed to have an operator token by the previous call to self.consume_operator_if")
                .clone()
                .try_into()
                .expect("consume_operator_if guaranteed us that the next token is an Operator");

            let right = self.unary()?;
            let (start, end) = (right.start, right.end);
            return Ok(Expression::Unary {
                operator,
                expression: Box::new(right),
            }
            .to_location(start, end));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<ExpressionLocation, Error> {
        if self.consume_token_if(&[Token::If]).is_some() {
            return self.if_expression();
        } else if self.consume_token_if(&[Token::While]).is_some() {
            return self.while_expression();
        } else if self.match_token(&[Token::LeftCurlyBracket]).is_some() {
            return self.block();
        }

        let token_location = self.require_current_token()?;

        let expression = match token_location.token {
            Token::False => Expression::BoolLiteral(false),
            Token::True => Expression::BoolLiteral(true),
            Token::Int64(num) => Expression::Int64Literal(num),
            Token::Float64(num) => Expression::Float64Literal(num),
            Token::BigInt(num) => Expression::BigIntLiteral(num),
            Token::String(value) => Expression::StringLiteral(Rc::new(value)),
            Token::LeftParentheses => {
                let expr = self.expression()?;
                self.require_current_token_matches(Token::RightParentheses)?;
                Expression::Grouping(Box::new(expr))
            }
            Token::Identifier(identifier) => Expression::Variable { identifier },
            _ => {
                // TODO: this error might not be the best way to describe what's happening here
                //       figure out if there is a better way to handle errors here.
                return Err(Error::ExpectedExpression {
                    actual_token: token_location,
                });
            }
        };

        Ok(expression.to_location(token_location.location, token_location.location))
    }

    //TODO: support } else if x { type structures
    fn if_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let expression = self.expression()?;
        let on_true = self.block()?;

        let on_false = if self.consume_token_if(&[Token::Else]).is_some() {
            Some(Box::new(self.block()?))
        } else {
            None
        };

        let (start, end) = (expression.start, expression.end);
        Ok(Expression::IfExpression {
            expression: Box::new(expression),
            on_true: Box::new(on_true),
            on_false,
        }
        .to_location(start, end))
    }

    fn while_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let expression = self.expression()?;
        let loop_body = self.block()?;

        let (start, end) = (expression.start, expression.end);
        Ok(Expression::WhileExpression {
            expression: Box::new(expression),
            loop_body: Box::new(loop_body),
        }
        .to_location(start, end))
    }

    fn block(&mut self) -> Result<ExpressionLocation, Error> {
        let start = self.require_token(&[Token::LeftCurlyBracket])?.location;

        let mut statements = Vec::new();
        // let mut expression = Expression::Literal(Value::Unit);

        let end = loop {
            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.location;
            }

            if self.current_token().is_some() {
                statements.push(self.expression_or_statement()?);
            } else {
                return Err(Error::UnexpectedEndOfStream {
                    help_text: String::from(
                        "Unexpected end of stream while parsing a block, expected '}'",
                    ),
                });
            }
        };

        Ok(Expression::BlockExpression { statements }.to_location(start, end))
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedEndOfStream {
        help_text: String,
    },
    ExpectedExpression {
        actual_token: TokenLocation,
    },
    ExpectedIdentifier {
        actual_token: TokenLocation,
    },
    ExpectedToken {
        actual_token: TokenLocation,
        expected_tokens: Vec<Token>,
    },
    InvalidAssignmentTarget {
        target: ExpressionLocation,
    },
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedEndOfStream { help_text } =>
                write!(f, "Unexpected end of stream: {help_text}"),
            Error::ExpectedExpression {
                actual_token: token,
            } => write!(
                f,
                "Unexpected token '{}' expected expression on {}",
                token,
                token.location
            ),
            Error::ExpectedIdentifier {
                actual_token,
            } => write!(
                f,
                "Unexpected token '{}' expected identifier on {}",
                actual_token.token,
                actual_token.location
            ),
            Error::ExpectedToken {
                actual_token,
                expected_tokens: expected_symbols,
            } =>  write!(
                f,
                "Unexpected token '{}' expected symbol {} on {}",
                actual_token.token,
                tokens_to_string(expected_symbols),
                actual_token.location
            ),
            Error::InvalidAssignmentTarget { target } => write!(f, "Invalid variable declaration or assignment. Cannot assign a value to expression: {target:?}")
        }
    }
}

fn tokens_to_string(tokens: &[Token]) -> String {
    let mut buf = String::new();
    for sym in tokens {
        write!(buf, "{sym}").expect("serializing symbols must succeed");
    }
    buf
}
