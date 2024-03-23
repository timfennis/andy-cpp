use crate::ast::expression::{ExpressionLocation, Lvalue};
use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::ast::Expression;
use crate::lexer::{Location, Token, TokenLocation};
use std::fmt::Write;
use std::rc::Rc;

pub struct Parser {
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
                Expression::Block { .. }
                    | Expression::Statement(_)
                    | Expression::If { .. }
                    | Expression::While { .. }
                    | Expression::For { .. }
                    | Expression::FunctionDeclaration { .. }
            )
        };
        let mut expressions = Vec::new();
        while self.peek_current_token_location().is_some() {
            let expr_loc = self.expression_or_statement()?;
            let is_statement = is_valid_statement(&expr_loc.expression);

            expressions.push(expr_loc);

            if !is_statement {
                break;
            }
        }

        // After consuming all statements if there are any tokens remaining we can emit an error that we expected a semicolon
        if let Some(token) = self.peek_current_token_location() {
            return Err(Error::ExpectedToken {
                expected_tokens: vec![Token::Semicolon],
                actual_token: token.clone(),
            });
        }

        Ok(expressions)
    }

    fn peek_current_token(&self) -> Option<&Token> {
        self.peek_current_token_location().map(|t| &t.token)
    }

    fn peek_current_token_location(&self) -> Option<&TokenLocation> {
        self.tokens.get(self.current)
    }

    /// Returns the current `TokenLocation` if it matches the given `Token` or returns `None` if it does not match.
    /// It does not consume the current token however, if that's the purpose you can use `consume_token_if`.
    fn match_token(&self, tokens: &[Token]) -> Option<&TokenLocation> {
        for search_token in tokens {
            if self.peek_current_token() == Some(search_token) {
                return self.peek_current_token_location();
            }
        }
        None
    }

    fn require_token(&mut self, tokens: &[Token]) -> Result<Location, Error> {
        // If the current token matches the expected location we advance and return the matched location
        if let Some(token) = self.match_token(tokens) {
            let location = token.location;
            self.advance();
            return Ok(location);
        }

        if let Some(current_token) = self.peek_current_token_location() {
            Err(Error::ExpectedToken {
                expected_tokens: Vec::from(tokens),
                actual_token: current_token.clone(),
            })
        } else {
            Err(Error::UnexpectedEndOfStream {
                help_text: String::new(),
            })
        }
        // if self.current_token_location().is_some() {
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
                actual_token: token_location,
            })
        }
    }

    // Requires that there is a valid current token, otherwise it returns Err(ParserError)
    fn require_current_token(&mut self) -> Result<TokenLocation, Error> {
        let token = self
            .peek_current_token_location()
            .ok_or_else(|| Error::UnexpectedEndOfStream { help_text: String::from("a token was required but an end of stream was found instead (require_current_token)") })?
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
            let operator: BinaryOperator = token_location
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

    fn consume_binary_expression_right_associative(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
        current: fn(&mut Self) -> Result<ExpressionLocation, Error>,
        valid_tokens: &[Token],
    ) -> Result<ExpressionLocation, Error> {
        let left = next(self)?;

        if let Some(token_location) = self.consume_token_if(valid_tokens) {
            let operator = BinaryOperator::try_from(token_location)
                .expect("COMPILER ERROR: consume_token_if must guarantee the correct token");
            let right = current(self)?;

            let (start, end) = (left.start, right.end);
            return Ok(Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .to_location(start, end));
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

    // ---------------------------------------- Recursive Descent Parser ----------------------------------------

    fn expression_or_statement(&mut self) -> Result<ExpressionLocation, Error> {
        let mut expression = self.variable_declaration_or_assignment()?;

        if self.match_token(&[Token::Semicolon]).is_some() {
            self.advance();
            expression = expression.to_statement();
        }

        Ok(expression)
    }

    fn expression(&mut self) -> Result<ExpressionLocation, Error> {
        self.variable_declaration_or_assignment()
    }

    fn variable_declaration_or_assignment(&mut self) -> Result<ExpressionLocation, Error> {
        // NOTE: I think the way we implemented this method makes it right associative
        // NOTE: Instead of just parsing an identifier we continue to recurse down to the next level `logic_or` at the
        //       time of writing. If whatever comes back is a valid identifier (possibly lvalue in the future) we treat
        //       this expression as an assignment expression. Otherwise, we just treat the expression as whatever we got.
        // NOTE: When we start supporting more lvalues we should insert this step between
        //       `variable_declaration_or_assignment` and `logic_or`
        let maybe_lvalue = self.logic_or()?;

        // If the token we parsed is some kind of variable we can potentially assign to it. Next we'll check if the next
        // token matches either the declaration operator or the assignment operator. If neither of those matches we are
        // just returning the variable expression as is.
        match maybe_lvalue.expression {
            Expression::Identifier(ref identifier) => {
                if self.consume_token_if(&[Token::CreateVar]).is_some() {
                    let expression = self.expression()?;
                    let (start, end) = (maybe_lvalue.start, expression.end);
                    Ok(Expression::VariableDeclaration {
                        l_value: Lvalue::Variable {
                            identifier: identifier.to_string(),
                        },
                        value: Box::new(expression),
                    }
                    .to_location(start, end))
                } else if self.consume_token_if(&[Token::EqualsSign]).is_some() {
                    let expression = self.expression()?;
                    let (start, end) = (maybe_lvalue.start, expression.end);
                    Ok(Expression::Assignment {
                        l_value: Lvalue::Variable {
                            identifier: identifier.to_string(),
                        },
                        value: Box::new(expression),
                    }
                    .to_location(start, end))
                } else {
                    Ok(maybe_lvalue)
                }
            }
            Expression::Index { value, index } => {
                if self.consume_token_if(&[Token::CreateVar]).is_some() {
                    todo!("TODO: err: can't assign to index expression");
                } else if self.consume_token_if(&[Token::EqualsSign]).is_some() {
                    let expression = self.expression()?;
                    let (start, end) = (value.start, expression.end);
                    Ok(Expression::Assignment {
                        l_value: Lvalue::Index { value, index },
                        value: Box::new(expression),
                    }
                    .to_location(start, end))
                } else {
                    Ok(Expression::Index { index, value }
                        .to_location(maybe_lvalue.start, maybe_lvalue.end))
                }
            }
            _ => {
                // In this case we got some kind of expression that we can't assign to. We can just return the expression as is.
                // But to improve error handling and stuff it would be nice if we could check if the next token matches one
                // of the assignment operator and throw an appropriate error.
                match self.peek_current_token() {
                    Some(Token::CreateVar | Token::EqualsSign) => {
                        Err(Error::InvalidAssignmentTarget {
                            target: maybe_lvalue,
                        })
                    }
                    _ => Ok(maybe_lvalue),
                }
            }
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
        self.consume_binary_expression_right_associative(
            Self::unary,
            Self::exponent,
            &[Token::Exponent],
        )
    }

    fn unary(&mut self) -> Result<ExpressionLocation, Error> {
        if let Some(token) = self.consume_token_if(&[Token::Bang, Token::Minus]) {
            let operator: UnaryOperator = token
                .try_into()
                .expect("consume_operator_if guaranteed us that the next token is an Operator");

            let right = self.unary()?;
            let (start, end) = (right.start, right.end);

            Ok(Expression::Unary {
                operator,
                expression: Box::new(right),
            }
            .to_location(start, end))
        } else {
            self.operand()
        }
    }

    fn operand(&mut self) -> Result<ExpressionLocation, Error> {
        let mut expr = self.primary()?;

        while let Some(current) =
            self.consume_token_if(&[Token::LeftParentheses, Token::LeftSquareBracket])
        {
            match current.token {
                Token::LeftParentheses => {
                    let Expression::Tuple { values: arguments } =
                        self.tuple(expr.start)?.expression
                    else {
                        unreachable!("self.tuple() must always produce a tuple");
                    };

                    let (start, end) = (expr.start, expr.end);

                    expr = ExpressionLocation {
                        expression: Expression::Call {
                            function: Box::new(expr),
                            arguments,
                        },
                        start,
                        end,
                    };
                }
                Token::LeftSquareBracket => {
                    let index_expression = self.expression()?;
                    let end_token =
                        self.require_current_token_matches(Token::RightSquareBracket)?;

                    let (start, end) = (expr.start, end_token.location);

                    expr = ExpressionLocation {
                        expression: Expression::Index {
                            value: Box::new(expr),
                            index: Box::new(index_expression),
                        },
                        start,
                        end,
                    };
                }
                _ => unreachable!("guaranteed to match"),
            }
        }

        Ok(expr)
    }

    fn group_of_expressions(
        &mut self,
        terminated_by: Token,
    ) -> Result<Vec<ExpressionLocation>, Error> {
        let mut values = Vec::new();

        loop {
            let current_token = self.peek_current_token_location().cloned();
            if let Some(ref token_location) = current_token {
                if token_location.token == terminated_by {
                    self.advance();
                    return Ok(values);
                }

                values.push(self.expression()?);

                // After we parse an expression we look ahead and see if the next token is either a ',' or ')'
                // if it's not we can return a parse error
                let current_token = self.peek_current_token_location();
                match current_token.map(|it| &it.token) {
                    Some(token) if token == &terminated_by => {
                        // Termination token is handled by the next iteration
                    }
                    Some(Token::Comma) => {
                        self.advance();
                    }
                    Some(_) => {
                        return Err(Error::ExpectedToken {
                            expected_tokens: vec![Token::Comma, terminated_by],
                            actual_token: current_token.unwrap().clone(),
                        });
                    }
                    _ => {
                        // None case is handled by just looping again
                    }
                }
            } else {
                return Err(Error::UnexpectedEndOfStream {
                    help_text: "unexpected end of stream while parsing method call".to_string(),
                });
            }
        }
    }
    fn tuple(&mut self, start: Location) -> Result<ExpressionLocation, Error> {
        let values = self.group_of_expressions(Token::RightParentheses)?;
        Ok(Expression::Tuple { values }.to_location(start, start)) // TODO: find end
    }

    fn list(&mut self, start: Location) -> Result<ExpressionLocation, Error> {
        let values = self.group_of_expressions(Token::RightSquareBracket)?;
        Ok(Expression::List { values }.to_location(start, start)) // TODO: find end
    }

    fn primary(&mut self) -> Result<ExpressionLocation, Error> {
        if self.consume_token_if(&[Token::If]).is_some() {
            return self.if_expression();
        } else if self.consume_token_if(&[Token::While]).is_some() {
            return self.while_expression();
        } else if self.consume_token_if(&[Token::For]).is_some() {
            return self.for_expression();
        } else if self.consume_token_if(&[Token::Fn]).is_some() {
            return self.function_declaration();
        } else if let Some(return_token_location) = self.consume_token_if(&[Token::Return]) {
            let expr_loc = if self.match_token(&[Token::Semicolon]).is_some() {
                Expression::UnitLiteral.to_location(
                    return_token_location.location,
                    return_token_location.location,
                )
            } else {
                self.expression()?
            };

            let end = expr_loc.end;

            let return_expression = Expression::Return {
                value: Box::new(expr_loc),
            }
            .to_location(return_token_location.location, end);

            return Ok(return_expression);
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
            Token::Complex(num) => Expression::ComplexLiteral(num),
            Token::String(value) => Expression::StringLiteral(Rc::new(value)),
            Token::LeftParentheses => {
                let expr = self.expression()?;
                self.require_current_token_matches(Token::RightParentheses)?;
                Expression::Grouping(Box::new(expr))
            }
            Token::LeftSquareBracket => return self.list(token_location.location),
            Token::Identifier(identifier) => Expression::Identifier(identifier),
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
        Ok(Expression::If {
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
        Ok(Expression::While {
            expression: Box::new(expression),
            loop_body: Box::new(loop_body),
        }
        .to_location(start, end))
    }

    fn for_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let (l_value, l_value_start, _l_value_end) = self.require_lvalue()?;
        self.require_current_token_matches(Token::In)?;
        let sequence = self.expression()?;
        let body = self.block()?;

        let end = body.end;
        Ok(Expression::For {
            l_value,
            sequence: Box::new(sequence),
            loop_body: Box::new(body),
        }
        .to_location(l_value_start, end))
    }

    fn require_identifier(&mut self) -> Result<(String, Location, Location), Error> {
        let identifier_expression = self.primary()?;

        match identifier_expression.expression {
            Expression::Identifier(identifier) => Ok((
                identifier,
                identifier_expression.start,
                identifier_expression.end,
            )),
            _ => Err(Error::ExpectedIdentifier {
                actual: identifier_expression,
            }),
        }
    }

    fn require_lvalue(&mut self) -> Result<(Lvalue, Location, Location), Error> {
        let (identifier, start, end) = self.require_identifier()?;
        Ok((Lvalue::Variable { identifier }, start, end))
    }

    fn function_declaration(&mut self) -> Result<ExpressionLocation, Error> {
        let identifier = self.primary()?;
        let Expression::Identifier(_) = identifier.expression else {
            return Err(Error::ExpectedIdentifier { actual: identifier });
        };

        let arg_start = self.require_token(&[Token::LeftParentheses])?;
        let argument_list = self.tuple(arg_start)?;
        let body = self.block()?;

        let (start, end) = (identifier.start, body.end);
        Ok(ExpressionLocation {
            expression: Expression::FunctionDeclaration {
                name: Box::new(identifier),
                arguments: Box::new(argument_list),
                body: Rc::new(body),
            },
            start,
            end,
        })
    }

    fn block(&mut self) -> Result<ExpressionLocation, Error> {
        let start = self.require_token(&[Token::LeftCurlyBracket])?;

        let mut statements = Vec::new();

        let end = loop {
            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.location;
            }

            if self.peek_current_token_location().is_some() {
                statements.push(self.expression_or_statement()?);
            } else {
                return Err(Error::UnexpectedEndOfStream {
                    help_text: String::from(
                        "Unexpected end of stream while parsing a block, expected '}'",
                    ),
                });
            }
        };

        Ok(Expression::Block { statements }.to_location(start, end))
    }
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error {
    #[error("unexpected end of stream: {help_text}")]
    UnexpectedEndOfStream { help_text: String },

    #[error("unexpected token '{}' expected expression on {}", .actual_token.token, .actual_token.location)]
    ExpectedExpression { actual_token: TokenLocation },

    #[error("expected identifier got '{:?}' on {}", .actual, .actual.start)]
    ExpectedIdentifier { actual: ExpressionLocation },

    #[error("unexpected token '{}' expected symbol '{}' on {}", .actual_token.token, tokens_to_string(.expected_tokens), .actual_token.location)]
    ExpectedToken {
        actual_token: TokenLocation,
        expected_tokens: Vec<Token>,
    },

    #[error("invalid variable declaration or assignment. Cannot assign a value to expression: {target:?}")]
    InvalidAssignmentTarget { target: ExpressionLocation },
}

fn tokens_to_string(tokens: &[Token]) -> String {
    let mut buf = String::new();
    for sym in tokens {
        write!(buf, "{sym}").expect("serializing symbols must succeed");
    }
    buf
}
