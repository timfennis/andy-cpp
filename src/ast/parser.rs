use std::fmt::Write;
use std::rc::Rc;

use either::Either;

use crate::ast::expression::{ExpressionLocation, ForBody, ForIteration, Lvalue};
use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::ast::Expression;
use crate::lexer::{Location, Token, TokenLocation};

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
            let operator: BinaryOperator = token_location.clone().try_into().unwrap_or_else(|_| {
                panic!(
                    "cannot convert '{}' into a binary operator",
                    token_location.token
                )
            });
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
        let maybe_lvalue = self.maybe_tuple(Self::single_expression)?;
        let start = maybe_lvalue.start;

        if !Lvalue::can_build_from_expression(&maybe_lvalue.expression) {
            // In this case we got some kind of expression that we can't assign to. We can just return the expression as is.
            // But to improve error handling and stuff it would be nice if we could check if the next token matches one
            // of the assignment operator and throw an appropriate error.
            return match self.peek_current_token() {
                Some(Token::DeclareVar | Token::EqualsSign) => {
                    Err(Error::InvalidAssignmentTarget {
                        target: maybe_lvalue,
                    })
                }
                _ => Ok(maybe_lvalue),
            };
        }

        return match self.peek_current_token() {
            // NOTE: the parser supports every LValue but some might cause an error when declaring vars
            Some(Token::DeclareVar) => {
                self.advance();
                let expression = self.maybe_tuple(Self::single_expression)?;
                let end = expression.end;
                let declaration = Expression::VariableDeclaration {
                    l_value: Lvalue::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an lvalue"),
                    value: Box::new(expression),
                };

                Ok(declaration.to_location(start, end))
            }
            Some(Token::EqualsSign) => {
                self.advance();
                let expression = self.maybe_tuple(Self::single_expression)?;
                let end = expression.end;
                let assignment_expression = Expression::Assignment {
                    l_value: Lvalue::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an lvalue"),
                    value: Box::new(expression),
                };

                Ok(assignment_expression.to_location(start, end))
            }
            Some(Token::OpAssign(inner)) => {
                let thing = match &inner.token {
                    Token::Identifier(identifier) => Either::Right(identifier.to_string()),
                    _ => Either::Left((*inner.clone()).try_into()?),
                };

                self.advance();
                let expression = self.maybe_tuple(Self::single_expression)?;
                let end = expression.end;
                let op_assign = Expression::OpAssignment {
                    l_value: Lvalue::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an lvalue"),
                    value: Box::new(expression),
                    operation: thing,
                };

                Ok(op_assign.to_location(start, end))
            }
            _ => Ok(maybe_lvalue),
        };
    }

    fn tuple_expressions(
        &mut self,
        next: fn(&mut Parser) -> Result<ExpressionLocation, Error>,
    ) -> Result<ExpressionLocation, Error> {
        let mut expressions = vec![next(self)?];
        while self.consume_token_if(&[Token::Comma]).is_some() {
            expressions.push(next(self)?);
        }

        let (start, end) = (
            expressions.first().unwrap().start,
            expressions.last().unwrap().end,
        );

        Ok(ExpressionLocation {
            expression: Expression::Tuple {
                values: expressions,
            },
            start,
            end,
        })
    }

    /// Parses a delimited tuple (enclosed in parentheses) that can be empty
    fn delimited_tuple(
        &mut self,
        next: fn(&mut Parser) -> Result<ExpressionLocation, Error>,
    ) -> Result<ExpressionLocation, Error> {
        let start = self.require_current_token_matches(Token::LeftParentheses)?;
        if let Some(end) = self.consume_token_if(&[Token::RightParentheses]) {
            Ok(Expression::Tuple { values: vec![] }.to_location(start.location, end.location))
        } else {
            let tuple_expression = self.tuple_expressions(next)?;
            self.require_current_token_matches(Token::RightParentheses)?;
            Ok(tuple_expression)
        }
    }

    fn maybe_tuple(
        &mut self,
        next: fn(&mut Parser) -> Result<ExpressionLocation, Error>,
    ) -> Result<ExpressionLocation, Error> {
        let tuple = self.tuple_expressions(next)?;
        Ok(tuple.maybe_extract_tuple())
    }

    fn single_expression(&mut self) -> Result<ExpressionLocation, Error> {
        self.range()
    }

    fn range(&mut self) -> Result<ExpressionLocation, Error> {
        let left = self.logic_or()?;
        if let Some(token) = self.consume_token_if(&[Token::DotDot, Token::DotDotEquals]) {
            let (start, end, right) = if self.peek_range_end() {
                (left.start, token.location, None)
            } else {
                let right = self.logic_or()?;
                (left.start, right.end, Some(Box::new(right)))
            };

            let expression = if token.token == Token::DotDot {
                Expression::RangeExclusive {
                    start: Some(Box::new(left)),
                    end: right,
                }
            } else {
                Expression::RangeInclusive {
                    start: Some(Box::new(left)),
                    end: right,
                }
            };

            Ok(ExpressionLocation {
                expression,
                start,
                end,
            })
        } else {
            Ok(left)
        }
    }
    fn logic_or(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_logical_expression_left_associative(Self::logic_and, &[Token::LogicOr])
    }

    fn logic_and(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_logical_expression_left_associative(Self::comparison, &[Token::LogicAnd])
    }

    fn comparison(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::boolean_or,
            &[
                Token::Equality,
                Token::Inequality,
                Token::Greater,
                Token::GreaterEquals,
                Token::Less,
                Token::LessEquals,
                Token::In,
            ],
        )
    }

    fn boolean_or(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::boolean_and, &[Token::Or])
    }

    fn boolean_and(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::term, &[Token::And])
    }

    fn term(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::factor,
            &[Token::Plus, Token::Minus, Token::Concat],
        )
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

        // This loop handles the following cases:
        //    * `identifier()` <-- call
        //    * `identifier[idx]` <-- idx
        //    * `identifier.ident()` <-- dot call
        while let Some(current) =
            self.match_token(&[Token::LeftParentheses, Token::LeftSquareBracket, Token::Dot])
        {
            match current.token {
                // handles: foo()
                Token::LeftParentheses => {
                    let Expression::Tuple { values: arguments } =
                        self.delimited_tuple(Self::single_expression)?.expression
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
                Token::Dot => {
                    self.require_current_token_matches(Token::Dot)?; // consume matched token
                    let l_value = self.require_identifier()?;
                    let (identifier_start, identifier_end) = (l_value.start, l_value.end);
                    let identifier = Lvalue::try_from(l_value)?;
                    let Lvalue::Variable { identifier } = identifier else {
                        unreachable!("Guaranteed to match by previous call to require_identifier")
                    };

                    let tuple_expression = self.delimited_tuple(Self::single_expression)?;
                    let Expression::Tuple {
                        values: mut arguments,
                    } = tuple_expression.expression
                    else {
                        unreachable!("self.tuple() must always produce a tuple");
                    };
                    arguments.insert(0, expr);

                    expr = ExpressionLocation {
                        expression: Expression::Call {
                            function: Box::new(
                                Expression::Identifier(identifier)
                                    .to_location(identifier_start, identifier_end),
                            ),
                            arguments,
                        },
                        start: identifier_start,
                        end: tuple_expression.end,
                    }

                    // for now, we require parentheses
                }
                Token::LeftSquareBracket => {
                    self.require_current_token_matches(Token::LeftSquareBracket)?;
                    // self.expression here allows for this syntax which is maybe a good idea
                    // `foo[1, 2] == foo[(1, 2)]`
                    // and
                    // `foo[x := 3]`
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

    fn list(&mut self, start: Location) -> Result<ExpressionLocation, Error> {
        let (values, end) =
            if let Some(bracket) = self.consume_token_if(&[Token::RightSquareBracket]) {
                (vec![], bracket.location)
            } else {
                let expr = self.tuple_expressions(Self::single_expression)?;

                match self.peek_current_token() {
                    Some(Token::RightSquareBracket) => {
                        self.advance();

                        let Expression::Tuple { values } = expr.expression else {
                            unreachable!("tuple_expression guarantees a tuple");
                        };

                        // Next we can maybe turn this into a list expression
                        let end = values.last().map_or(start, |e| e.end);
                        (values, end)
                        // DONE
                    }
                    // WOAH, this is not a list, it's a list comprehension
                    Some(Token::For) => {
                        let result = ForBody::Result(expr.maybe_extract_tuple());
                        let mut iterations = Vec::new();

                        loop {
                            iterations.push(self.for_iteration()?);

                            // If next is not a comma we break
                            if self.consume_token_if(&[Token::Comma]).is_none() {
                                break;
                            };
                        }

                        // TODO: continue parsing more
                        self.require_current_token_matches(Token::RightSquareBracket)?;

                        return Ok(Expression::For {
                            body: Box::new(result),
                            iterations,
                        }
                        .to_location(start, start)); // TODO fix loc
                    }
                    _ => panic!("KAPOT"),
                }
            };

        Ok(Expression::List { values }.to_location(start, end))
    }

    fn for_iteration(&mut self) -> Result<ForIteration, Error> {
        self.require_current_token_matches(Token::For)?;

        let l_value = Lvalue::try_from(self.maybe_tuple(Self::primary)?)?;

        self.require_current_token_matches(Token::In)?;

        let iteration = ForIteration::Iteration {
            l_value,
            sequence: self.single_expression()?,
        };

        Ok(iteration)
    }

    #[allow(clippy::too_many_lines)] // WERE BUILDING A PROGRAMMING LANGUAGE CLIPPY WHAT DO YOU WANT?
    fn primary(&mut self) -> Result<ExpressionLocation, Error> {
        // matches if expression like `if a < b { } else { }`
        if self.consume_token_if(&[Token::If]).is_some() {
            return self.if_expression();
        }
        // matches while loops like `while foo < bar { }`
        else if self.consume_token_if(&[Token::While]).is_some() {
            return self.while_expression();
        }
        // matches for loops like `for x in xs { }`
        else if self.match_token(&[Token::For]).is_some() {
            return self.for_expression();
        }
        // matches function declarations like `fn function_name(arg1, arg2) { }`
        else if self.consume_token_if(&[Token::Fn]).is_some() {
            return self.function_declaration();
        }
        // matches `return;` and `return (expression);`
        else if let Some(return_token_location) = self.consume_token_if(&[Token::Return]) {
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
        }
        // matches curly bracketed block expression `{ }`
        else if self.match_token(&[Token::LeftCurlyBracket]).is_some() {
            return self.block();
        }
        // matches map expression %{1,2,3}
        else if self.match_token(&[Token::MapOpen]).is_some() {
            return self.map_expression();
        }
        // matches either a grouped expression `(1+1)` or a tuple `(1,1)`
        else if let Some(start_parentheses) = self.consume_token_if(&[Token::LeftParentheses]) {
            // If an opening parentheses is immediately followed by a closing parentheses we're dealing with a Unit expression
            if let Some(end_parentheses) = self.consume_token_if(&[Token::RightParentheses]) {
                return Ok(Expression::UnitLiteral
                    .to_location(start_parentheses.location, end_parentheses.location));
            }

            let grouped = self.expression()?;

            self.require_current_token_matches(Token::RightParentheses)?;

            return Ok(grouped);
        }

        let token_location = self.require_current_token()?;

        let expression = match token_location.token {
            Token::False => Expression::BoolLiteral(false),
            Token::True => Expression::BoolLiteral(true),
            Token::Int64(num) => Expression::Int64Literal(num),
            Token::Float64(num) => Expression::Float64Literal(num),
            Token::BigInt(num) => Expression::BigIntLiteral(num),
            Token::Complex(num) => Expression::ComplexLiteral(num),
            Token::String(value) => Expression::StringLiteral(value),
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

    /// Parses if expression without the `if` token
    /// Example:
    /// ```ndc
    /// if foo == bar {
    ///
    /// } else if bar == baz {
    ///
    /// } else {
    ///
    /// }
    /// ```
    fn if_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let expression = self.expression()?;
        let on_true = self.block()?;

        let on_false = if self.consume_token_if(&[Token::Else]).is_some() {
            if self.consume_token_if(&[Token::If]).is_some() {
                let expression = self.if_expression()?;
                Some(Box::new(expression))
            } else {
                Some(Box::new(self.block()?))
            }
        } else {
            None
        };

        let (start, end) = (expression.start, expression.end);
        Ok(Expression::If {
            condition: Box::new(expression),
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
        let start = self
            .peek_current_token_location()
            .expect("required to be the correct token")
            .location;
        let iteration = self.for_iteration()?;
        let body = self.block()?;

        let end = body.end;
        Ok(Expression::For {
            iterations: vec![iteration],
            body: Box::new(ForBody::Body(body)),
        }
        .to_location(start, end))
    }

    fn require_identifier(&mut self) -> Result<ExpressionLocation, Error> {
        let identifier_expression = self.primary()?;

        if matches!(
            identifier_expression,
            ExpressionLocation {
                expression: Expression::Identifier(_),
                ..
            }
        ) {
            Ok(identifier_expression)
        } else {
            Err(Error::ExpectedIdentifier {
                actual: identifier_expression,
            })
        }
    }

    fn function_declaration(&mut self) -> Result<ExpressionLocation, Error> {
        let identifier = self.primary()?;
        let Expression::Identifier(_) = identifier.expression else {
            return Err(Error::ExpectedIdentifier { actual: identifier });
        };

        let argument_list = self.delimited_tuple(Self::single_expression)?;
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

    /// Parses a block expression including the block delimiters `{` and `}`
    /// example:
    /// ```ndc
    /// {
    ///     func();
    ///     x := 1 + 1;
    ///     x
    /// }
    /// ```
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

    fn map_expression(&mut self) -> Result<ExpressionLocation, Error> {
        // This should have been checked before this method is called;
        let start = self.require_token(&[Token::MapOpen])?;

        // Optional default value
        let default = if self.consume_token_if(&[Token::Colon]).is_some() {
            let default = self.single_expression()?;
            if self.match_token(&[Token::RightCurlyBracket]).is_some() {
                // If the list ends without any values we just do nothing and let the loop below handle the rest
            } else {
                // If there isn't a } to close the map there must be a comma otherwise we error out
                self.require_current_token_matches(Token::Comma)?;
            }
            Some(Box::new(default))
        } else {
            None
        };

        let mut values = Vec::new();

        let end = loop {
            // End parsing if we see a RightCurlyBracket, this one only happens if the expression is
            // empty `%{}` or if there is a trailing comma `%{1,2,3,}`
            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.location;
            }

            let key = self.single_expression()?;

            if self.consume_token_if(&[Token::Colon]).is_some() {
                let value = self.single_expression()?;
                values.push((key, Some(value)));
            } else {
                values.push((key, None));
            }

            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.location;
            }

            self.require_current_token_matches(Token::Comma)?;
        };
        Ok(Expression::Map { values, default }.to_location(start, end))
    }
    fn peek_range_end(&self) -> bool {
        matches!(
            self.peek_current_token(),
            Some(
                Token::Semicolon
                    | Token::Comma
                    | Token::RightCurlyBracket
                    | Token::RightParentheses
                    | Token::RightSquareBracket
            ) | None
        )
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

    #[error("unexpected token {} on {}", .actual_token.token, .actual_token.location)]
    UnexpectedToken { actual_token: TokenLocation },

    #[error("the following expression cannot be used as an lvalue: {0:?}")]
    InvalidLvalue(Expression),
}

fn tokens_to_string(tokens: &[Token]) -> String {
    let mut buf = String::new();
    let mut iter = tokens.iter().peekable();
    while let Some(token) = iter.next() {
        if iter.peek().is_none() {
            write!(buf, "{token}").expect("serializing symbols must succeed");
        } else {
            write!(buf, "{token}, ").expect("serializing symbols must succeed");
        }
    }
    buf
}
