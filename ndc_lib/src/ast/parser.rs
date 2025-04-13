use std::fmt::Write;
use std::rc::Rc;

use either::Either;
use miette::Diagnostic;

use crate::ast::expression::{ExpressionLocation, ForBody, ForIteration, Lvalue};
use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::ast::Expression;
use crate::lexer::{Span, Token, TokenLocation};

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
            return Err(Error::text(
                format!("Expected semicolon but got {:?} instead", token.token),
                token.span,
            ));
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

    fn require_token(&mut self, tokens: &[Token]) -> Result<Span, Error> {
        // If the current token matches the expected location we advance and return the matched location
        if let Some(token) = self.match_token(tokens) {
            let location = token.span;
            self.advance();
            return Ok(location);
        }

        if let Some(current_token) = self.peek_current_token_location() {
            Err(Error::text(
                format!(
                    "expected one of {} but got {} instead",
                    tokens_to_string(tokens),
                    current_token.token
                ),
                current_token.span,
            ))
        } else {
            Err(Error::end_of_input(
                self.tokens.last().expect("last token exists").span,
            ))
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
        match_token: &Token,
    ) -> Result<TokenLocation, Error> {
        let token_location = self.require_current_token()?;

        if match_token == &token_location.token {
            Ok(token_location)
        } else {
            Err(Error::text(
                format!(
                    "Expected token '{}' but got '{}' instead",
                    match_token, token_location.token
                ),
                token_location.span,
            ))
        }
    }

    // Requires that there is a valid current token, otherwise it returns Err(ParserError)
    fn require_current_token(&mut self) -> Result<TokenLocation, Error> {
        let token = self
            .peek_current_token_location()
            .ok_or_else(|| {
                Error::end_of_input(self.tokens.last().expect("last token exists").span)
            })?
            .clone();
        self.advance();
        Ok(token)
    }

    /// Creates a tree of binary expression in a left associative way.
    /// Optionally you can set the `augment_not` boolean to true to allow token augmentation
    /// for now this is hardcoded to a single keyword and operation, but we could extend this in the
    /// future.
    ///
    /// The most important reason this exists is for `x not in y` to translate to `not x in y`, but
    /// it also works for expression like `x not == 5` which is a debatable feature.
    fn consume_binary_expression_left_associative(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
        valid_tokens: &[Token],
        augment_not: bool,
    ) -> Result<ExpressionLocation, Error> {
        let mut left = next(self)?;

        let mut extended_valid_tokens = valid_tokens.to_vec();
        if augment_not {
            extended_valid_tokens.push(Token::LogicNot);
        }

        while let Some(token_location) = self.consume_token_if(&extended_valid_tokens) {
            let (invert, token_location) = if token_location.token == Token::LogicNot {
                let augmented = self.consume_token_if(valid_tokens).ok_or_else(|| {
                    Error::text(
                        format!(
                            "unexpected token {}",
                            self.require_current_token()
                                .expect("there has to be a token")
                                .token
                        ),
                        self.require_current_token()
                            .expect("must have current token")
                            .span,
                    )
                })?;
                (Some(token_location), augmented)
            } else {
                (None, token_location)
            };

            let operator: BinaryOperator = token_location.clone().try_into().unwrap_or_else(|_| {
                panic!(
                    "cannot convert '{}' into a binary operator",
                    token_location.token
                )
            });
            let right = next(self)?;
            // IS this new span logic sound?
            let new_span = left.span.merge(right.span);
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .to_location(new_span);

            if let Some(invert) = invert {
                left = Expression::Unary {
                    expression: Box::new(left),
                    operator: UnaryOperator::Not,
                }
                .to_location(new_span.merge(invert.span));
            }
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

            let new_span = left.span.merge(right.span);
            return Ok(Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .to_location(new_span));
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
            let new_span = left.span.merge(right.span);
            left = Expression::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .to_location(new_span);
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
        let mut expression = if self.match_token(&[Token::Let]).is_some() {
            self.let_statement()?
        } else {
            self.expression()?
        };

        if self.match_token(&[Token::Semicolon]).is_some() {
            self.advance();
            expression = expression.to_statement();
        }

        Ok(expression)
    }

    fn let_statement(&mut self) -> Result<ExpressionLocation, Error> {
        let let_token = self
            .require_current_token_matches(&Token::Let)
            .expect("guaranteed to match by caller");

        let maybe_lvalue = self.tuple_expression(Self::single_expression, false)?;
        let lvalue_span = maybe_lvalue.span;

        let Ok(lvalue) = Lvalue::try_from(maybe_lvalue) else {
            return Err(Error::with_help(
                "Invalid assignment target".to_string(),
                lvalue_span,
                "Assignment target is not a valid lvalue. Only a few expressions can be assigned a value. Check that the left-hand side of the assignment is a valid target.".to_string()
            ));
        };

        self.require_current_token_matches(&Token::EqualsSign)?;

        let expression = self.variable_assignment()?;
        let end = expression.span;
        let declaration = Expression::VariableDeclaration {
            l_value: lvalue,
            value: Box::new(expression),
        };

        if self.peek_current_token().is_some() {
            self.require_current_token_matches(&Token::Semicolon)?;
        }

        Ok(declaration
            .to_location(let_token.span.merge(end))
            .to_statement())
    }

    fn expression(&mut self) -> Result<ExpressionLocation, Error> {
        self.variable_assignment()
    }

    fn variable_assignment(&mut self) -> Result<ExpressionLocation, Error> {
        let maybe_lvalue = self.tuple_expression(Self::single_expression, false)?;
        let start = maybe_lvalue.span;

        if !Lvalue::can_build_from_expression(&maybe_lvalue.expression) {
            // In this case we got some kind of expression that we can't assign to. We can just return the expression as is.
            // But to improve error handling and stuff it would be nice if we could check if the next token matches one
            // of the assignment operator and throw an appropriate error.
            return match self.peek_current_token() {
                // TODO: this really requires a link to the documentation once we have that.
                Some(Token::EqualsSign) => Err(Error::with_help(
                    "Invalid assignment target".to_string(),
                    maybe_lvalue.span,
                    "Assignment target is not a valid lvalue. Only a few expressions can be assigned a value. Check that the left-hand side of the assignment is a valid target.".to_string()
                )),
                _ => Ok(maybe_lvalue),
            };
        }

        match self.peek_current_token() {
            // NOTE: the parser supports every LValue but some might cause an error when declaring vars
            Some(Token::EqualsSign) => {
                self.advance();
                let expression = self.tuple_expression(Self::single_expression, false)?;
                let end = expression.span;
                let assignment_expression = Expression::Assignment {
                    l_value: Lvalue::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an lvalue"),
                    r_value: Box::new(expression),
                };

                Ok(assignment_expression.to_location(start.merge(end)))
            }
            Some(Token::OpAssign(inner)) => {
                let thing = match &inner.token {
                    Token::Identifier(identifier) => Either::Right(identifier.to_string()),
                    _ => Either::Left((*inner.clone()).try_into()?),
                };

                self.advance();
                let expression = self.tuple_expression(Self::single_expression, false)?;
                let end = expression.span;
                let op_assign = Expression::OpAssignment {
                    l_value: Lvalue::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an lvalue"),
                    value: Box::new(expression),
                    operation: thing,
                };

                Ok(op_assign.to_location(start.merge(end)))
            }
            _ => Ok(maybe_lvalue),
        }
    }

    fn tuple_expression(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
        must_be_tuple: bool,
    ) -> Result<ExpressionLocation, Error> {
        let mut expressions = vec![next(self)?];
        let mut must_be_tuple = must_be_tuple;
        while self.consume_token_if(&[Token::Comma]).is_some() {
            // Peek at right paren, if that matches we break
            // this helps us support expressions like `(1,)` and `(1,2,)`
            if self.match_token(&[Token::RightParentheses]).is_some() {
                must_be_tuple = true;
                break;
            }
            expressions.push(next(self)?);
        }

        let new_span = expressions
            .first()
            .expect("first is guaranteed to have a result")
            .span
            .merge(expressions.last().unwrap().span);

        let tuple_expression = ExpressionLocation {
            expression: Expression::Tuple {
                values: expressions,
            },
            span: new_span,
        };

        if must_be_tuple {
            Ok(tuple_expression)
        } else {
            Ok(tuple_expression.simplify())
        }
    }

    /// Parses a delimited tuple (enclosed in parentheses) that can be empty
    fn delimited_tuple(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
    ) -> Result<ExpressionLocation, Error> {
        let start = self.require_current_token_matches(&Token::LeftParentheses)?;
        if let Some(end) = self.consume_token_if(&[Token::RightParentheses]) {
            Ok(Expression::Tuple { values: vec![] }.to_location(start.span.merge(end.span)))
        } else {
            let mut tuple_expression = self.tuple_expression(next, true)?;
            let right_paren_span = self
                .require_current_token_matches(&Token::RightParentheses)?
                .span;

            // Include the right paretheses in the span
            tuple_expression.span = tuple_expression.span.merge(right_paren_span);

            Ok(tuple_expression)
        }
    }

    fn single_expression(&mut self) -> Result<ExpressionLocation, Error> {
        self.logic_or()
    }

    fn logic_or(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_logical_expression_left_associative(Self::logic_and, &[Token::LogicOr])
    }
    fn logic_and(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_logical_expression_left_associative(Self::logic_not, &[Token::LogicAnd])
    }

    // The logical not operator that has much lower precedence than the ! operator.
    // ```ndc
    // x := not foo in bar
    // ```
    fn logic_not(&mut self) -> Result<ExpressionLocation, Error> {
        if let Some(token) = self.consume_token_if(&[Token::LogicNot]) {
            let operator: UnaryOperator = token
                .try_into()
                .expect("consume_operator_if guaranteed us that this token can be UnaryOperator");

            let right = self.logic_not()?;
            let span = right.span;

            Ok(Expression::Unary {
                operator,
                expression: Box::new(right),
            }
            .to_location(span))
        } else {
            self.range()
        }
    }

    fn range(&mut self) -> Result<ExpressionLocation, Error> {
        let left = self.comparison()?;
        if let Some(token) = self.consume_token_if(&[Token::DotDot, Token::DotDotEquals]) {
            let (span, right) = if self.peek_range_end() {
                (left.span.merge(token.span), None)
            } else {
                let right = self.comparison()?;
                (left.span.merge(right.span), Some(Box::new(right)))
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

            Ok(ExpressionLocation { expression, span })
        } else {
            Ok(left)
        }
    }

    fn comparison(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::spaceship,
            &[
                Token::EqualsEquals,
                Token::BangEquals,
                Token::Greater,
                Token::GreaterEquals,
                Token::Less,
                Token::LessEquals,
                Token::In,
            ],
            true,
        )
    }

    fn spaceship(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::bit_shift,
            &[Token::Spaceship, Token::InverseSpaceship],
            false,
        )
    }

    fn bit_shift(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::boolean_or,
            &[Token::LessLess, Token::GreaterGreater],
            false,
        )
    }

    fn boolean_or(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::boolean_xor, &[Token::Pipe], false)
    }

    fn boolean_xor(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::boolean_and, &[Token::Tilde], false)
    }

    fn boolean_and(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(Self::term, &[Token::Ampersand], false)
    }

    fn term(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::factor,
            &[Token::Plus, Token::Minus, Token::PlusPlus, Token::Diamond],
            false,
        )
    }

    fn factor(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_left_associative(
            Self::exponent,
            &[
                Token::ForwardSlash,
                Token::Backslash,
                Token::Asterix,
                Token::Percent,
                Token::PercentPercent,
            ],
            false,
        )
    }

    fn exponent(&mut self) -> Result<ExpressionLocation, Error> {
        self.consume_binary_expression_right_associative(
            Self::tight_unary,
            Self::exponent,
            &[Token::Caret],
        )
    }

    fn tight_unary(&mut self) -> Result<ExpressionLocation, Error> {
        if let Some(token) = self.consume_token_if(&[Token::Bang, Token::Minus, Token::Tilde]) {
            let token_span = token.span;
            let operator: UnaryOperator = token
                .try_into()
                .expect("consume_operator_if guaranteed us that the next token is an Operator");

            let right = self.tight_unary()?;
            let span = right.span;

            Ok(Expression::Unary {
                operator,
                expression: Box::new(right),
            }
            .to_location(span.merge(token_span)))
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
        //    * `identifier.ident` <-- dot call w/o parentheses
        while let Some(current) =
            self.match_token(&[Token::LeftParentheses, Token::LeftSquareBracket, Token::Dot])
        {
            match current.token {
                // handles: foo()
                Token::LeftParentheses => {
                    let arguments = self.delimited_tuple(Self::single_expression)?;
                    let arguments_span = arguments.span;
                    let Expression::Tuple { values: arguments } = arguments.expression else {
                        unreachable!("self.tuple() must always produce a tuple");
                    };

                    let span = expr.span;

                    expr = ExpressionLocation {
                        expression: Expression::Call {
                            function: Box::new(expr),
                            arguments,
                        },
                        span: span.merge(arguments_span),
                    };
                }
                Token::Dot => {
                    self.require_current_token_matches(&Token::Dot)?; // consume matched token
                    let l_value = self.require_identifier()?;
                    let identifier_span = l_value.span;
                    let first_argument_span = expr.span;
                    let identifier = Lvalue::try_from(l_value)?;
                    let Lvalue::Variable { identifier } = identifier else {
                        unreachable!("Guaranteed to match by previous call to require_identifier")
                    };

                    // () is now optional?
                    let (mut arguments, tuple_span) =
                        if self.match_token(&[Token::LeftParentheses]).is_some() {
                            let tuple_expression = self.delimited_tuple(Self::single_expression)?;

                            if let Expression::Tuple { values: arguments } =
                                tuple_expression.expression
                            {
                                (arguments, Some(tuple_expression.span))
                            } else {
                                unreachable!("self.tuple() must always produce a tuple");
                            }
                        } else {
                            (Vec::new(), None)
                        };

                    arguments.insert(0, expr);

                    expr = ExpressionLocation {
                        expression: Expression::Call {
                            function: Box::new(
                                // TODO: probably fix the spans here
                                Expression::Identifier(identifier).to_location(identifier_span),
                            ),
                            arguments,
                        },
                        span: tuple_span
                            .unwrap_or(identifier_span)
                            .merge(first_argument_span),
                    }

                    // for now, we require parentheses
                }
                Token::LeftSquareBracket => {
                    self.require_current_token_matches(&Token::LeftSquareBracket)?;
                    // self.expression here allows for this syntax which is maybe a good idea
                    // `foo[1, 2] == foo[(1, 2)]`
                    // and
                    // `foo[x := 3]`
                    let index_expression = self.expression()?;

                    // TODO: this error may be triggered in a scenario described below, and it would
                    //       probably be nice if we could have a special message in a later version
                    //
                    // # Error code
                    //
                    // if x == y { true } else { false }
                    // [x for x in 1..10]
                    //
                    // In this case we have some kind of expression that could also be a statement
                    // followed by a list comprehension (the same problem would arise if the next
                    // statement was a tuple). The list comprehension or tuple will now be interpreted
                    // as an operand for the previous expression as if we meant to write this:
                    //
                    // if x == y { foo } else { bar }[12]
                    //
                    // This ambiguity can only be resolved by adding a semicolon to the if expression
                    // or by not putting a list comprehension or tuple in this position.
                    let end_token =
                        self.require_current_token_matches(&Token::RightSquareBracket)?;

                    let span = expr.span.merge(end_token.span);

                    expr = ExpressionLocation {
                        expression: Expression::Index {
                            value: Box::new(expr),
                            index: Box::new(index_expression),
                        },
                        span,
                    };
                }
                _ => unreachable!("guaranteed to match"),
            }
        }

        Ok(expr)
    }

    /// Parses a list expression from the current position.
    /// There are a few valid kinds of expressions
    /// # 1. empty list
    /// ```ndc
    /// []
    /// ```
    /// # 2. list of values
    /// ```ndc
    /// [1,2,3]
    /// ```
    /// # 3. list comprehensions
    /// ```ndc
    /// [x + y for x in 0..10, y in 0..10, if x != y]
    /// ```
    fn list(&mut self) -> Result<ExpressionLocation, Error> {
        // Lists must begin with a `[` and the caller should have checked for this
        let left_square_bracket_span = self
            .require_current_token_matches(&Token::LeftSquareBracket)?
            .span;

        if let Some(bracket) = self.consume_token_if(&[Token::RightSquareBracket]) {
            return Ok(Expression::List { values: vec![] }
                .to_location(left_square_bracket_span.merge(bracket.span)));
        }

        // If this isn't an empty list we parse a tuple expression (without delimiters) consisting of
        // single statements like `a, b, c`
        let expr = self.tuple_expression(Self::single_expression, true)?;

        // Now we can take two paths, either the list ends, and it's just a literal list of values
        // OR right after the last value there is a `for` keyword turning this into a list comprehension
        // if neither of those is the case it is an error
        match self.peek_current_token() {
            Some(Token::RightSquareBracket) => {
                let right_square_bracket_span = self
                    .peek_current_token_location()
                    .expect("guaranteed to exist")
                    .span;

                self.advance();

                let Expression::Tuple { values } = expr.expression else {
                    unreachable!("tuple_expression must guarantee us a tuple");
                };

                // Next we can maybe turn this into a list expression
                //let last_value_span = values.last().map_or(left_square_bracket_span, |e| e.span);

                Ok(Expression::List { values }
                    .to_location(left_square_bracket_span.merge(right_square_bracket_span)))
            }
            // WOAH, this is not a list, it's a list comprehension
            Some(Token::For) => {
                let result = ForBody::List(expr.simplify());
                self.for_comprehension(left_square_bracket_span, result, &Token::RightSquareBracket)
            }
            _ => {
                let token = self.require_current_token().expect("must have token here");
                Err(Error::with_help(
                    format!(
                        "Unexpected token '{}' expected either ']' or 'for'",
                        token.token
                    ),
                    token.span,
                    "Expected a list or a for comprehension, but found an unexpected token. Ensure you're using either a concrete list '[...]' or a for comprehension '[... for ... in ...]'.".to_string(),
                ))
            }
        }
    }

    fn for_comprehension(
        &mut self,
        span: Span,
        result: ForBody,
        end_token: &Token,
    ) -> Result<ExpressionLocation, Error> {
        self.require_current_token_matches(&Token::For)
            .expect("guaranteed to match");
        let mut iterations = Vec::new();

        loop {
            match self.peek_current_token() {
                Some(Token::Comma) => {
                    let current_token = self.require_current_token().expect("must have a token");
                    return Err(Error::with_help(
                        format!("Unexpected token '{}'", current_token.token),
                        current_token.span,
                        "The for comprehension contains two consecutive commas".to_string(),
                    ));
                }
                Some(Token::RightSquareBracket) => break,
                Some(Token::If) => iterations.push(self.if_guard()?),
                Some(_) => iterations.push(self.for_iteration()?),
                _ => {}
            }

            // If next is not a comma we break
            if self.consume_token_if(&[Token::Comma]).is_none() {
                break;
            };
        }

        let end = self.require_current_token_matches(end_token)?;

        Ok(Expression::For {
            body: Box::new(result),
            iterations,
        }
        .to_location(span.merge(end.span)))
    }

    fn if_guard(&mut self) -> Result<ForIteration, Error> {
        self.require_current_token_matches(&Token::If)?;
        let guard = self.single_expression()?;
        Ok(ForIteration::Guard(guard))
    }

    /// Parses a for iteration without the for keyword. Examples
    /// ```ndc
    /// x in xs
    /// ```
    fn for_iteration(&mut self) -> Result<ForIteration, Error> {
        let l_value = Lvalue::try_from(self.tuple_expression(Self::primary, false)?)?;

        self.require_current_token_matches(&Token::In)?;

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
        else if self.match_token(&[Token::Fn, Token::Pure]).is_some() {
            return self.function_declaration();
        }
        // matches `return;` and `return (expression);`
        else if let Some(return_token_location) = self.consume_token_if(&[Token::Return]) {
            let expr_loc = if self.match_token(&[Token::Semicolon]).is_some() {
                Expression::Tuple { values: vec![] }.to_location(return_token_location.span)
            } else {
                self.expression()?
            };

            let span = expr_loc.span;

            let return_expression = Expression::Return {
                value: Box::new(expr_loc),
            }
            .to_location(return_token_location.span.merge(span));

            return Ok(return_expression);
        } else if let Some(token_location) = self.consume_token_if(&[Token::Break]) {
            let expression = Expression::Break;
            return Ok(expression.to_location(token_location.span));
        }
        // matches curly bracketed block expression `{ }`
        else if self.match_token(&[Token::LeftCurlyBracket]).is_some() {
            return self.block();
        }
        // matches map expression %{1,2,3}
        else if self.match_token(&[Token::MapOpen]).is_some() {
            return self.map_expression();
        }
        // matches list and list comprehensions
        else if self.match_token(&[Token::LeftSquareBracket]).is_some() {
            return self.list();
        }
        // matches either a grouped expression `(1+1)` or a tuple `(1,1)`
        else if let Some(start_parentheses) = self.consume_token_if(&[Token::LeftParentheses]) {
            // If an opening parentheses is immediately followed by a closing parentheses we're dealing with a Unit expression
            if let Some(end_parentheses) = self.consume_token_if(&[Token::RightParentheses]) {
                return Ok(Expression::Tuple { values: vec![] }
                    .to_location(start_parentheses.span.merge(end_parentheses.span)));
            }

            let grouped = self.expression()?;

            self.require_current_token_matches(&Token::RightParentheses)?;

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
            Token::Identifier(identifier) => Expression::Identifier(identifier),
            _ => {
                // TODO: this error might not be the best way to describe what's happening here
                //       figure out if there is a better way to handle errors here.
                return Err(Error::text(
                    format!(
                        "Expected an expression but got '{}' instead",
                        token_location.token
                    ),
                    token_location.span,
                ));
            }
        };

        Ok(expression.to_location(token_location.span))
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

        let span = expression.span;
        Ok(Expression::If {
            condition: Box::new(expression),
            on_true: Box::new(on_true),
            on_false,
        }
        .to_location(span))
    }

    fn while_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let expression = self.expression()?;
        let loop_body = self.block()?;

        let span = expression.span;
        Ok(Expression::While {
            expression: Box::new(expression),
            loop_body: Box::new(loop_body),
        }
        .to_location(span))
    }

    fn for_expression(&mut self) -> Result<ExpressionLocation, Error> {
        let for_token_span = self
            .require_current_token_matches(&Token::For)
            .expect("required to be the correct token")
            .span;
        let mut iterations = vec![self.for_iteration()?];

        while self.consume_token_if(&[Token::Comma]).is_some() {
            match self.peek_current_token() {
                // Some(Token::RightCurlyBracket) => break,
                Some(Token::If) => iterations.push(self.if_guard()?),
                Some(_) => iterations.push(self.for_iteration()?),
                _ => {
                    return Err(Error::end_of_input(
                        self.tokens.last().expect("must have tokens").span,
                    ));
                }
            }
        }

        let body = self.block()?;
        let body_span = body.span;

        Ok(Expression::For {
            iterations,
            body: Box::new(ForBody::Block(body)),
        }
        .to_location(for_token_span.merge(body_span)))
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
            Err(Error::text(
                "expected an identifier".to_string(),
                identifier_expression.span,
            ))
        }
    }

    fn function_declaration(&mut self) -> Result<ExpressionLocation, Error> {
        let mut modifiers = Vec::new();

        while let Some(token) = self.consume_token_if(&[Token::Fn, Token::Pure]) {
            modifiers.push(token);
        }

        // Remove the last token, check that it matches fn
        let fn_token = match modifiers.pop() {
            // Must be true
            Some(
                token @ TokenLocation {
                    token: Token::Fn, ..
                },
            ) => token,
            Some(TokenLocation { token: _, span }) => {
                return Err(Error::with_help(
                    "The fn keyword must be last in function declaration".to_string(),
                    span,
                    "This token is not in the right place try changing the order".to_string(),
                ));
            }
            None => {
                unreachable!("the caller of this method has guaranteed us that there are tokens")
            }
        };

        let is_pure = modifiers.iter().any(|keyword| keyword.token == Token::Pure);

        // After the fn token we either expect an identifier, or a parameter list in case the function is anonymous
        // fn foo () { }
        // fn() { }
        let identifier = match self.peek_current_token() {
            Some(Token::LeftParentheses) => None,
            Some(Token::Identifier(_)) => Some(
                self.require_identifier()
                    .expect("this is guaranteed to produce an identifier by previous match"),
            ), // MUST BE IDENT
            Some(_) | None => {
                return Err(Error::with_help(
                    "Expected identifier or left parentheses to follow fn keyword".to_string(),
                    self.peek_current_token_location()
                        .map_or(fn_token.span, |it| it.span),
                    "The fn token must either start a named function or an anonymous function."
                        .to_string(),
                ));
            }
        };

        let argument_list = self.delimited_tuple(Self::single_expression)?;

        // Next we either expect a body block `{ ... }` or a fat arrow followed by a single expression `=> ...`

        let body = match self.peek_current_token() {
            Some(Token::FatArrow) => { self.advance(); self.single_expression()?},
            Some(Token::LeftCurlyBracket) => self.block()?,
            Some(token) => {
                return Err(Error::with_help(
                    format!("unexpected token: {token}"),
                    self.peek_current_token_location().unwrap().span,
                    "Expected that the argument list is followed by either a body `{}` or a fat arrow `=>`".to_string(),
                ))
            }
            None => return Err(Error::end_of_input(argument_list.span)),
        };

        let span = fn_token.span.merge(body.span);
        Ok(ExpressionLocation {
            expression: Expression::FunctionDeclaration {
                name: identifier.map(Box::new),
                arguments: Box::new(argument_list),
                body: Rc::new(body),
                pure: is_pure,
            },
            span,
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
        let left_curly_span = self.require_token(&[Token::LeftCurlyBracket])?;

        let mut statements = Vec::new();

        let loop_span = loop {
            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.span;
            }

            if self.peek_current_token_location().is_some() {
                statements.push(self.expression_or_statement()?);
            } else {
                return Err(Error::text(
                    "Unexpected end of input while parsing a block, expected '}'".to_string(),
                    self.tokens.last().expect("tokens must not be empty").span,
                ));
            }
        };

        Ok(Expression::Block { statements }.to_location(left_curly_span.merge(loop_span)))
    }

    fn map_expression(&mut self) -> Result<ExpressionLocation, Error> {
        // This should have been checked before this method is called;
        let map_open_span = self.require_token(&[Token::MapOpen])?;

        // Optional default value
        let default = if self.consume_token_if(&[Token::Colon]).is_some() {
            let default = self.single_expression()?;
            // If the list ends without any values we just do nothing and let the loop below handle the rest
            if self.match_token(&[Token::RightCurlyBracket]).is_none() {
                // If there isn't a '}' to close the map there must be a comma otherwise we error out
                self.require_current_token_matches(&Token::Comma)?;
            }
            Some(Box::new(default))
        } else {
            None
        };

        let mut values = Vec::new();

        let map_close_span = loop {
            // End parsing if we see a RightCurlyBracket, this one only happens if the expression is
            // empty `%{}` or if there is a trailing comma `%{1,2,3,}`
            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.span;
            }

            let key = self.single_expression()?;

            if self.consume_token_if(&[Token::Colon]).is_some() {
                let value = self.single_expression()?;
                values.push((key, Some(value)));
            } else {
                values.push((key, None));
            }

            if let Some(token_location) = self.consume_token_if(&[Token::RightCurlyBracket]) {
                break token_location.span;
            }

            if values.len() == 1 && self.match_token(&[Token::For]).is_some() {
                let (key_expr, value_expr) = values
                    .pop()
                    .expect("guaranteed by previous call to values.len()");
                return self.for_comprehension(
                    map_open_span,
                    ForBody::Map {
                        key: key_expr,
                        value: value_expr,
                        default,
                    },
                    &Token::RightCurlyBracket,
                );
            }

            self.require_current_token_matches(&Token::Comma)?;
        };
        Ok(Expression::Map { values, default }.to_location(map_open_span.merge(map_close_span)))
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

#[cfg_attr(feature = "miette" derive(miette:Diagnostic))]
#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct Error {
    text: String,
    #[cfg_attr(feature = "miette", help)]
    help_text: Option<String>,
    #[cfg_attr(feature = "miette", label("here"))]
    span: Span,
}

impl Error {
    #[must_use]
    pub fn text(text: String, span: Span) -> Self {
        Self {
            text,
            span,
            help_text: None,
        }
    }

    #[must_use]
    pub fn with_help(text: String, span: Span, help_text: String) -> Self {
        Self {
            text,
            span,
            help_text: Some(help_text),
        }
    }

    #[must_use]
    pub fn end_of_input(span: Span) -> Self {
        Self {
            text: "Unexpected end of input".to_string(),
            span,
            help_text: Some("The token stream ended prematurely. Ensure all blocks, expressions, or statements are properly closed.".to_string()),
        }
    }
}

fn tokens_to_string(tokens: &[Token]) -> String {
    let mut buf = String::new();
    let mut iter = tokens.iter().peekable();
    while let Some(token) = iter.next() {
        if iter.peek().is_none() {
            write!(buf, "'{token}'").expect("serializing symbols must succeed");
        } else {
            write!(buf, "'{token}', ").expect("serializing symbols must succeed");
        }
    }
    buf
}
