use ndc_core::NodeId;
use std::fmt::Write;

use crate::expression::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation,
    ExpressionLocation, ForBody, ForIteration, FunctionParameter, NonBindingTarget,
};
use crate::expression::{Expression, StructField, TypeParameter};
use crate::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::type_expr::TypeExpr;
use ndc_lexer::{NumericLiteral, Span, Token, TokenLocation};

pub struct Parser {
    tokens: Vec<TokenLocation>,
    current: usize,
}

impl Parser {
    /// A parser over `tokens`, without their comments: a comment can stand
    /// between any two tokens and never changes what the program means.
    pub fn from_tokens(tokens: Vec<TokenLocation>) -> Self {
        let tokens = tokens
            .into_iter()
            .filter(|token| !matches!(token.token, Token::Comment(_)))
            .collect();
        Self { tokens, current: 0 }
    }

    /// Parse a Vec of Tokens into a Vec of statements
    /// # Errors
    /// If the parsing fails which it could do for many different reasons it will return an [Error]. See the variants of
    /// this type for more information about the different kinds of errors.
    pub fn parse(&mut self) -> Result<Vec<ExpressionLocation>, Error> {
        let is_valid_statement = |expr: &Expression| -> bool {
            matches!(
                expr,
                Expression::Block { .. }
                    | Expression::Statement(_)
                    | Expression::If { .. }
                    | Expression::While { .. }
                    | Expression::For { .. }
                    | Expression::FunctionDeclaration { .. }
                    | Expression::VariableDeclaration { .. }
                    | Expression::StructDeclaration { .. }
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
        if self.peek_current_token_location().is_some() {
            return Err(self.missing_semicolon());
        }

        Ok(expressions)
    }

    fn peek_current_token(&self) -> Option<&Token> {
        self.peek_current_token_location().map(|t| &t.token)
    }

    fn peek_current_token_location(&self) -> Option<&TokenLocation> {
        self.tokens.get(self.current)
    }

    /// The error for a statement that ends without a `;` before the current
    /// token. It points right after the statement's last token, where the `;`
    /// goes, rather than at the token that follows, which is usually on the
    /// next line: in `let a = 5\nprint(a);`, right after the `5`.
    fn missing_semicolon(&self) -> Error {
        let found = self
            .peek_current_token_location()
            .expect("a statement is followed by a token");
        if let Some(error) = self.unmatched_closing_bracket(found) {
            return error;
        }
        let span = match self.current.checked_sub(1).map(|i| &self.tokens[i]) {
            Some(last) => Span::new(last.span.source_id(), last.span.end(), 0),
            None => found.span,
        };
        Error::with_help(
            format!("Expected token ';' but got '{}' instead", found.token),
            span,
            "Every statement ends with a `;`".to_string(),
        )
    }

    fn peek_next_token(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1).map(|t| &t.token)
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
            let expected = match tokens {
                [token] => format!("'{token}'"),
                _ => format!("one of {}", tokens_to_string(tokens)),
            };
            Err(Error::text(
                format!(
                    "Expected {expected} but got '{}' instead",
                    current_token.token
                ),
                current_token.span,
            ))
        } else {
            Err(self.unexpected_end_of_input())
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
            .ok_or_else(|| self.unexpected_end_of_input())?
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
            let (invert, operator_token_loc) = if token_location.token == Token::LogicNot {
                let augmented = match self.consume_token_if(valid_tokens) {
                    Some(t) => t,
                    None => {
                        return Err(match self.peek_current_token_location() {
                            Some(current) => Error::text(
                                format!("unexpected token {}", current.token),
                                current.span,
                            ),
                            None => Error::end_of_input(token_location.span),
                        });
                    }
                };
                (Some(token_location), augmented)
            } else {
                (None, token_location)
            };

            let operator: BinaryOperator =
                operator_token_loc.clone().try_into().unwrap_or_else(|_| {
                    panic!(
                        "cannot convert '{}' into a binary operator",
                        operator_token_loc.token
                    )
                });
            let right = next(self)?;
            // IS this new span logic sound?
            let new_span = left.span.merge(right.span);

            // Is this always the same
            debug_assert_eq!(operator.to_string(), operator_token_loc.token.to_string());

            left = Expression::OperatorCall {
                function: Box::new(
                    Expression::Identifier {
                        name: operator_token_loc.token.to_string(),
                        identifier_span: operator_token_loc.span,
                        type_arguments: Vec::new(),
                    }
                    .to_location(operator_token_loc.span),
                ),
                arguments: vec![left, right],
            }
            .to_location(new_span);

            if let Some(not_token) = invert {
                left = Expression::OperatorCall {
                    function: Box::new(
                        Expression::Identifier {
                            name: not_token.token.to_string(),
                            identifier_span: not_token.span,
                            type_arguments: Vec::new(),
                        }
                        .to_location(not_token.span),
                    ),
                    arguments: vec![left],
                }
                .to_location(new_span.merge(not_token.span));
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
            let operator_span = token_location.span;
            let operator = BinaryOperator::try_from(token_location)
                .expect("COMPILER ERROR: consume_token_if must guarantee the correct token");
            let right = current(self)?;

            let new_span = left.span.merge(right.span);

            return Ok(Expression::OperatorCall {
                function: Box::new(
                    Expression::Identifier {
                        name: operator.to_string(),
                        identifier_span: operator_span,
                        type_arguments: Vec::new(),
                    }
                    .to_location(operator_span),
                ),
                arguments: vec![left, right],
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
        } else if self.match_token(&[Token::Struct]).is_some() {
            self.struct_declaration()?
        } else if self
            .match_token(&[Token::If, Token::While, Token::For, Token::LeftCurlyBracket])
            .is_some()
        {
            // A statement that starts with an `if`, a loop or a block ends
            // at its closing brace, as in Rust: what follows starts the next
            // statement, so `for x in xs { ... }` and then `[x]` on the next
            // line is a list, not an index into the loop's value. Within an
            // expression they still take an index or a call, as in
            // `print(if c { [1] } else { [2] }[0])`.
            self.primary()?
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

        if let Some(name) = self.peek_current_token_location() {
            if name.token == Token::EqualsSign {
                return Err(Error::with_help(
                    "Expected a name after 'let'".to_string(),
                    name.span,
                    "Give the variable a name, as in `let x = 5;`".to_string(),
                ));
            }
            if is_keyword(&name.token) {
                return Err(Error::with_help(
                    format!(
                        "'{}' is a keyword, so it can't be a variable name",
                        name.token
                    ),
                    name.span,
                    "Choose another name for the variable".to_string(),
                ));
            }
        }

        let (lvalue, annotated_type) = self.named_binding()?;

        self.require_current_token_matches(&Token::EqualsSign)?;

        let expression = self.variable_assignment()?;
        let end = expression.span;
        let declaration = Expression::VariableDeclaration {
            l_value: lvalue,
            annotated_type,
            value: Box::new(expression),
        };

        // Require the terminating semicolon but leave it for
        // `expression_or_statement` to consume, so the declaration is wrapped
        // in a Statement like any other `;`-terminated expression.
        if self
            .peek_current_token()
            .is_some_and(|token| token != &Token::Semicolon)
        {
            return Err(self.missing_semicolon());
        }

        Ok(declaration.to_location(let_token.span.merge(end)))
    }

    fn expression(&mut self) -> Result<ExpressionLocation, Error> {
        self.variable_assignment()
    }

    fn variable_assignment(&mut self) -> Result<ExpressionLocation, Error> {
        let maybe_lvalue = self.tuple_expression(Self::single_expression, false)?;
        let start = maybe_lvalue.span;

        if !AssignmentTarget::can_build_from_expression(&maybe_lvalue.expression) {
            // In this case we got some kind of expression that we can't assign to. We can just return the expression as is.
            // But to improve error handling and stuff it would be nice if we could check if the next token matches one
            // of the assignment operator and throw an appropriate error.
            return match self.peek_current_token() {
                Some(Token::EqualsSign) => Err(Error::with_help(
                    "Invalid assignment target".to_string(),
                    maybe_lvalue.span,
                    "Assignment target is not a valid lvalue. Only a few expressions can be assigned a value. Check that the left-hand side of the assignment is a valid target.".to_string(),
                )),
                _ => Ok(maybe_lvalue),
            };
        }

        match self.peek_current_token() {
            Some(Token::EqualsSign) => {
                self.advance();
                let expression = self.tuple_expression(Self::single_expression, false)?;
                let end = expression.span;
                let assignment_expression = Expression::Assignment {
                    l_value: AssignmentTargetLocation::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an assignment target"),
                    r_value: Box::new(expression),
                };

                Ok(assignment_expression.to_location(start.merge(end)))
            }
            Some(Token::OpAssign(inner)) => {
                let operation_identifier = inner.token.to_string();

                self.advance();
                let expression = self.tuple_expression(Self::single_expression, false)?;
                let end = expression.span;
                let op_assign = Expression::OpAssignment {
                    l_value: AssignmentTargetLocation::try_from(maybe_lvalue)
                        .expect("guaranteed to produce an assignment target"),
                    r_value: Box::new(expression),
                    operation: operation_identifier,
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
            // A comma right before a closing bracket is a trailing comma, as in
            // `(1, 2,)` and `[1, 2,]`. After a single value in parentheses it
            // makes a 1-tuple: `(1,)`
            if self
                .match_token(&[Token::RightParentheses, Token::RightSquareBracket])
                .is_some()
            {
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
            id: NodeId::next(),
        };

        if must_be_tuple {
            Ok(tuple_expression)
        } else {
            Ok(tuple_expression.simplify())
        }
    }

    fn delimited_comma_separated<T>(
        &mut self,
        open: &Token,
        close: &Token,
        parse_item: fn(&mut Self) -> Result<T, Error>,
        allow_empty: bool,
    ) -> Result<(Vec<T>, Span), Error> {
        let open_span = self.require_current_token_matches(open)?.span;

        if let Some(close_token) = self.consume_token_if(std::slice::from_ref(close)) {
            if allow_empty {
                return Ok((Vec::new(), open_span.merge(close_token.span)));
            }

            return Err(Error::with_help(
                format!("expected an item before '{close}'"),
                close_token.span,
                "This delimited list cannot be empty.".to_string(),
            ));
        }

        let mut items = vec![parse_item(self)?];

        while self.consume_token_if(&[Token::Comma]).is_some() {
            if self.match_token(std::slice::from_ref(close)).is_some() {
                break;
            }

            items.push(parse_item(self)?);
        }

        let close_span = self.require_current_token_matches(close)?.span;
        Ok((items, open_span.merge(close_span)))
    }

    /// Parses a delimited tuple (enclosed in parentheses) that can be empty
    fn delimited_tuple(
        &mut self,
        next: fn(&mut Self) -> Result<ExpressionLocation, Error>,
    ) -> Result<ExpressionLocation, Error> {
        let (values, span) = self.delimited_comma_separated(
            &Token::LeftParentheses,
            &Token::RightParentheses,
            next,
            true,
        )?;

        Ok(Expression::Tuple { values }.to_location(span))
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
        if let Some(operator_token_loc) = self.consume_token_if(&[Token::LogicNot]) {
            let operator_span = operator_token_loc.span;
            let _: UnaryOperator = operator_token_loc
                .clone()
                .try_into()
                .expect("consume_operator_if guaranteed us that this token can be UnaryOperator");

            let right = self.logic_not()?;
            let span = right.span;

            Ok(Expression::OperatorCall {
                function: Box::new(
                    Expression::Identifier {
                        name: operator_token_loc.token.to_string(),
                        identifier_span: operator_span,
                        type_arguments: Vec::new(),
                    }
                    .to_location(operator_span),
                ),
                arguments: vec![right],
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

            Ok(ExpressionLocation {
                expression,
                span,
                id: NodeId::next(),
            })
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
            Self::no_as_cast,
            Self::exponent,
            &[Token::Caret],
        )
    }

    /// An operand, which `as` no longer follows: a cast is the call
    /// `cast::<T>(x)`. `as` and `as?` stay keywords, so `x as Int` says how to
    /// write it now.
    fn no_as_cast(&mut self) -> Result<ExpressionLocation, Error> {
        let expression = self.tight_unary()?;
        let Some(operator) = self.consume_token_if(&[Token::As, Token::AsOptional]) else {
            return Ok(expression);
        };
        let name = if operator.token == Token::AsOptional {
            "cast?"
        } else {
            "cast"
        };
        let target = self
            .type_annotation()
            .map_or_else(|_| "T".to_string(), |annotation| annotation.to_string());
        Err(Error::with_help(
            format!("`{}` is no longer a cast", operator.token),
            operator.span,
            format!(
                "a cast is a call now: write `{name}::<{target}>(x)` instead of `x {} {target}`",
                operator.token
            ),
        ))
    }

    fn tight_unary(&mut self) -> Result<ExpressionLocation, Error> {
        if let Some(operator_token_loc) =
            self.consume_token_if(&[Token::Bang, Token::Minus, Token::Tilde])
        {
            let token_span = operator_token_loc.span;

            let right = self.tight_unary()?;
            let span = right.span;

            Ok(Expression::OperatorCall {
                function: Box::new(
                    Expression::Identifier {
                        name: operator_token_loc.token.to_string(),
                        identifier_span: token_span,
                        type_arguments: Vec::new(),
                    }
                    .to_location(token_span),
                ),
                arguments: vec![right],
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
        //    * `identifier.ident` <-- member access
        while let Some(current) =
            self.match_token(&[Token::LeftParentheses, Token::LeftSquareBracket, Token::Dot])
        {
            match current.token {
                // handles: foo()
                Token::LeftParentheses => {
                    let arguments = self.delimited_tuple(Self::single_expression)?;
                    let arguments_span = arguments.span;
                    let Expression::Tuple {
                        values: mut arguments,
                    } = arguments.expression
                    else {
                        unreachable!("self.tuple() must always produce a tuple");
                    };

                    let span = expr.span;
                    let expression = match expr.expression {
                        Expression::MemberAccess {
                            receiver,
                            member,
                            member_span,
                            ..
                        } => {
                            arguments.insert(0, *receiver);
                            Expression::MethodCall {
                                function: Box::new(
                                    Expression::Identifier {
                                        name: member,
                                        identifier_span: member_span,
                                        type_arguments: Vec::new(),
                                    }
                                    .to_location(member_span),
                                ),
                                arguments,
                            }
                        }
                        expression => Expression::Call {
                            function: Box::new(ExpressionLocation {
                                expression,
                                span: expr.span,
                                id: expr.id,
                            }),
                            arguments,
                        },
                    };

                    expr = ExpressionLocation {
                        expression,
                        span: span.merge(arguments_span),
                        id: NodeId::next(),
                    };
                }
                Token::Dot => {
                    self.require_current_token_matches(&Token::Dot)?; // consume matched token
                    let l_value = self.require_identifier()?;
                    let identifier_span = l_value.span;
                    let identifier = AssignmentTargetLocation::try_from(l_value)?;
                    let AssignmentTarget::Identifier { identifier, .. } = identifier.target else {
                        unreachable!("Guaranteed to match by previous call to require_identifier")
                    };

                    // `x.f::<T>(y)` is the call `f::<T>(x, y)`; type arguments
                    // only go with a call, as a member has none.
                    if self.peek_current_token() == Some(&Token::ColonColon) {
                        let (type_arguments, function_span) = self.turbofish(identifier_span)?;
                        if self.peek_current_token() != Some(&Token::LeftParentheses) {
                            return Err(Error::with_help(
                                format!("expected '(' after `.{identifier}::<…>`"),
                                function_span,
                                format!(
                                    "type arguments after a `.` go with a call, as in `x.{identifier}::<T>()`"
                                ),
                            ));
                        }
                        let arguments = self.delimited_tuple(Self::single_expression)?;
                        let arguments_span = arguments.span;
                        let Expression::Tuple {
                            values: mut arguments,
                        } = arguments.expression
                        else {
                            unreachable!("self.tuple() must always produce a tuple");
                        };
                        let span = expr.span.merge(arguments_span);
                        arguments.insert(0, expr);
                        expr = Expression::MethodCall {
                            function: Box::new(
                                Expression::Identifier {
                                    name: identifier,
                                    identifier_span,
                                    type_arguments,
                                }
                                .to_location(function_span),
                            ),
                            arguments,
                        }
                        .to_location(span);
                        continue;
                    }

                    let span = expr.span.merge(identifier_span);
                    expr = Expression::MemberAccess {
                        receiver: Box::new(expr),
                        member: identifier,
                        member_span: identifier_span,
                    }
                    .to_location(span);
                }
                Token::LeftSquareBracket => {
                    let bracket_span = current.span;
                    self.require_current_token_matches(&Token::LeftSquareBracket)?;
                    // self.expression here allows for this syntax which is maybe a good idea
                    // `foo[1, 2] == foo[(1, 2)]`
                    // and
                    // `foo[x := 3]`
                    let mut index_expression = self.expression()?;

                    // Reject `a[x..=]` — inclusive ranges must have an end.
                    if matches!(
                        &index_expression.expression,
                        Expression::RangeInclusive { end: None, .. }
                    ) {
                        return Err(Error::text(
                            "inclusive ranges must have an end".to_string(),
                            index_expression.span,
                        ));
                    }

                    // Normalize open-ended ranges: `a[..3]` → `a[0..3]` so the range
                    // can be evaluated as a standalone value (ranges without a lower
                    // bound cannot be evaluated otherwise).
                    match &mut index_expression.expression {
                        Expression::RangeExclusive {
                            start: start @ None,
                            ..
                        }
                        | Expression::RangeInclusive {
                            start: start @ None,
                            ..
                        } => {
                            *start = Some(Box::new(
                                Expression::NumericLiteral(NumericLiteral::Int64(0))
                                    .to_location(index_expression.span),
                            ));
                        }
                        _ => {}
                    }

                    // Note: `if x == y { true } else { false }` followed by `[x for x in 1..10]`
                    // on the next line triggers this path — the list comprehension is parsed as an
                    // index operation on the if-expression. A semicolon after the if resolves the
                    // ambiguity.
                    let end_token =
                        self.require_current_token_matches(&Token::RightSquareBracket)?;

                    let span = expr.span.merge(end_token.span);

                    expr = ExpressionLocation {
                        expression: Expression::Call {
                            function: Box::new(
                                Expression::Identifier {
                                    name: "[]".to_string(),
                                    identifier_span: bracket_span,
                                    type_arguments: Vec::new(),
                                }
                                .to_location(bracket_span),
                            ),
                            arguments: vec![expr, index_expression],
                        },
                        span,
                        id: NodeId::next(),
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
                let result = ForBody::List {
                    expr: expr.simplify(),
                };
                self.for_comprehension(left_square_bracket_span, result, &Token::RightSquareBracket)
            }
            _ => {
                let token = self.require_current_token()?;
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
        let mut iterations = vec![self.comprehension_clause()?];

        while self.consume_token_if(&[Token::Comma]).is_some() {
            match self.peek_current_token() {
                // A trailing comma, as in `[x for x in xs,]`
                Some(token) if token == end_token => break,
                Some(Token::Comma) => {
                    let current_token = self.require_current_token().expect("must have a token");
                    return Err(Error::with_help(
                        format!("Unexpected token '{}'", current_token.token),
                        current_token.span,
                        "The for comprehension contains two consecutive commas".to_string(),
                    ));
                }
                _ => iterations.push(self.comprehension_clause()?),
            }
        }

        let end = self.require_current_token_matches(end_token)?;

        Ok(Expression::For {
            body: Box::new(result),
            iterations,
        }
        .to_location(span.merge(end.span)))
    }

    /// Parses one clause of a comprehension: an iteration `x in xs` or a guard `if x > 1`
    fn comprehension_clause(&mut self) -> Result<ForIteration, Error> {
        if self.peek_current_token() == Some(&Token::If) {
            self.if_guard()
        } else {
            self.for_iteration()
        }
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
        let maybe_lvalue = self.tuple_expression(Self::primary, false)?;
        let lvalue_span = maybe_lvalue.span;
        let l_value = AssignmentTargetLocation::try_from(maybe_lvalue)?;

        let l_value = BindingPatternLocation::try_from(l_value).map_err(|target| {
            let help = match target {
                NonBindingTarget::Member => {
                    "A for loop introduces a new binding for each iteration; a struct field like `foo.bar` cannot be an iteration variable."
                }
                NonBindingTarget::Index => {
                    "A for loop introduces a new binding for each iteration; an indexed element like `foo[index]` cannot be an iteration variable."
                }
            };
            Error::with_help(
                "Invalid iteration variable".to_string(),
                lvalue_span,
                help.to_string(),
            )
        })?;

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
        if let Some(if_token) = self.consume_token_if(&[Token::If]) {
            return self.if_expression(if_token.span);
        }
        // matches while loops like `while foo < bar { }`
        else if let Some(while_token) = self.consume_token_if(&[Token::While]) {
            return self.while_expression(while_token.span);
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
        } else if let Some(token_location) = self.consume_token_if(&[Token::Continue]) {
            let expression = Expression::Continue;
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

            let mut grouped = self.expression()?;

            let end_parentheses = self.require_current_token_matches(&Token::RightParentheses)?;
            let span = start_parentheses.span.merge(end_parentheses.span);

            // Parenthesized member access keeps its Grouping wrapper so the
            // postfix-call rewrite can tell `(s.f)()` (call the field's value)
            // apart from `s.f()` (method call, lowered to `f(s)`).
            if matches!(grouped.expression, Expression::MemberAccess { .. }) {
                return Ok(Expression::Grouping(Box::new(grouped)).to_location(span));
            }

            grouped.span = span;
            return Ok(grouped);
        }

        let Some(token_location) = self.peek_current_token_location().cloned() else {
            return Err(self.expected_expression_at_end());
        };

        let expression = match token_location.token {
            Token::False => Expression::BoolLiteral(false),
            Token::True => Expression::BoolLiteral(true),
            Token::None => Expression::NoneLiteral,
            Token::NumericLiteral(literal) => Expression::NumericLiteral(literal),
            Token::String(value) => Expression::StringLiteral(value),
            Token::Identifier(identifier) => {
                self.advance();
                let (type_arguments, span) = self.turbofish(token_location.span)?;
                return Ok(Expression::Identifier {
                    name: identifier,
                    identifier_span: token_location.span,
                    type_arguments,
                }
                .to_location(span));
            }
            _ => return Err(self.expected_expression(&token_location)),
        };
        self.advance();

        Ok(expression.to_location(token_location.span))
    }

    /// The token before the current one, if any.
    fn previous_token_location(&self) -> Option<&TokenLocation> {
        self.current.checked_sub(1).and_then(|i| self.tokens.get(i))
    }

    /// The error for `found`, the current token, where an expression should
    /// start. It names the mistake where it can tell what it is: `1 + ;`
    /// says the `+` has nothing after it, a lone `)` that nothing opened says
    /// so, and `[1,,2]` points out the extra comma.
    fn expected_expression(&self, found: &TokenLocation) -> Error {
        let previous = self.previous_token_location();

        if let Some(error) = self.unmatched_closing_bracket(found) {
            return error;
        }

        match (&found.token, previous.map(|p| &p.token)) {
            (Token::Else, _) => {
                return Error::with_help(
                    "Unexpected 'else' without an 'if' before it".to_string(),
                    found.span,
                    "'else' can only follow the block of an 'if', as in `if x > 0 { 1 } else { 2 }`"
                        .to_string(),
                );
            }
            (Token::Dot, _) if matches!(self.peek_next_token(), Some(Token::NumericLiteral(_))) => {
                return Error::with_help(
                    "A number can't start with '.'".to_string(),
                    found.span,
                    "Put a digit before the decimal point: write `0.5` instead of `.5`".to_string(),
                );
            }
            (Token::Comma, Some(Token::Comma)) => {
                return Error::with_help(
                    "Expected a value between these two commas".to_string(),
                    found.span,
                    "Remove the extra ',' or put a value between the commas".to_string(),
                );
            }
            (
                Token::Comma,
                Some(Token::LeftParentheses | Token::LeftSquareBracket | Token::MapOpen),
            ) => {
                return Error::with_help(
                    "Expected a value before ','".to_string(),
                    found.span,
                    "Remove the ',' or put a value in front of it".to_string(),
                );
            }
            _ => {}
        }

        if let Some(previous) = previous.filter(|p| expects_expression_after(&p.token)) {
            return Error::with_help(
                format!(
                    "Expected an expression after '{}' but got '{}' instead",
                    previous.token, found.token
                ),
                found.span,
                expression_after_help(&previous.token),
            );
        }

        Error::text(
            format!("Expected an expression but got '{}' instead", found.token),
            found.span,
        )
    }

    /// The error for a program that ends where an expression should start,
    /// as in `let x = 1 +`.
    fn expected_expression_at_end(&self) -> Error {
        match self
            .previous_token_location()
            .filter(|p| expects_expression_after(&p.token))
        {
            Some(previous) => Error::with_help(
                format!(
                    "Expected an expression after '{}' but the program ended",
                    previous.token
                ),
                previous.span,
                expression_after_help(&previous.token),
            ),
            None => self.unexpected_end_of_input(),
        }
    }

    /// The error for a program that ends too soon. It points at the last
    /// bracket that is never closed, if there is one, as that is the usual
    /// reason: in `print([1, 2`, the `[`.
    fn unexpected_end_of_input(&self) -> Error {
        let mut open: Vec<&TokenLocation> = Vec::new();
        for token in &self.tokens {
            if closing_bracket_for(&token.token).is_some() {
                open.push(token);
            } else if open
                .last()
                .is_some_and(|last| closing_bracket_for(&last.token).as_ref() == Some(&token.token))
            {
                open.pop();
            }
        }

        match open.last() {
            Some(bracket) => Error::with_help(
                format!(
                    "Unexpected end of input: this '{}' is never closed",
                    bracket.token
                ),
                bracket.span,
                format!(
                    "Add a '{}' to close it",
                    closing_bracket_for(&bracket.token).expect("is an opening bracket")
                ),
            ),
            None => Error::end_of_input(self.tokens.last().expect("last token exists").span),
        }
    }

    /// The error for `found`, the current token, if it is a closing bracket
    /// that no bracket before it opened, as the `)` in `print(x));`.
    fn unmatched_closing_bracket(&self, found: &TokenLocation) -> Option<Error> {
        let open = opening_bracket_for(&found.token)?;
        if self.has_unclosed(&found.token) {
            return None;
        }
        Some(Error::with_help(
            format!("Unexpected '{}' without a matching '{open}'", found.token),
            found.span,
            format!(
                "Remove the '{}', or add the '{open}' it is meant to close",
                found.token
            ),
        ))
    }

    /// Whether a bracket before the current token is still open that
    /// `closing` would close.
    fn has_unclosed(&self, closing: &Token) -> bool {
        let mut depth = 0i32;
        for token in &self.tokens[..self.current] {
            if closing_bracket_for(&token.token).as_ref() == Some(closing) {
                depth += 1;
            } else if &token.token == closing {
                depth -= 1;
            }
        }
        depth > 0
    }

    /// Whether `header`, the condition of an `if` or `while` or the sequence
    /// of a `for`, is a block with no block after it: then the header is
    /// missing and its body was taken for it, as in `if { }`.
    fn took_body_as_header(&self, header: &ExpressionLocation) -> bool {
        matches!(header.expression, Expression::Block { .. })
            && self.peek_current_token() != Some(&Token::LeftCurlyBracket)
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
    /// Parses an `if` after its keyword, whose span is `if_span`. The expression
    /// spans from the keyword to the end of its last branch.
    fn if_expression(&mut self, if_span: Span) -> Result<ExpressionLocation, Error> {
        let expression = self.expression()?;
        if self.took_body_as_header(&expression) {
            return Err(Error::with_help(
                "Expected a condition between 'if' and its block".to_string(),
                if_span,
                "Write the condition after 'if', as in `if x > 0 { ... }`".to_string(),
            ));
        }
        let on_true = self.block()?;

        let on_false = if self.consume_token_if(&[Token::Else]).is_some() {
            if let Some(if_token) = self.consume_token_if(&[Token::If]) {
                let expression = self.if_expression(if_token.span)?;
                Some(Box::new(expression))
            } else {
                Some(Box::new(self.block()?))
            }
        } else {
            None
        };

        let last_branch = on_false.as_deref().unwrap_or(&on_true);
        let span = if_span.merge(last_branch.span);
        Ok(Expression::If {
            condition: Box::new(expression),
            on_true: Box::new(on_true),
            on_false,
        }
        .to_location(span))
    }

    /// Parses a `while` after its keyword, whose span is `while_span`. The
    /// expression spans from the keyword to the end of its body.
    fn while_expression(&mut self, while_span: Span) -> Result<ExpressionLocation, Error> {
        let expression = self.expression()?;
        if self.took_body_as_header(&expression) {
            return Err(Error::with_help(
                "Expected a condition between 'while' and its block".to_string(),
                while_span,
                "Write the condition after 'while', as in `while x > 0 { ... }`".to_string(),
            ));
        }
        let loop_body = self.block()?;

        let span = while_span.merge(loop_body.span);
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
                    return Err(self.unexpected_end_of_input());
                }
            }
        }

        if let Some(ForIteration::Iteration { sequence, .. }) = iterations.last()
            && self.took_body_as_header(sequence)
        {
            return Err(Error::with_help(
                "Expected something to loop over between 'in' and the loop's block".to_string(),
                for_token_span,
                "Write what to loop over after 'in', as in `for x in [1, 2, 3] { ... }`"
                    .to_string(),
            ));
        }

        let body = self.block()?;
        let body_span = body.span;

        Ok(Expression::For {
            iterations,
            body: Box::new(ForBody::Block(body)),
        }
        .to_location(for_token_span.merge(body_span)))
    }

    /// A name: an identifier with no type arguments, as a declaration and
    /// a member are named. `x.f::<T>()` takes its type arguments itself.
    fn require_identifier(&mut self) -> Result<ExpressionLocation, Error> {
        let after_dot = self
            .previous_token_location()
            .filter(|previous| previous.token == Token::Dot)
            .cloned();
        if let Some(dot) = &after_dot
            && self.peek_current_token().is_none()
        {
            return Err(Error::with_help(
                "Expected a name after '.' but the program ended".to_string(),
                dot.span,
                "Write the name of a field or a function after the '.', as in `xs.len()`"
                    .to_string(),
            ));
        }
        let token = self.require_current_token()?;
        let Token::Identifier(name) = token.token else {
            if after_dot.is_some() {
                return Err(Error::with_help(
                    format!(
                        "Expected a name after '.' but got '{}' instead",
                        token.token
                    ),
                    token.span,
                    "Write the name of a field or a function after the '.', as in `xs.len()`"
                        .to_string(),
                ));
            }
            return Err(Error::text(
                "expected an identifier".to_string(),
                token.span,
            ));
        };
        Ok(Expression::Identifier {
            name,
            identifier_span: token.span,
            type_arguments: Vec::new(),
        }
        .to_location(token.span))
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
            Some(Token::Less) => {
                let less = self
                    .peek_current_token_location()
                    .expect("the current token is '<'");
                return Err(Error::with_help(
                    "an anonymous function can't declare type parameters".to_string(),
                    less.span,
                    "declare them on a named function: `fn f<T>(x: T) -> T`".to_string(),
                ));
            }
            Some(Token::Identifier(_)) => {
                let Ok(ExpressionLocation {
                    expression: Expression::Identifier { name, .. },
                    ..
                }) = self.require_identifier()
                else {
                    unreachable!("guaranteed to to produce identifier")
                };

                Some(name)
            } // MUST BE IDENT
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

        let type_parameters = if self.peek_current_token() == Some(&Token::Less) {
            self.type_parameters()?
        } else {
            Vec::new()
        };

        let (argument_list, parameters_span) = self.delimited_comma_separated(
            &Token::LeftParentheses,
            &Token::RightParentheses,
            Self::named_parameter,
            true,
        )?;

        // Optional return type annotation: `-> Type`
        let return_annotation = if self.peek_current_token() == Some(&Token::RightArrow) {
            self.advance();
            Some(self.type_annotation()?)
        } else {
            None
        };

        // Next we either expect a body block `{ ... }` or a fat arrow followed by a single expression `=> ...`

        let body = match self.peek_current_token() {
            Some(Token::FatArrow) => {
                self.advance();
                self.single_expression()?
            }
            Some(Token::LeftCurlyBracket) => self.block()?,
            Some(token) => {
                return Err(Error::with_help(
                    format!("unexpected token: {token}"),
                    self.peek_current_token_location().unwrap().span,
                    "Expected that the argument list is followed by either a body `{}` or a fat arrow `=>`".to_string(),
                ))
            }
            None => return Err(Error::end_of_input(parameters_span)),
        };

        let span = fn_token.span.merge(body.span);
        Ok(ExpressionLocation {
            expression: Expression::FunctionDeclaration {
                name: identifier,
                type_parameters,
                parameters: argument_list,
                parameters_span,
                body: Box::new(body),
                return_annotation,
                pure: is_pure,
            },
            span,
            id: NodeId::next(),
        })
    }

    fn struct_declaration(&mut self) -> Result<ExpressionLocation, Error> {
        let struct_start = self.require_token(&[Token::Struct])?;
        let identifier = self.require_identifier()?;

        let (fields, field_span) = self.delimited_comma_separated(
            &Token::LeftCurlyBracket,
            &Token::RightCurlyBracket,
            Self::struct_field,
            true,
        )?;

        Ok(ExpressionLocation {
            id: NodeId::next(),
            expression: Expression::StructDeclaration {
                name: identifier.to_identifier(),
                fields,
            },
            span: struct_start.merge(field_span),
        })
    }

    /// Parses a block expression including the block delimiters `{` and `}`
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

        // Non-last items in a block are in statement position: wrap bare expressions
        // (e.g. `if` without else) in Statement so the compiler discards their values.
        let last = statements.len().saturating_sub(1);
        let statements = statements
            .into_iter()
            .enumerate()
            .map(|(i, stmt)| {
                if i < last && !matches!(stmt.expression, Expression::Statement(_)) {
                    stmt.to_statement()
                } else {
                    stmt
                }
            })
            .collect();

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

    pub fn type_annotation(&mut self) -> Result<TypeExpr, Error> {
        let Some(TokenLocation { token, span }) = self.peek_current_token_location() else {
            return Err(self.unexpected_end_of_input());
        };

        match token {
            Token::Fn => self.function_type(),
            Token::Identifier(name)
                if name == "Fn" && self.peek_next_token() == Some(&Token::LeftParentheses) =>
            {
                Err(Error::with_help(
                    "unknown type `Fn`".to_string(),
                    *span,
                    "A function type is written `fn(A) -> R`, as in `fn(Int) -> String`"
                        .to_string(),
                ))
            }
            Token::Identifier(_) => self.named_or_generic_type(),
            Token::LeftParentheses => self.tuple_type(),
            Token::LeftSquareBracket => self.list_type(),
            Token::MapOpen => self.map_type(),
            _ => Err(Error::with_help(
                format!("expected a type annotation, found `{token}`"),
                *span,
                "Use a valid type name or tuple type annotation in this position.".to_string(),
            )),
        }
    }

    /// The type arguments `::<A, B>` after a name, if any, and the span of
    /// the name with them. `name_span` is the span of the name. After `::<`
    /// a type always follows, so nothing has to be guessed: `f::<Int>(x) < y`
    /// compares.
    fn turbofish(&mut self, name_span: Span) -> Result<(Vec<TypeExpr>, Span), Error> {
        if self.consume_token_if(&[Token::ColonColon]).is_none() {
            return Ok((Vec::new(), name_span));
        }
        // `<>` lexes as one token.
        if let Some(location) = self
            .peek_current_token_location()
            .filter(|location| location.token == Token::Diamond)
        {
            return Err(Error::text(
                "type arguments can't be empty".to_string(),
                location.span,
            ));
        }
        if self.peek_current_token() != Some(&Token::Less) {
            let span = self
                .peek_current_token_location()
                .map_or(name_span, |location| location.span);
            return Err(Error::with_help(
                "expected '<' after '::'".to_string(),
                span,
                "`::` starts type arguments, as in `f::<Int>(x)`".to_string(),
            ));
        }
        let (arguments, close_span) = self.delimited_type_params()?;
        Ok((arguments, name_span.merge(close_span)))
    }

    /// The type parameters `<T, U>` a function declares, the `<` current.
    fn type_parameters(&mut self) -> Result<Vec<TypeParameter>, Error> {
        let (parameters, _) = self.delimited_comma_separated(
            &Token::Less,
            &Token::Greater,
            Self::type_parameter,
            false,
        )?;
        Ok(parameters)
    }

    fn type_parameter(&mut self) -> Result<TypeParameter, Error> {
        let token = self.require_current_token()?;
        let Token::Identifier(name) = token.token else {
            return Err(Error::text(
                format!(
                    "Expected a type parameter but got '{}' instead",
                    token.token
                ),
                token.span,
            ));
        };
        Ok(TypeParameter {
            id: NodeId::next(),
            name,
            span: token.span,
        })
    }

    pub fn named_or_generic_type(&mut self) -> Result<TypeExpr, Error> {
        let Ok(TokenLocation {
            token: Token::Identifier(ident),
            span,
        }) = self.require_current_token()
        else {
            unreachable!("this should have been checked");
        };

        let (args, span) = if self.peek_current_token() == Some(&Token::Less) {
            let (args, close_span) = self.delimited_type_params()?;
            (args, span.merge(close_span))
        } else {
            (Vec::new(), span)
        };

        Ok(TypeExpr::Name {
            id: NodeId::next(),
            name: ident,
            args,
            span,
        })
    }

    /// Parses `<T, U, ...>` type parameter lists, handling the `>>` / `>=` / `>>=`
    /// ambiguity that arises with nested generics like `[[Int]]`. Returns
    /// the parsed items and the span of the closing `>`.
    fn delimited_type_params(&mut self) -> Result<(Vec<TypeExpr>, Span), Error> {
        self.require_current_token_matches(&Token::Less)?;

        let mut items = vec![self.type_annotation()?];

        while self.consume_token_if(&[Token::Comma]).is_some() {
            if self.peek_current_token() == Some(&Token::Greater) {
                break;
            }
            items.push(self.type_annotation()?);
        }

        let close_span = self.consume_closing_angle_bracket()?;
        Ok((items, close_span))
    }

    /// Consumes a closing `>` for a generic type parameter list. If the current
    /// token is `>>`, `>=`, or `>>=`, it is split so that the leading `>` is
    /// consumed and the remainder is left as the current token.
    fn consume_closing_angle_bracket(&mut self) -> Result<Span, Error> {
        if let Some(token) = self.consume_token_if(&[Token::Greater]) {
            return Ok(token.span);
        }

        let Some(loc) = self.peek_current_token_location() else {
            return Err(self.unexpected_end_of_input());
        };

        let greater_span = Span::new(loc.span.source_id(), loc.span.offset(), 1);
        let rest_span = Span::new(
            loc.span.source_id(),
            loc.span.offset() + 1,
            loc.span.end() - loc.span.offset() - 1,
        );

        let remainder = match &loc.token {
            // >> becomes >
            Token::GreaterGreater => Token::Greater,
            // >= becomes =
            Token::GreaterEquals => Token::EqualsSign,
            // >>= (OpAssign(>>)) becomes >=
            Token::OpAssign(inner) if inner.token == Token::GreaterGreater => Token::GreaterEquals,
            _ => {
                let loc = loc.clone();
                return Err(Error::text(
                    format!("Expected token '>' but got '{}' instead", loc.token),
                    loc.span,
                ));
            }
        };

        self.tokens[self.current] = TokenLocation {
            token: remainder,
            span: rest_span,
        };

        Ok(greater_span)
    }

    /// Parses `fn(A, B) -> C`, where `-> C` is optional.
    fn function_type(&mut self) -> Result<TypeExpr, Error> {
        let start = self.require_current_token()?.span;
        let (parameters, parameters_span) = self.delimited_comma_separated(
            &Token::LeftParentheses,
            &Token::RightParentheses,
            Self::type_annotation,
            true,
        )?;
        let (return_type, span) = if self.consume_token_if(&[Token::RightArrow]).is_some() {
            let return_type = self.type_annotation()?;
            let span = start.merge(return_type.span());
            (Some(Box::new(return_type)), span)
        } else {
            (None, start.merge(parameters_span))
        };
        Ok(TypeExpr::Function {
            parameters,
            return_type,
            span,
        })
    }

    /// Parses `()`, `(A, B)` and the 1-tuple `(A,)`. Like in an expression,
    /// parentheses around a single type without a comma only group it, so
    /// `(A)` is `A`.
    pub fn tuple_type(&mut self) -> Result<TypeExpr, Error> {
        let (mut elements, span) = self.delimited_comma_separated(
            &Token::LeftParentheses,
            &Token::RightParentheses,
            Self::type_annotation,
            true,
        )?;
        let trailing_comma = self.tokens[self.current - 2].token == Token::Comma;
        if elements.len() == 1 && !trailing_comma {
            return Ok(elements.remove(0));
        }
        Ok(TypeExpr::Tuple { elements, span })
    }

    /// Parses `%{K: V}`, a map from `K` to `V`, or `%{K}`, a set of `K`.
    fn map_type(&mut self) -> Result<TypeExpr, Error> {
        let open = self.require_current_token_matches(&Token::MapOpen)?;
        let key = self.type_annotation()?;
        let value = if self.consume_token_if(&[Token::Colon]).is_some() {
            Some(Box::new(self.type_annotation()?))
        } else {
            None
        };
        let close = self.require_current_token_matches(&Token::RightCurlyBracket)?;
        Ok(TypeExpr::Map {
            key: Box::new(key),
            value,
            span: open.span.merge(close.span),
        })
    }

    /// Parses `[T]`, a list whose elements are `T`.
    fn list_type(&mut self) -> Result<TypeExpr, Error> {
        let open = self.require_current_token_matches(&Token::LeftSquareBracket)?;
        let element = self.type_annotation()?;
        let close = self.require_current_token_matches(&Token::RightSquareBracket)?;
        Ok(TypeExpr::List {
            element: Box::new(element),
            span: open.span.merge(close.span),
        })
    }

    fn struct_field(&mut self) -> Result<StructField, Error> {
        let ExpressionLocation {
            expression,
            span: identifier_span,
            ..
        } = self.single_expression()?;

        let Expression::Identifier {
            name: identifier, ..
        } = expression
        else {
            return Err(Error::with_help(
                "Expected field name".to_string(),
                identifier_span,
                "Struct field must be identifiers followed by a type annotation (e.g. `x: Int`)."
                    .to_string(),
            ));
        };

        let annotation = if self.peek_current_token() == Some(&Token::Colon) {
            self.advance();
            self.type_annotation()?
        } else {
            return Err(Error::with_help(
                "Expected field type".to_string(),
                identifier_span,
                "Struct fields must have a type annotation".to_string(),
            ));
        };

        Ok(StructField {
            identifier,
            annotation,
            span: identifier_span.merge(self.tokens[self.current - 1].span),
        })
    }
    fn named_parameter(&mut self) -> Result<FunctionParameter, Error> {
        let maybe_lvalue = self.single_expression()?;
        let lvalue_span = maybe_lvalue.span;

        let Some(
            lvalue @ BindingPatternLocation {
                pattern: BindingPattern::Identifier { .. },
                ..
            },
        ) = AssignmentTargetLocation::try_from(maybe_lvalue)
            .ok()
            .and_then(|target| BindingPatternLocation::try_from(target).ok())
        else {
            return Err(Error::with_help(
                "Expected parameter name".to_string(),
                lvalue_span,
                "Function parameters must be identifiers, optionally followed by a type annotation (e.g. `x` or `x: Int`).".to_string(),
            ));
        };

        let annotation = if self.peek_current_token() == Some(&Token::Colon) {
            self.advance();
            Some(self.type_annotation()?)
        } else {
            None
        };

        let span = if annotation.is_some() {
            lvalue_span.merge(self.tokens[self.current - 1].span)
        } else {
            lvalue_span
        };

        Ok(FunctionParameter {
            lvalue,
            annotation,
            span,
        })
    }

    pub fn named_binding(&mut self) -> Result<(BindingPatternLocation, Option<TypeExpr>), Error> {
        let maybe_lvalue = self.tuple_expression(Self::single_expression, false)?;
        let lvalue_span = maybe_lvalue.span;

        let Ok(lvalue) = AssignmentTargetLocation::try_from(maybe_lvalue) else {
            return Err(Error::with_help(
                "Invalid assignment target".to_string(),
                lvalue_span,
                "Assignment target is not a valid lvalue. Only a few expressions can be assigned a value. Check that the left-hand side of the assignment is a valid target.".to_string(),
            ));
        };

        let lvalue = BindingPatternLocation::try_from(lvalue).map_err(|target| {
            let help = match target {
                NonBindingTarget::Member => {
                    "`let` introduces a new binding; assign to a field with `foo.bar = value` instead."
                }
                NonBindingTarget::Index => {
                    "`let` introduces a new binding; assign to an element with `foo[index] = value` instead."
                }
            };
            Error::with_help(
                "Invalid declaration target".to_string(),
                lvalue_span,
                help.to_string(),
            )
        })?;

        let annotated_type = if self.peek_current_token() == Some(&Token::Colon) {
            self.advance();
            Some(self.type_annotation()?)
        } else {
            None
        };

        Ok((lvalue, annotated_type))
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

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct Error {
    text: String,
    help_text: Option<String>,
    span: Span,
}

impl Error {
    #[must_use]
    pub fn text<S>(text: S, span: Span) -> Self
    where
        S: Into<String>,
    {
        Self {
            text: text.into(),
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

    pub fn location(&self) -> Span {
        self.span
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn help_text(&self) -> Option<&str> {
        self.help_text.as_deref()
    }
}

/// The bracket that closes `token`, if it opens one: `)` for `(`, and `}`
/// for both `{` and `%{`.
fn closing_bracket_for(token: &Token) -> Option<Token> {
    match token {
        Token::LeftParentheses => Some(Token::RightParentheses),
        Token::LeftSquareBracket => Some(Token::RightSquareBracket),
        Token::LeftCurlyBracket | Token::MapOpen => Some(Token::RightCurlyBracket),
        _ => None,
    }
}

/// Whether `token` is a keyword, a word with a meaning of its own such as
/// `fn` or `if`, which can't name a variable.
fn is_keyword(token: &Token) -> bool {
    !matches!(
        token,
        Token::Identifier(_) | Token::String(_) | Token::NumericLiteral(_)
    ) && token
        .to_string()
        .chars()
        .all(|c| c.is_ascii_alphabetic() || c == '?')
}

/// The bracket that `token` closes, if it closes one: `(` for `)`.
fn opening_bracket_for(token: &Token) -> Option<&'static str> {
    match token {
        Token::RightParentheses => Some("("),
        Token::RightSquareBracket => Some("["),
        Token::RightCurlyBracket => Some("{"),
        _ => None,
    }
}

/// Whether `token` needs an expression after it, so that an expression
/// missing after it is best explained by naming it: an operator as in
/// `1 + ;`, the `=` of `let x = ;`, or a keyword such as `return` or `in`.
fn expects_expression_after(token: &Token) -> bool {
    matches!(
        token,
        Token::EqualsSign
            | Token::OpAssign(_)
            | Token::FatArrow
            | Token::Colon
            | Token::Return
            | Token::If
            | Token::While
            | Token::LogicAnd
            | Token::LogicOr
            | Token::LogicNot
            | Token::Bang
            | Token::DotDot
            | Token::DotDotEquals
            // The binary operators
            | Token::EqualsEquals
            | Token::BangEquals
            | Token::Greater
            | Token::GreaterEquals
            | Token::Less
            | Token::LessEquals
            | Token::Spaceship
            | Token::InverseSpaceship
            | Token::Plus
            | Token::Minus
            | Token::Asterix
            | Token::ForwardSlash
            | Token::Backslash
            | Token::Percent
            | Token::PercentPercent
            | Token::Caret
            | Token::Ampersand
            | Token::Tilde
            | Token::Pipe
            | Token::In
            | Token::PlusPlus
            | Token::Diamond
            | Token::GreaterGreater
            | Token::LessLess
    )
}

/// The help for an expression missing after `token`, one that
/// `expects_expression_after`.
fn expression_after_help(token: &Token) -> String {
    match token {
        Token::Return => "To return without a value, write `return;`".to_string(),
        Token::EqualsSign | Token::OpAssign(_) => {
            format!("Put the value to assign after the '{token}'")
        }
        Token::Colon => "Every key in a map needs a value after its ':'".to_string(),
        Token::If | Token::While => format!("Put a condition after '{token}'"),
        _ => format!("Put a value after '{token}', or remove the '{token}'"),
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
