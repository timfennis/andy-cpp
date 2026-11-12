//! Lays out a parsed program as a [`Doc`].
//!
//! The output has the same tokens as the source in the same order, so it
//! parses to the same program; only the whitespace between them changes. The
//! exceptions are a trailing comma in a list broken over several lines, the
//! `=>` before a function body in braces, and where comments go.

use crate::doc::{
    Doc, broken, concat, group, hardline, if_break, indent, line, nil, softline, text, will_break,
};
use crate::source::Source;
use ndc_lexer::{Span, Token};
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForBody, ForIteration, FunctionParameter, StructField, TypeExpr,
};
use std::ops::Range;
use std::rc::Rc;

pub struct Formatter<'a> {
    source: Source<'a>,
    /// The index of the next comment to print. Every comment is printed once,
    /// in source order.
    next_comment: usize,
}

/// How a bracketed list is laid out
#[derive(Clone, Copy)]
struct Brackets {
    open: &'static str,
    close: &'static str,
    /// The offset of the closing bracket
    close_at: usize,
    /// A space inside the brackets when on one line: `{ x: Int }`
    padded: bool,
    /// A comma after the last item even on one line: `(1,)`
    always_comma: bool,
    /// Where the opening bracket is, for a list that stays broken if its
    /// first item is on a later line than the bracket: a map written one
    /// entry per line stays that way
    open_at: Option<usize>,
    /// The last item may break over several lines while the others stay on
    /// the line of the opening bracket, as a function passed last does
    hug_last: bool,
}

impl Brackets {
    fn new(open: &'static str, close: &'static str, close_at: usize) -> Self {
        Self {
            open,
            close,
            close_at,
            padded: false,
            always_comma: false,
            open_at: None,
            hug_last: false,
        }
    }
}

impl<'a> Formatter<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            source,
            next_comment: 0,
        }
    }

    pub fn program(mut self, statements: &[ExpressionLocation]) -> Doc {
        let (head, body) = self.statements(statements, self.source.text.len());
        concat([head, concat(body)])
    }

    /// The statements of a program or block, each on its own line unless
    /// they're in a block on one line, with at most one empty line between
    /// them where the source has any, and the comments among them up to `end`.
    /// Comments that follow the opening bracket on its line come back
    /// separately, to go right after it.
    fn statements(&mut self, statements: &[ExpressionLocation], end: usize) -> (Doc, Vec<Doc>) {
        let mut head = Vec::new();
        let mut body = Vec::new();
        for statement in statements {
            let start = self.start_of(statement.span);
            self.comments_among_statements(start, &mut head, &mut body);
            self.separate(&mut body, start);
            body.push(self.expr(statement));
            body.push(self.trailing_comment(statement.span.end(), &Token::Semicolon));
        }
        self.comments_among_statements(end, &mut head, &mut body);
        (concat(head), body)
    }

    fn comments_among_statements(
        &mut self,
        before: usize,
        head: &mut Vec<Doc>,
        body: &mut Vec<Doc>,
    ) {
        while let Some((start, comment)) = self.take_comment_before(before) {
            if self.source.starts_line(start) {
                self.separate(body, start);
                body.push(text(comment));
            } else {
                let after = if body.is_empty() {
                    &mut *head
                } else {
                    &mut *body
                };
                after.push(end_of_line(&comment));
            }
        }
    }

    /// Starts a new line in `body` for what starts at `start`, after an
    /// empty one if the source has one there. In a block on one line, a
    /// space does instead.
    fn separate(&self, body: &mut Vec<Doc>, start: usize) {
        if !body.is_empty() {
            body.push(line());
            if self.source.blank_line_before(start) {
                body.push(line());
            }
        }
    }

    /// Where the code at `span` starts: a `pure fn` starts at `pure`,
    /// although its span starts at `fn`
    fn start_of(&self, span: Span) -> usize {
        let mut start = span.offset();
        while let Some(token) = self.source.token_before(start)
            && token.token == Token::Pure
        {
            start = token.span.offset();
        }
        start
    }

    fn take_comment_before(&mut self, offset: usize) -> Option<(usize, String)> {
        let comment = self.source.comments.get(self.next_comment)?;
        if comment.range.start >= offset {
            return None;
        }
        self.next_comment += 1;
        Some((comment.range.start, comment.text.clone()))
    }

    /// The comments before `offset`: one on its own line goes on a line of
    /// its own, one after code at the end of the line it's on
    fn leading_comments(&mut self, offset: usize) -> Doc {
        let mut docs = Vec::new();
        while let Some((start, comment)) = self.take_comment_before(offset) {
            if self.source.starts_line(start) {
                docs.push(text(comment));
                docs.push(hardline());
            } else {
                docs.push(end_of_line(&comment));
            }
        }
        concat(docs)
    }

    /// A comment on the same line as `end` with nothing between them but
    /// `separator`, as in `let x = 1; // one`
    fn trailing_comment(&mut self, end: usize, separator: &Token) -> Doc {
        let Some(comment) = self.source.comments.get(self.next_comment) else {
            return nil();
        };
        let between = end..comment.range.start;
        if comment.range.start < end
            || self.source.spans_lines(between.clone())
            || !self
                .source
                .tokens_in(between)
                .iter()
                .all(|token| &token.token == separator)
        {
            return nil();
        }
        let doc = end_of_line(&comment.text);
        self.next_comment += 1;
        doc
    }

    fn expr(&mut self, expression: &ExpressionLocation) -> Doc {
        self.node(expression, true)
    }

    /// `expression` with the comments before it. `grouped` is false for the
    /// branches of an `if`, which break together with the `if` they're in.
    fn node(&mut self, expression: &ExpressionLocation, grouped: bool) -> Doc {
        let leading = self.leading_comments(expression.span.offset());
        let (range, parentheses) = self.source.without_parentheses(expression.span);
        let close_at = range.end.saturating_sub(1);

        let doc = match &expression.expression {
            Expression::BoolLiteral(value) => text(value.to_string()),
            Expression::NoneLiteral => text("None"),
            Expression::StringLiteral(_) | Expression::NumericLiteral(_) => {
                text(&self.source.text[range])
            }
            Expression::Identifier { name, .. } => text(name),
            Expression::Statement(inner) => {
                let semicolon = self
                    .source
                    .token_after(expression.span.end())
                    .is_some_and(|token| token.token == Token::Semicolon);
                concat([self.expr(inner), if semicolon { text(";") } else { nil() }])
            }
            Expression::Logical { .. } => self.operator_chain(expression),
            Expression::Grouping(inner) => concat([text("("), self.expr(inner), text(")")]),
            Expression::Cast { value, annotation } => {
                concat([self.expr(value), text(" as "), type_expr(annotation)])
            }
            Expression::VariableDeclaration {
                l_value,
                annotated_type,
                value,
            } => concat([
                text("let "),
                self.pattern(l_value),
                annotated_type.as_ref().map_or_else(nil, |annotation| {
                    concat([text(": "), type_expr(annotation)])
                }),
                text(" = "),
                self.expr(value),
            ]),
            Expression::Assignment { l_value, r_value } => {
                concat([self.target(l_value), text(" = "), self.expr(r_value)])
            }
            Expression::OpAssignment {
                l_value, r_value, ..
            } => {
                let target = self.target(l_value);
                let operator = self.token_text_after(l_value.span.end());
                concat([
                    target,
                    text(" "),
                    text(operator),
                    text(" "),
                    self.expr(r_value),
                ])
            }
            Expression::FunctionDeclaration {
                name,
                parameters,
                parameters_span,
                body,
                return_annotation,
                pure,
            } => self.function(
                name.as_deref(),
                parameters,
                *parameters_span,
                body,
                return_annotation.as_ref(),
                *pure,
            ),
            Expression::StructDeclaration { name, fields } => {
                let open_at = self
                    .source
                    .tokens_in(range)
                    .iter()
                    .find(|token| token.token == Token::LeftCurlyBracket)
                    .map(|token| token.span.offset());
                let brackets = Brackets {
                    padded: true,
                    open_at,
                    ..Brackets::new("{", "}", close_at)
                };
                let fields = self.delimited(brackets, fields, Self::field);
                concat([text("struct "), text(name), text(" "), fields])
            }
            Expression::Block { statements } => self.block(statements, range, grouped),
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                let mut docs = vec![
                    text("if "),
                    self.expr(condition),
                    text(" "),
                    self.node(on_true, false),
                ];
                if let Some(on_false) = on_false {
                    docs.push(text(" else "));
                    docs.push(self.node(on_false, false));
                }
                match (grouped, self.branches_span_lines(expression)) {
                    (true, true) => broken(&group(concat(docs))),
                    (true, false) => group(concat(docs)),
                    (false, _) => concat(docs),
                }
            }
            Expression::While {
                expression,
                loop_body,
            } => concat([
                text("while "),
                self.expr(expression),
                text(" "),
                self.expr(loop_body),
            ]),
            Expression::For { iterations, body } => self.for_expression(iterations, body),
            Expression::Call {
                function,
                arguments,
            } => {
                if let Expression::Identifier { name, .. } = &function.expression
                    && name == "[]"
                    && let [value, index] = arguments.as_slice()
                {
                    concat([self.expr(value), text("["), self.expr(index), text("]")])
                } else {
                    let function = self.expr(function);
                    concat([function, self.arguments(arguments, close_at)])
                }
            }
            Expression::MethodCall {
                function,
                arguments,
            } => {
                let (receiver, arguments) = arguments
                    .split_first()
                    .expect("a method call has a receiver");
                let receiver = self.expr(receiver);
                let Expression::Identifier { name, .. } = &function.expression else {
                    unreachable!("a method is named");
                };
                concat([
                    receiver,
                    text("."),
                    text(name),
                    self.arguments(arguments, close_at),
                ])
            }
            Expression::OperatorCall {
                function,
                arguments,
            } => match arguments.as_slice() {
                [_, _] => self.operator_chain(expression),
                // `x not in xs`: `not` follows its operand
                [operand] if function.span.offset() > operand.span.offset() => {
                    let Expression::OperatorCall {
                        function: inner,
                        arguments,
                    } = &operand.expression
                    else {
                        unreachable!("an infix `not` negates an operator");
                    };
                    let [left, right] = arguments.as_slice() else {
                        unreachable!("an infix `not` negates a binary operator");
                    };
                    let left = self.expr(left);
                    let operator = self.source.slice(inner.span);
                    group(concat([
                        left,
                        indent(concat([
                            line(),
                            text("not "),
                            text(operator),
                            text(" "),
                            self.expr(right),
                        ])),
                    ]))
                }
                [operand] => {
                    let operator = self.source.slice(function.span);
                    let space = if operator == "not" { " " } else { "" };
                    concat([text(operator), text(space), self.expr(operand)])
                }
                _ => unreachable!("an operator takes one or two operands"),
            },
            Expression::MemberAccess {
                receiver, member, ..
            } => concat([self.expr(receiver), text("."), text(member)]),
            Expression::Tuple { values } if values.is_empty() => text("()"),
            Expression::Tuple { values } if parentheses => {
                let brackets = Brackets {
                    always_comma: values.len() == 1,
                    ..Brackets::new("(", ")", expression.span.end() - 1)
                };
                self.delimited(brackets, values, Self::item)
            }
            Expression::Tuple { values } => {
                let mut docs = Vec::new();
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        docs.push(text(", "));
                    }
                    docs.push(self.expr(value));
                }
                if values.len() == 1 {
                    docs.push(text(","));
                }
                concat(docs)
            }
            Expression::List { values } => {
                self.delimited(Brackets::new("[", "]", close_at), values, Self::item)
            }
            Expression::Map { values, default } => {
                let brackets = Brackets {
                    open_at: Some(range.start),
                    ..Brackets::new("%{", "}", close_at)
                };
                let entries: Vec<MapEntry<'_>> = default
                    .iter()
                    .map(|default| MapEntry::Default(default))
                    .chain(
                        values
                            .iter()
                            .map(|(key, value)| MapEntry::Pair(key, value.as_ref())),
                    )
                    .collect();
                self.delimited(brackets, &entries, Self::map_entry)
            }
            Expression::Return { value } => {
                // `return;` returns a `()` that isn't in the source
                if value.span.offset() == expression.span.offset() {
                    text("return")
                } else {
                    concat([text("return "), self.expr(value)])
                }
            }
            Expression::Break => text("break"),
            Expression::Continue => text("continue"),
            Expression::RangeInclusive { start, end } => {
                self.range(start.as_deref(), "..=", end.as_deref())
            }
            Expression::RangeExclusive { start, end } => {
                self.range(start.as_deref(), "..", end.as_deref())
            }
        };

        // A tuple's parentheses are its own, and a statement's are its expression's
        let doc = if parentheses
            && !matches!(
                expression.expression,
                Expression::Tuple { .. } | Expression::Grouping(_) | Expression::Statement(_)
            ) {
            concat([text("("), doc, text(")")])
        } else {
            doc
        };
        concat([leading, doc])
    }

    fn item(&mut self, expression: &ExpressionLocation) -> (Doc, Span) {
        (self.expr(expression), expression.span)
    }

    fn field(&mut self, field: &StructField) -> (Doc, Span) {
        let leading = self.leading_comments(field.span.offset());
        let doc = concat([
            leading,
            text(&field.identifier),
            text(": "),
            type_expr(&field.annotation),
        ]);
        (doc, field.span)
    }

    fn parameter(&mut self, parameter: &FunctionParameter) -> (Doc, Span) {
        let leading = self.leading_comments(parameter.span.offset());
        let annotation = parameter
            .annotation
            .as_ref()
            .map_or_else(nil, |annotation| {
                concat([text(": "), type_expr(annotation)])
            });
        let doc = concat([leading, self.pattern(&parameter.lvalue), annotation]);
        (doc, parameter.span)
    }

    fn map_entry(&mut self, entry: &MapEntry<'_>) -> (Doc, Span) {
        match entry {
            MapEntry::Default(default) => (concat([text(":"), self.expr(default)]), default.span),
            MapEntry::Pair(key, None) => self.item(key),
            MapEntry::Pair(key, Some(value)) => {
                let key_doc = self.expr(key);
                let doc = concat([key_doc, text(": "), self.expr(value)]);
                (doc, key.span.merge(value.span))
            }
        }
    }

    /// Items between brackets, on one line if they fit and otherwise each on
    /// its own line with a comma after every one
    fn delimited<T>(
        &mut self,
        brackets: Brackets,
        items: &[T],
        mut item: impl FnMut(&mut Self, &T) -> (Doc, Span),
    ) -> Doc {
        let mut docs = Vec::new();
        let mut spans = Vec::new();
        let mut stays_broken = false;
        for (i, value) in items.iter().enumerate() {
            let (doc, span) = item(self, value);
            if i == 0
                && let Some(open_at) = brackets.open_at
            {
                stays_broken = self.source.spans_lines(open_at..span.offset());
            }
            let trailing = self.trailing_comment(span.end(), &Token::Comma);
            docs.push(Rc::new(concat([doc, trailing])));
            spans.push(span);
        }

        // Comments before the closing bracket
        let mut dangling = Vec::new();
        while let Some((start, comment)) = self.take_comment_before(brackets.close_at) {
            if self.source.starts_line(start) {
                dangling.push(hardline());
                dangling.push(text(comment));
            } else {
                dangling.push(end_of_line(&comment));
            }
        }

        if items.is_empty() && dangling.is_empty() {
            return text(format!("{}{}", brackets.open, brackets.close));
        }

        // Each item on its own line
        let mut lines = Vec::new();
        for (i, doc) in docs.iter().enumerate() {
            if i > 0 {
                lines.push(text(","));
                lines.push(line());
            }
            lines.push(Doc::Shared(doc.clone()));
        }
        if !items.is_empty() {
            lines.push(if brackets.always_comma {
                text(",")
            } else {
                if_break(text(","), nil())
            });
        }
        let hug_last = brackets.hug_last
            && dangling.is_empty()
            && !docs
                .split_last()
                .is_some_and(|(_, others)| others.iter().any(|doc| will_break(doc)))
            && !self.comment_before_last(&spans);
        let edge = if brackets.padded { line } else { softline };
        let inside = if items.is_empty() {
            concat(dangling)
        } else {
            concat([edge(), concat(lines), concat(dangling)])
        };
        let mut layout = group(concat([
            text(brackets.open),
            indent(inside),
            edge(),
            text(brackets.close),
        ]));
        if stays_broken {
            layout = broken(&layout);
        }

        // `f(x, fn() {` ... `})`: the last item may break on its own
        let Some((last, others)) = docs.split_last().filter(|_| hug_last) else {
            return layout;
        };
        let mut one_line = vec![text(brackets.open)];
        for doc in others {
            one_line.push(Doc::Shared(doc.clone()));
            one_line.push(text(", "));
        }
        let mut last_broken = one_line.clone();
        one_line.push(Doc::Shared(last.clone()));
        one_line.push(text(brackets.close));
        last_broken.push(broken(last));
        last_broken.push(text(brackets.close));
        Doc::Conditional(vec![concat(one_line), concat(last_broken), broken(&layout)])
    }

    /// Whether a comment comes right before the last of the items at `spans`
    fn comment_before_last(&self, spans: &[Span]) -> bool {
        let Some(last) = spans.last() else {
            return false;
        };
        let start = self.start_of(*last);
        let after = self
            .source
            .token_before(start)
            .map_or(0, |token| token.span.end());
        self.source.has_comment_in(after..start)
    }

    fn arguments(&mut self, arguments: &[ExpressionLocation], close_at: usize) -> Doc {
        let hug_last = arguments.last().is_some_and(|last| {
            !self.parenthesized_span(last.span)
                && match &last.expression {
                    Expression::FunctionDeclaration { body, .. } => {
                        matches!(body.expression, Expression::Block { .. })
                    }
                    Expression::List { .. } | Expression::Map { .. } => true,
                    Expression::For { body, .. } => !matches!(**body, ForBody::Block(_)),
                    _ => false,
                }
        });
        let brackets = Brackets {
            hug_last,
            ..Brackets::new("(", ")", close_at)
        };
        self.delimited(brackets, arguments, Self::item)
    }

    /// `{ ... }`: on one line if it is in the source and still fits, and
    /// otherwise with each statement on its own line. When it's a branch of
    /// an `if`, the `if` decides.
    fn block(
        &mut self,
        statements: &[ExpressionLocation],
        range: Range<usize>,
        grouped: bool,
    ) -> Doc {
        let multiline = self.source.spans_lines(range.clone());
        let (head, body) = self.statements(statements, range.end - 1);
        if body.is_empty() {
            return maybe_group(concat([text("{"), head, softline(), text("}")]), grouped);
        }
        let doc = concat([
            text("{"),
            head,
            indent(concat([line(), concat(body)])),
            line(),
            text("}"),
        ]);
        match (grouped, multiline) {
            (true, true) => broken(&group(doc)),
            (true, false) => group(doc),
            (false, _) => doc,
        }
    }

    /// Whether a block of the `if` at `expression`, or of an `else if` after
    /// it, is written over several lines
    fn branches_span_lines(&self, expression: &ExpressionLocation) -> bool {
        match &expression.expression {
            Expression::If {
                on_true, on_false, ..
            } => {
                self.branches_span_lines(on_true)
                    || on_false
                        .as_deref()
                        .is_some_and(|on_false| self.branches_span_lines(on_false))
            }
            Expression::Block { statements } => {
                !statements.is_empty() && self.source.spans_lines(expression.span.range())
            }
            _ => false,
        }
    }

    fn function(
        &mut self,
        name: Option<&str>,
        parameters: &[FunctionParameter],
        parameters_span: Span,
        body: &ExpressionLocation,
        return_annotation: Option<&TypeExpr>,
        pure: bool,
    ) -> Doc {
        let brackets = Brackets::new("(", ")", parameters_span.end() - 1);
        let mut docs = vec![text(if pure { "pure fn" } else { "fn" })];
        if let Some(name) = name {
            docs.push(text(" "));
            docs.push(text(name));
        }
        docs.push(self.delimited(brackets, parameters, Self::parameter));
        if let Some(annotation) = return_annotation {
            docs.push(text(" -> "));
            docs.push(type_expr(annotation));
        }
        if matches!(body.expression, Expression::Block { .. })
            && !self.parenthesized_span(body.span)
        {
            docs.push(text(" "));
        } else {
            docs.push(text(" => "));
        }
        docs.push(self.expr(body));
        concat(docs)
    }

    fn for_expression(&mut self, iterations: &[ForIteration], body: &ForBody) -> Doc {
        match body {
            ForBody::Block(block) => {
                let iterations = self.iterations(iterations);
                concat([text("for "), iterations, text(" "), self.expr(block)])
            }
            ForBody::List { expr } => {
                let value = self.expr(expr);
                self.comprehension("[", value, iterations)
            }
            ForBody::Map {
                key,
                value,
                default,
            } => {
                let mut docs = Vec::new();
                if let Some(default) = default {
                    docs.push(text(":"));
                    docs.push(self.expr(default));
                    docs.push(text(","));
                    docs.push(line());
                }
                docs.push(self.expr(key));
                if let Some(value) = value {
                    docs.push(text(": "));
                    docs.push(self.expr(value));
                }
                self.comprehension("%{", concat(docs), iterations)
            }
        }
    }

    /// `[x * x for x in xs]`, with `for` on a line of its own if it doesn't fit
    fn comprehension(&mut self, open: &str, value: Doc, iterations: &[ForIteration]) -> Doc {
        let close = if open == "[" { "]" } else { "}" };
        let iterations = self.iterations(iterations);
        group(concat([
            text(open),
            indent(concat([
                softline(),
                value,
                line(),
                text("for "),
                iterations,
            ])),
            softline(),
            text(close),
        ]))
    }

    fn iterations(&mut self, iterations: &[ForIteration]) -> Doc {
        let mut docs = Vec::new();
        for (i, iteration) in iterations.iter().enumerate() {
            if i > 0 {
                docs.push(text(", "));
            }
            match iteration {
                ForIteration::Iteration { l_value, sequence } => {
                    docs.push(self.pattern(l_value));
                    docs.push(text(" in "));
                    docs.push(self.expr(sequence));
                }
                ForIteration::Guard(guard) => {
                    docs.push(text("if "));
                    docs.push(self.expr(guard));
                }
            }
        }
        concat(docs)
    }

    fn range(
        &mut self,
        start: Option<&ExpressionLocation>,
        operator: &str,
        end: Option<&ExpressionLocation>,
    ) -> Doc {
        let start = start.map_or_else(nil, |start| self.expr(start));
        let end = end.map_or_else(nil, |end| self.expr(end));
        concat([start, text(operator), end])
    }

    /// `a + b + c` or `a and b and c`; a long chain breaks before each
    /// operator. Operands joined by the same operator line up.
    fn operator_chain(&mut self, expression: &ExpressionLocation) -> Doc {
        // The chain `((a + b) + c)` is `a`, then `+ b` and `+ c`. It stops at
        // another operator and at parentheses: `(a + b) * c` is `(a + b)`, `* c`.
        let mut rest: Vec<(&str, &ExpressionLocation)> = Vec::new();
        let mut leftmost = expression;
        while let Some((operator, left, right)) = self.binary(leftmost)
            && rest
                .last()
                .is_none_or(|(previous, _)| *previous == operator)
            && (rest.is_empty() || !self.parenthesized_span(leftmost.span))
        {
            rest.push((operator, right));
            leftmost = left;
        }

        let first = self.expr(leftmost);
        let mut docs = Vec::new();
        for (operator, right) in rest.into_iter().rev() {
            docs.push(line());
            docs.push(text(operator));
            docs.push(text(" "));
            docs.push(self.expr(right));
        }
        group(concat([first, indent(concat(docs))]))
    }

    /// The operator and operands of `left op right`
    fn binary<'e>(
        &self,
        expression: &'e ExpressionLocation,
    ) -> Option<(&'a str, &'e ExpressionLocation, &'e ExpressionLocation)> {
        match &expression.expression {
            Expression::OperatorCall {
                function,
                arguments,
            } if arguments.len() == 2 => Some((
                self.source.slice(function.span),
                &arguments[0],
                &arguments[1],
            )),
            Expression::Logical { left, right, .. } => {
                Some((self.token_text_after(left.span.end()), left, right))
            }
            _ => None,
        }
    }

    fn parenthesized_span(&self, span: Span) -> bool {
        self.source.without_parentheses(span).1
    }

    /// The source text of the token at or after `offset`
    fn token_text_after(&self, offset: usize) -> &'a str {
        let token = self
            .source
            .token_after(offset)
            .expect("an operator follows its left operand");
        self.source.slice(token.span)
    }

    fn pattern(&mut self, pattern: &BindingPatternLocation) -> Doc {
        match &pattern.pattern {
            BindingPattern::Identifier { identifier, .. } => {
                self.keep_parentheses(pattern.span, text(identifier))
            }
            BindingPattern::Sequence(items) => {
                let items = items.iter().map(|item| self.pattern(item)).collect();
                self.sequence(pattern.span, items)
            }
        }
    }

    fn target(&mut self, target: &AssignmentTargetLocation) -> Doc {
        let doc = match &target.target {
            AssignmentTarget::Identifier { identifier, .. } => text(identifier),
            AssignmentTarget::Index { value, index } => {
                concat([self.expr(value), text("["), self.expr(index), text("]")])
            }
            AssignmentTarget::Sequence(items) => {
                let items = items.iter().map(|item| self.target(item)).collect();
                return self.sequence(target.span, items);
            }
            AssignmentTarget::Member {
                receiver, member, ..
            } => concat([self.expr(receiver), text("."), text(member)]),
        };
        self.keep_parentheses(target.span, doc)
    }

    /// `doc` in parentheses if `span` is: `(s.x) = 1` keeps them
    fn keep_parentheses(&self, span: Span, doc: Doc) -> Doc {
        if self.parenthesized_span(span) {
            concat([text("("), doc, text(")")])
        } else {
            doc
        }
    }

    /// `(a, b)`, `[a, b]` or `a, b`, as in the source
    fn sequence(&self, span: Span, items: Vec<Doc>) -> Doc {
        let (range, parentheses) = self.source.without_parentheses(span);
        let one = items.len() == 1;
        let (open, close) = if parentheses {
            ("(", if one { ",)" } else { ")" })
        } else if self
            .source
            .bracketed(&range, &Token::LeftSquareBracket)
            .is_some()
        {
            ("[", "]")
        } else {
            ("", "")
        };
        let mut docs = vec![text(open)];
        for (i, item) in items.into_iter().enumerate() {
            if i > 0 {
                docs.push(text(", "));
            }
            docs.push(item);
        }
        docs.push(text(close));
        concat(docs)
    }
}

enum MapEntry<'e> {
    Default(&'e ExpressionLocation),
    Pair(&'e ExpressionLocation, Option<&'e ExpressionLocation>),
}

fn type_expr(annotation: &TypeExpr) -> Doc {
    text(annotation.to_string())
}

/// A comment after code, at the end of its line
fn end_of_line(comment: &str) -> Doc {
    concat([Doc::LineSuffix(format!(" {comment}")), Doc::BreakParent])
}

fn maybe_group(doc: Doc, grouped: bool) -> Doc {
    if grouped { group(doc) } else { doc }
}
