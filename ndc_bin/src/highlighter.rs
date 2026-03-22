use ahash::AHashSet;
use itertools::Itertools;
use ndc_lexer::{Lexer, SourceId, Token, TokenLocation};
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration};
use yansi::{Paint, Painted};

pub(crate) struct AndycppHighlighter;

impl AndycppHighlighter {
    /// Parser-enhanced highlighting that correctly identifies function names
    /// even in method-call syntax like `foo.len`.
    pub fn highlight_parsed(line: &str) -> Vec<Painted<&str>> {
        let mut function_spans = AHashSet::new();

        let expressions = Lexer::new(line, SourceId::SYNTHETIC)
            .collect::<Result<Vec<TokenLocation>, _>>()
            .ok()
            .and_then(|tokens| ndc_parser::Parser::from_tokens(tokens).parse().ok());

        if let Some(expressions) = expressions {
            for expr in &expressions {
                collect_function_spans(expr, &mut function_spans);
            }
        }

        Self::highlight_tokens(line, &function_spans)
    }

    fn highlight_tokens<'a>(
        line: &'a str,
        function_spans: &AHashSet<usize>,
    ) -> Vec<Painted<&'a str>> {
        let Ok(tokens) = Lexer::new(line, SourceId::SYNTHETIC).collect::<Result<Vec<_>, _>>()
        else {
            return vec![line.red()];
        };

        let mut start = 0;
        let mut ranges = vec![];
        for token in tokens.iter().dropping(1) {
            let end = token.span.offset();
            ranges.push(start..end);
            start = end;
        }
        if !line.is_empty() {
            ranges.push(start..line.len());
        }

        let mut out = Vec::new();
        let pairs: Vec<_> = ranges.into_iter().zip(tokens).collect();
        for (i, (range, token)) in pairs.iter().enumerate() {
            let substring = &line[range.start..(range.start + range.len())];
            let next_token = pairs.get(i + 1).map(|(_, t)| &t.token);

            let colored = match &token.token {
                // Strings — green
                Token::String(_) => substring.rgb(152, 195, 121),
                // Numeric literals and booleans — orange
                Token::BigInt(_)
                | Token::Int64(_)
                | Token::Float64(_)
                | Token::Complex(_)
                | Token::Infinity
                | Token::True
                | Token::False => substring.rgb(209, 154, 102),
                // Keywords — coral red
                Token::Let
                | Token::Fn
                | Token::If
                | Token::Else
                | Token::Return
                | Token::Break
                | Token::Continue
                | Token::For
                | Token::In
                | Token::While
                | Token::Pure
                | Token::LogicAnd
                | Token::LogicOr
                | Token::LogicNot => substring.rgb(224, 108, 117),
                // Function identifiers — yellow/gold
                // Detected by parser (dot-calls, etc.) or by token heuristics as fallback
                Token::Identifier(_) if function_spans.contains(&token.span.offset()) => {
                    substring.rgb(229, 192, 123)
                }
                Token::Identifier(_) if matches!(next_token, Some(Token::LeftParentheses)) => {
                    substring.rgb(229, 192, 123)
                }
                Token::Identifier(_) if i > 0 && matches!(pairs[i - 1].1.token, Token::Fn) => {
                    substring.rgb(229, 192, 123)
                }
                // Variable identifiers — blue
                Token::Identifier(_) => substring.rgb(97, 175, 239),
                // Arrows, fat arrows, and assignment — cyan
                Token::RightArrow | Token::FatArrow | Token::EqualsSign | Token::OpAssign(_) => {
                    substring.rgb(86, 182, 194)
                }
                // Brackets and delimiters — light gray (neutral)
                Token::LeftSquareBracket
                | Token::RightSquareBracket
                | Token::LeftCurlyBracket
                | Token::RightCurlyBracket
                | Token::LeftParentheses
                | Token::RightParentheses
                | Token::MapOpen
                | Token::Semicolon
                | Token::Comma
                | Token::Colon => substring.rgb(171, 178, 191),
                // Operators — purple
                _ => substring.rgb(198, 120, 221),
            };

            out.push(colored);
        }

        out
    }
}

/// Walk the parsed AST and collect the byte offsets of identifiers used as function names.
fn collect_function_spans(expr: &ExpressionLocation, spans: &mut AHashSet<usize>) {
    match &expr.expression {
        Expression::Call {
            function,
            arguments,
        } => {
            if let Expression::Identifier { .. } = &function.expression {
                spans.insert(function.span.offset());
            }
            collect_function_spans(function, spans);
            for arg in arguments {
                collect_function_spans(arg, spans);
            }
        }
        Expression::FunctionDeclaration { body, .. } => {
            collect_function_spans(body, spans);
        }
        Expression::VariableDeclaration { value, .. }
        | Expression::Assignment { r_value: value, .. }
        | Expression::OpAssignment { r_value: value, .. }
        | Expression::Return { value } => {
            collect_function_spans(value, spans);
        }
        Expression::Statement(inner) | Expression::Grouping(inner) => {
            collect_function_spans(inner, spans);
        }
        Expression::Block { statements } => {
            for s in statements {
                collect_function_spans(s, spans);
            }
        }
        Expression::If {
            condition,
            on_true,
            on_false,
        } => {
            collect_function_spans(condition, spans);
            collect_function_spans(on_true, spans);
            if let Some(f) = on_false {
                collect_function_spans(f, spans);
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            collect_function_spans(expression, spans);
            collect_function_spans(loop_body, spans);
        }
        Expression::For { iterations, body } => {
            for iteration in iterations {
                match iteration {
                    ForIteration::Iteration { sequence, .. } => {
                        collect_function_spans(sequence, spans);
                    }
                    ForIteration::Guard(expr) => collect_function_spans(expr, spans),
                }
            }
            match body.as_ref() {
                ForBody::Block(e) | ForBody::List { expr: e, .. } => {
                    collect_function_spans(e, spans);
                }
                ForBody::Map {
                    key,
                    value,
                    default,
                    ..
                } => {
                    collect_function_spans(key, spans);
                    if let Some(v) = value {
                        collect_function_spans(v, spans);
                    }
                    if let Some(d) = default {
                        collect_function_spans(d, spans);
                    }
                }
            }
        }
        Expression::Logical { left, right, .. } => {
            collect_function_spans(left, spans);
            collect_function_spans(right, spans);
        }
        Expression::Tuple { values } | Expression::List { values } => {
            for v in values {
                collect_function_spans(v, spans);
            }
        }
        Expression::Map { values, default } => {
            for (k, v) in values {
                collect_function_spans(k, spans);
                if let Some(v) = v {
                    collect_function_spans(v, spans);
                }
            }
            if let Some(d) = default {
                collect_function_spans(d, spans);
            }
        }
        Expression::RangeInclusive { start, end } | Expression::RangeExclusive { start, end } => {
            if let Some(s) = start {
                collect_function_spans(s, spans);
            }
            if let Some(e) = end {
                collect_function_spans(e, spans);
            }
        }
        // Leaves — no sub-expressions to recurse into
        Expression::BoolLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::Int64Literal(_)
        | Expression::Float64Literal(_)
        | Expression::BigIntLiteral(_)
        | Expression::ComplexLiteral(_)
        | Expression::Identifier { .. }
        | Expression::Break
        | Expression::Continue => {}
    }
}
