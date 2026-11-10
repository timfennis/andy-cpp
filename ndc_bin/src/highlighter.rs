use ahash::AHashSet;
use itertools::Itertools;
use ndc_lexer::{Lexer, SourceId, Token, TokenLocation};
use ndc_parser::{Expression, ExpressionLocation};
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
                // Numeric literals, booleans and None — orange
                Token::NumericLiteral(_) | Token::True | Token::False | Token::None => {
                    substring.rgb(209, 154, 102)
                }
                // Keywords — coral red
                Token::Let
                | Token::Fn
                | Token::Struct
                | Token::If
                | Token::Else
                | Token::Return
                | Token::Break
                | Token::Continue
                | Token::For
                | Token::In
                | Token::While
                | Token::Pure
                | Token::As
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
        Expression::Call { function, .. } | Expression::OperatorCall { function, .. }
            if matches!(function.expression, Expression::Identifier { .. }) =>
        {
            spans.insert(function.span.offset());
        }
        Expression::MemberAccess { member_span, .. } => {
            spans.insert(member_span.offset());
        }
        _ => {}
    }
    for child in expr.children() {
        collect_function_spans(child, spans);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yansi::Color;

    #[test]
    fn number_literals_are_highlighted_as_numbers() {
        for literal in ["42n", "1.25n", "0b101n", "0o77n", "0xffn"] {
            let highlighted = AndycppHighlighter::highlight_parsed(literal);

            assert_eq!(
                highlighted.len(),
                1,
                "unexpected tokenization for {literal}"
            );
            assert_eq!(highlighted[0].value, literal);
            assert_eq!(
                highlighted[0].style.foreground,
                Some(Color::Rgb(209, 154, 102)),
                "unexpected color for {literal}"
            );
        }
    }
}
