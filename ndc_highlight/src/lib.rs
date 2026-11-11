//! What each piece of a program is, for colouring it.
//!
//! `let n = len(xs); // count` comes out as `let` a keyword, `n` a variable,
//! `=` an assignment, `len` a function, `(` `)` `;` punctuation, `xs` a
//! variable and `// count` a comment. Whitespace between them is left out.

use ahash::AHashSet;
use ndc_lexer::{Lexer, SourceId, Token, TokenLocation};
use ndc_parser::{Expression, ExpressionLocation};
use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    /// `"text"`, `r#"text"#`
    String,
    /// `42`, `1.5`, `0xff`, `true`, `False`, `None`
    Literal,
    /// `let`, `fn`, `if`, `and`, …
    Keyword,
    /// A name that is called or declared as a function: `len` in `len(xs)`,
    /// in `xs.len` and in `fn len`.
    Function,
    /// Any other name.
    Variable,
    /// `=`, `+=`, `->`, `=>`
    Assignment,
    /// Brackets, `;`, `,`, `:`
    Punctuation,
    /// `+`, `==`, `..`, …
    Operator,
    /// `// text` and `#! text`
    Comment,
    /// Everything from where the lexer gave up, such as an unclosed string.
    Invalid,
}

/// One piece of the source and what it is.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Highlight {
    /// The byte range in the source.
    pub range: Range<usize>,
    pub kind: Kind,
}

/// The pieces of `source` in order. Code that fails to lex is highlighted up
/// to the error; code that fails to parse still gets highlighted by token.
#[must_use]
pub fn highlight(source: &str) -> Vec<Highlight> {
    let mut tokens = Vec::new();
    let mut invalid_from = None;
    for token in Lexer::new(source, SourceId::SYNTHETIC) {
        match token {
            Ok(token) => tokens.push(token),
            Err(err) => {
                invalid_from = Some(err.span().offset());
                break;
            }
        }
    }

    let function_spans = if invalid_from.is_none() {
        function_spans(tokens.clone())
    } else {
        AHashSet::new()
    };

    let mut highlights = Vec::new();
    let mut end_of_previous = 0;
    for (i, token) in tokens.iter().enumerate() {
        let range = token.span.range();
        comments(source, end_of_previous..range.start, &mut highlights);
        end_of_previous = range.end;

        let next = tokens.get(i + 1).map(|t| &t.token);
        let previous = i.checked_sub(1).map(|i| &tokens[i].token);
        let kind = kind(token, previous, next, &function_spans);
        highlights.push(Highlight { range, kind });
    }

    match invalid_from {
        Some(start) => {
            comments(source, end_of_previous..start, &mut highlights);
            highlights.push(Highlight {
                range: start..source.len(),
                kind: Kind::Invalid,
            });
        }
        None => comments(source, end_of_previous..source.len(), &mut highlights),
    }
    highlights
}

fn kind(
    token: &TokenLocation,
    previous: Option<&Token>,
    next: Option<&Token>,
    function_spans: &AHashSet<usize>,
) -> Kind {
    match &token.token {
        Token::String(_) => Kind::String,
        Token::NumericLiteral(_) | Token::True | Token::False | Token::None => Kind::Literal,
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
        | Token::LogicNot => Kind::Keyword,
        // The parser finds calls and member accesses; the neighbouring tokens
        // cover declarations and code that doesn't parse yet.
        Token::Identifier(_)
            if function_spans.contains(&token.span.offset())
                || matches!(next, Some(Token::LeftParentheses))
                || matches!(previous, Some(Token::Fn)) =>
        {
            Kind::Function
        }
        Token::Identifier(_) => Kind::Variable,
        Token::RightArrow | Token::FatArrow | Token::EqualsSign | Token::OpAssign(_) => {
            Kind::Assignment
        }
        Token::LeftSquareBracket
        | Token::RightSquareBracket
        | Token::LeftCurlyBracket
        | Token::RightCurlyBracket
        | Token::LeftParentheses
        | Token::RightParentheses
        | Token::MapOpen
        | Token::Semicolon
        | Token::Comma
        | Token::Colon => Kind::Punctuation,
        _ => Kind::Operator,
    }
}

/// The comments in `gap`, a stretch between tokens that holds only
/// whitespace and comments. Each runs to the end of its line.
fn comments(source: &str, gap: Range<usize>, highlights: &mut Vec<Highlight>) {
    let mut rest = gap.start;
    while let Some(found) = ["//", "#!"]
        .iter()
        .filter_map(|marker| source[rest..gap.end].find(marker))
        .min()
    {
        let start = rest + found;
        let end = source[start..gap.end]
            .find('\n')
            .map_or(gap.end, |newline| start + newline);
        highlights.push(Highlight {
            range: start..end,
            kind: Kind::Comment,
        });
        rest = end;
    }
}

/// The byte offsets of identifiers the parser sees used as functions.
fn function_spans(tokens: Vec<TokenLocation>) -> AHashSet<usize> {
    let mut spans = AHashSet::new();
    if let Ok(expressions) = ndc_parser::Parser::from_tokens(tokens).parse() {
        for expression in &expressions {
            collect_function_spans(expression, &mut spans);
        }
    }
    spans
}

fn collect_function_spans(expr: &ExpressionLocation, spans: &mut AHashSet<usize>) {
    match &expr.expression {
        Expression::Call { function, .. }
        | Expression::OperatorCall { function, .. }
        | Expression::MethodCall { function, .. }
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

    fn kinds(source: &str) -> Vec<(&str, Kind)> {
        highlight(source)
            .into_iter()
            .map(|h| (&source[h.range], h.kind))
            .collect()
    }

    #[test]
    fn number_literals_are_one_literal() {
        for literal in ["42n", "1.25n", "0b101n", "0o77n", "0xffn"] {
            assert_eq!(kinds(literal), [(literal, Kind::Literal)]);
        }
    }

    #[test]
    fn a_member_name_nested_in_a_loop_or_lambda_is_a_function() {
        // A member is not followed by `(`, so only the parsed AST marks it.
        for (line, member) in [
            ("for x in xs { x.len }", "len"),
            ("let f = fn(s) => s.trim;", "trim"),
        ] {
            assert!(
                kinds(line).contains(&(member, Kind::Function)),
                "{member} in {line:?}: {:?}",
                kinds(line)
            );
        }
    }

    #[test]
    fn comments_are_found_between_tokens() {
        assert_eq!(
            kinds("// hi\nlet x = 1; #! there\n// last"),
            [
                ("// hi", Kind::Comment),
                ("let", Kind::Keyword),
                ("x", Kind::Variable),
                ("=", Kind::Assignment),
                ("1", Kind::Literal),
                (";", Kind::Punctuation),
                ("#! there", Kind::Comment),
                ("// last", Kind::Comment),
            ]
        );
    }

    #[test]
    fn a_comment_marker_inside_a_string_is_part_of_the_string() {
        assert_eq!(kinds(r#""a // b""#), [(r#""a // b""#, Kind::String)]);
    }

    #[test]
    fn code_after_a_lexer_error_is_invalid() {
        assert_eq!(
            kinds("let s = \"open"),
            [
                ("let", Kind::Keyword),
                ("s", Kind::Variable),
                ("=", Kind::Assignment),
                ("\"open", Kind::Invalid),
            ]
        );
    }

    #[test]
    fn code_that_does_not_parse_is_still_highlighted() {
        assert_eq!(
            kinds("fn f(x"),
            [
                ("fn", Kind::Keyword),
                ("f", Kind::Function),
                ("(", Kind::Punctuation),
                ("x", Kind::Variable),
            ]
        );
    }
}
