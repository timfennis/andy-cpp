//! Makes sure formatting leaves the program as it was: the output parses to
//! the same syntax tree, and has the same comments in the same order.

use crate::Error;
use ndc_lexer::{Lexer, SourceId, Token, TokenLocation};
use ndc_parser::{ExpressionLocation, Parser};

pub fn unchanged(
    program: &[ExpressionLocation],
    source: &str,
    formatted: &str,
) -> Result<(), Error> {
    let changed = |what: &str| Err(Error::Changed(what.to_string()));

    let Ok(tokens) = Lexer::new(formatted, SourceId::SYNTHETIC).collect::<Result<Vec<_>, _>>()
    else {
        return changed("the output doesn't lex");
    };
    let Ok(source_tokens) = Lexer::new(source, SourceId::SYNTHETIC).collect::<Result<Vec<_>, _>>()
    else {
        return changed("the source doesn't lex");
    };
    if comments(&tokens, formatted) != comments(&source_tokens, source) {
        return changed("the comments are different");
    }

    let Ok(reparsed) = Parser::from_tokens(tokens).parse() else {
        return changed("the output doesn't parse");
    };
    if without_locations(&format!("{program:?}")) != without_locations(&format!("{reparsed:?}")) {
        return changed("the output parses to a different program");
    }
    Ok(())
}

fn comments<'a>(tokens: &[TokenLocation], source: &'a str) -> Vec<&'a str> {
    tokens
        .iter()
        .filter(|token| matches!(token.token, Token::Comment(_)))
        .map(|token| source[token.span.range()].trim_end())
        .collect()
}

/// The debug output of a syntax tree without its `Span`s and `NodeId`s,
/// which say where in the source each part is rather than what it is.
/// String literals are left alone, whatever they hold.
fn without_locations(debug: &str) -> String {
    let mut out = String::with_capacity(debug.len());
    let mut rest = debug;
    while let Some(c) = rest.chars().next() {
        if c == '"' {
            let end = string_end(rest);
            out.push_str(&rest[..end]);
            rest = &rest[end..];
        } else if let Some(after) = rest.strip_prefix("Span {") {
            rest = &after[after.find('}').map_or(after.len(), |end| end + 1)..];
        } else if let Some(after) = rest.strip_prefix("NodeId(") {
            rest = &after[after.find(')').map_or(after.len(), |end| end + 1)..];
        } else {
            out.push(c);
            rest = &rest[c.len_utf8()..];
        }
    }
    out
}

/// The length of the quoted, escaped string `debug` starts with
fn string_end(debug: &str) -> usize {
    let mut escaped = false;
    for (i, c) in debug.char_indices().skip(1) {
        match c {
            _ if escaped => escaped = false,
            '\\' => escaped = true,
            '"' => return i + 1,
            _ => {}
        }
    }
    debug.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locations_are_left_out_but_strings_are_kept() {
        assert_eq!(
            without_locations(
                r#"A { id: NodeId(3), s: "Span {", span: Span { source_id: SourceId(1), offset: 2, length: 3 } }"#
            ),
            r#"A { id: , s: "Span {", span:  }"#
        );
    }
}
