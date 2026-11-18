#![allow(unused_crate_dependencies)]

use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;

/// The error for `source`, with the offset it points at.
fn error(source: &str) -> (String, usize, usize) {
    let tokens = Lexer::new(source, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let error = Parser::from_tokens(tokens).parse().unwrap_err();
    (
        error.to_string(),
        error.span().offset(),
        error.span().range().len(),
    )
}

/// A missing `;` is reported right after the statement, where it goes, not
/// at the start of the next one.
#[test]
fn a_missing_semicolon_points_after_the_statement() {
    for (source, before) in [
        ("let a = 5\nlet b = 6;", "let a = 5"),
        ("print(1)\nprint(2);", "print(1)"),
        ("let x = { 5 }\nlet y = { 10 };", "let x = { 5 }"),
        ("let a = [1,\n  2]\nprint(a);", "let a = [1,\n  2]"),
    ] {
        let (_, offset, length) = error(source);
        assert_eq!((offset, length), (before.len(), 0), "{source:?}");
    }
}

#[test]
fn a_comment_after_the_statement_is_skipped() {
    let source = "let a = 5 // five\n\nprint(a);";
    let (_, offset, _) = error(source);
    assert_eq!(offset, "let a = 5".len());
}

#[test]
fn a_missing_semicolon_names_the_next_token_and_how_to_fix_it() {
    let tokens = Lexer::new("print(1)\nprint(2);", SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let error = Parser::from_tokens(tokens).parse().unwrap_err();
    assert_eq!(
        error.to_string(),
        "Expected token ';' but got 'print' instead"
    );
    assert_eq!(error.help_text(), Some("Every statement ends with a `;`"));
}
