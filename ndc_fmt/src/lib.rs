//! Formats Andy C++ code: `let   xs=[1,2 ,3]` becomes `let xs = [1, 2, 3];`.
//!
//! Only the whitespace between tokens changes, apart from a few rewrites that
//! keep the meaning: a list broken over several lines gets a comma after its
//! last item, and `fn f() => { ... }` becomes `fn f() { ... }`. Lines are kept
//! within [`WIDTH`] columns where breaking them allows.

mod check;
mod doc;
mod format;
mod printer;
mod source;

use format::Formatter;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use source::Source;

/// How many columns a line may take
pub const WIDTH: usize = 120;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Lexer(#[from] ndc_lexer::Error),
    #[error(transparent)]
    Parser(#[from] ndc_parser::Error),
    /// The formatted code would mean something else, which is a bug in the
    /// formatter
    #[error("formatting would change the program: {0}")]
    Changed(String),
}

/// `source` formatted. `source_id` goes into the spans of a lexer or parser
/// error, to report it against the right source.
///
/// # Errors
/// If `source` doesn't lex or parse, or if formatting it would change what it
/// means.
pub fn format(source: &str, source_id: SourceId) -> Result<String, Error> {
    let tokens = Lexer::new(source, source_id).collect::<Result<Vec<_>, _>>()?;
    let program = Parser::from_tokens(tokens.clone()).parse()?;
    let doc = Formatter::new(Source::new(source, tokens)).program(&program);
    let formatted = printer::print(&doc, WIDTH);
    let formatted = if formatted.trim().is_empty() {
        String::new()
    } else {
        formatted
    };
    check::unchanged(&program, source, &formatted)?;
    Ok(formatted)
}
