//! Formatting a document with `ndc fmt`'s formatter.

use ls_types::{Range, TextEdit};
use ndc_lexer::SourceId;

use crate::util::LineIndex;

/// The edit that formats `text`: one edit of the part that changes, so the
/// cursor and what's scrolled into view stay put around it, or none when
/// `text` is formatted already. The formatter has one style, 120 columns
/// wide, so an editor's tab and indent settings don't change it.
///
/// # Errors
/// When `text` doesn't parse, or formatting it would change what it means,
/// which is a bug in the formatter.
pub fn format(text: &str) -> Result<Vec<TextEdit>, String> {
    let formatted = ndc_fmt::format(text, SourceId::SYNTHETIC).map_err(|error| match error {
        ndc_fmt::Error::Changed(_) => error.to_string(),
        _ => format!("the program can't be formatted until it parses: {error}"),
    })?;
    Ok(changed_part(text, &formatted).into_iter().collect())
}

/// The one edit that turns `before` into `after`: the part between what
/// they start and end with alike.
fn changed_part(before: &str, after: &str) -> Option<TextEdit> {
    if before == after {
        return None;
    }
    let prefix = common_len(before.char_indices(), after.char_indices());
    let suffix = common_len(
        before[prefix..].char_indices().rev(),
        after[prefix..].char_indices().rev(),
    );
    let index = LineIndex::new(before);
    let start = index.position(before, prefix);
    let end = index.position(before, before.len() - suffix);
    Some(TextEdit {
        range: Range { start, end },
        new_text: after[prefix..after.len() - suffix].to_string(),
    })
}

/// How many bytes two texts' characters have alike from where their
/// iterators start, counted in whole characters.
fn common_len(
    a: impl Iterator<Item = (usize, char)>,
    b: impl Iterator<Item = (usize, char)>,
) -> usize {
    a.zip(b)
        .take_while(|((_, x), (_, y))| x == y)
        .map(|((_, c), _)| c.len_utf8())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `text` with the edits `format` gives applied.
    fn formatted(text: &str) -> Result<String, String> {
        let edits = format(text)?;
        let index = LineIndex::new(text);
        let mut result = text.to_string();
        for edit in edits.iter().rev() {
            let start = index.offset(text, edit.range.start).expect("start");
            let end = index.offset(text, edit.range.end).expect("end");
            result.replace_range(start..end, &edit.new_text);
        }
        Ok(result)
    }

    #[test]
    fn formatting_gives_what_ndc_fmt_gives() {
        let text = "let   xs =[1,2 ,3];\nprint( xs );\n";
        let expected = ndc_fmt::format(text, SourceId::SYNTHETIC).expect("formats");
        assert_eq!(formatted(text).as_deref(), Ok(expected.as_str()));
    }

    #[test]
    fn a_formatted_text_needs_no_edit() {
        let text = ndc_fmt::format("let x =1;", SourceId::SYNTHETIC).expect("formats");
        assert_eq!(format(&text), Ok(Vec::new()));
    }

    #[test]
    fn the_edit_covers_only_what_changes() {
        let text = "let a = 1;\nlet b   =2;\nlet c = 3;\n";
        let edits = format(text).expect("formats");
        let [edit] = edits.as_slice() else {
            panic!("one edit: {edits:?}");
        };
        // The first and last lines are left alone.
        assert_eq!(edit.range.start.line, 1);
        assert_eq!(edit.range.end.line, 1);
        assert_eq!(
            formatted(text).as_deref(),
            Ok("let a = 1;\nlet b = 2;\nlet c = 3;\n")
        );
    }

    #[test]
    fn a_change_next_to_text_outside_ascii_lands_in_the_right_place() {
        let text = "let s = \"héé\";\nlet b   =2;\n";
        assert_eq!(
            formatted(text).as_deref(),
            Ok("let s = \"héé\";\nlet b = 2;\n")
        );
    }

    #[test]
    fn a_program_that_does_not_parse_is_not_formatted() {
        let error = format("let x = ;").expect_err("doesn't parse");
        assert!(error.contains("until it parses"), "{error}");
    }
}
