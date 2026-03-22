use ndc_lexer::Span;
use tower_lsp::lsp_types::{Position, Range};

pub fn span_to_range(text: &str, span: Span) -> Range {
    Range {
        start: position_from_offset(text, span.offset()),
        end: position_from_offset(text, span.end()),
    }
}

pub fn position_from_offset(text: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut col = 0;
    let mut byte_count = 0;

    for c in text.chars() {
        let char_len = c.len_utf8();
        if byte_count >= offset {
            break;
        }

        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }

        byte_count += char_len;
    }

    Position {
        line,
        character: col,
    }
}

/// Convert an LSP `Position` (line, character) to a byte offset in the source text.
pub fn offset_from_position(text: &str, pos: Position) -> Option<usize> {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, c) in text.char_indices() {
        if line == pos.line && col == pos.character {
            return Some(i);
        }
        if c == '\n' {
            if line == pos.line {
                return Some(i);
            }
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    if line == pos.line && col == pos.character {
        return Some(text.len());
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset_to_position_single_line() {
        let text = "hello";
        assert_eq!(position_from_offset(text, 0), Position::new(0, 0));
        assert_eq!(position_from_offset(text, 3), Position::new(0, 3));
        assert_eq!(position_from_offset(text, 5), Position::new(0, 5));
    }

    #[test]
    fn offset_to_position_multiline() {
        let text = "ab\ncd\nef";
        assert_eq!(position_from_offset(text, 0), Position::new(0, 0));
        assert_eq!(position_from_offset(text, 2), Position::new(0, 2));
        // offset 3 is start of second line
        assert_eq!(position_from_offset(text, 3), Position::new(1, 0));
        assert_eq!(position_from_offset(text, 4), Position::new(1, 1));
        assert_eq!(position_from_offset(text, 6), Position::new(2, 0));
    }

    #[test]
    fn position_to_offset_roundtrip() {
        let text = "let x = 5\nx.";
        for offset in 0..=text.len() {
            let pos = position_from_offset(text, offset);
            let back = offset_from_position(text, pos);
            assert_eq!(back, Some(offset), "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn position_to_offset_past_end_of_line() {
        let text = "ab\ncd";
        // Position past end of first line should clamp to the newline
        let offset = offset_from_position(text, Position::new(0, 5));
        assert_eq!(offset, Some(2));
    }

    #[test]
    fn position_to_offset_past_end_of_text() {
        let text = "ab";
        assert_eq!(offset_from_position(text, Position::new(0, 2)), Some(2));
        assert_eq!(offset_from_position(text, Position::new(1, 0)), None);
    }
}
