use ndc_lexer::Span;
use tower_lsp::lsp_types::{Position, Range};

/// Precomputed line-start byte offsets for a document, enabling O(log n)
/// offset<->position conversion instead of rescanning the text from the start
/// on every call.
///
/// The character column is counted in Unicode scalar values (matching the rest
/// of the server), not UTF-16 code units.
#[derive(Debug, Clone, Default)]
pub struct LineIndex {
    /// Byte offset of the first character of each line. Always starts with `0`.
    line_starts: Vec<usize>,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Convert a byte `offset` into an LSP [`Position`]. Offsets past the end of
    /// the text clamp to the end.
    pub fn position(&self, text: &str, offset: usize) -> Position {
        let offset = offset.min(text.len());
        // Largest line whose start offset is <= `offset`.
        let line = self.line_starts.partition_point(|&start| start <= offset) - 1;
        let line_start = self.line_starts[line];
        let character = text[line_start..offset].chars().count();
        Position {
            line: line as u32,
            character: character as u32,
        }
    }

    /// Convert an LSP [`Position`] into a byte offset, or `None` if the line lies
    /// beyond the end of the text. A character column past the end of its line
    /// clamps to the line's terminating newline (or the end of the text).
    pub fn offset(&self, text: &str, pos: Position) -> Option<usize> {
        let line = pos.line as usize;
        let line_start = *self.line_starts.get(line)?;
        let line_end = self
            .line_starts
            .get(line + 1)
            .map_or(text.len(), |&next| next);

        let mut offset = line_start;
        for (col, c) in text[line_start..line_end].chars().enumerate() {
            if col as u32 == pos.character || c == '\n' {
                return Some(offset);
            }
            offset += c.len_utf8();
        }
        Some(offset)
    }

    pub fn range(&self, text: &str, span: Span) -> Range {
        Range {
            start: self.position(text, span.offset()),
            end: self.position(text, span.end()),
        }
    }
}

/// Convert a [`Span`] to an LSP [`Range`]. Builds a transient [`LineIndex`];
/// prefer a cached `LineIndex` when converting repeatedly.
pub fn span_to_range(text: &str, span: Span) -> Range {
    LineIndex::new(text).range(text, span)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset_to_position_single_line() {
        let text = "hello";
        let index = LineIndex::new(text);
        assert_eq!(index.position(text, 0), Position::new(0, 0));
        assert_eq!(index.position(text, 3), Position::new(0, 3));
        assert_eq!(index.position(text, 5), Position::new(0, 5));
    }

    #[test]
    fn offset_to_position_multiline() {
        let text = "ab\ncd\nef";
        let index = LineIndex::new(text);
        assert_eq!(index.position(text, 0), Position::new(0, 0));
        assert_eq!(index.position(text, 2), Position::new(0, 2));
        // offset 3 is start of second line
        assert_eq!(index.position(text, 3), Position::new(1, 0));
        assert_eq!(index.position(text, 4), Position::new(1, 1));
        assert_eq!(index.position(text, 6), Position::new(2, 0));
    }

    #[test]
    fn position_to_offset_roundtrip() {
        let text = "let x = 5\nx.";
        let index = LineIndex::new(text);
        for offset in 0..=text.len() {
            let pos = index.position(text, offset);
            let back = index.offset(text, pos);
            assert_eq!(back, Some(offset), "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn position_to_offset_past_end_of_line() {
        let text = "ab\ncd";
        let index = LineIndex::new(text);
        // Position past end of first line should clamp to the newline
        assert_eq!(index.offset(text, Position::new(0, 5)), Some(2));
    }

    #[test]
    fn position_to_offset_past_end_of_text() {
        let text = "ab";
        let index = LineIndex::new(text);
        assert_eq!(index.offset(text, Position::new(0, 2)), Some(2));
        assert_eq!(index.offset(text, Position::new(1, 0)), None);
    }

    #[test]
    fn multibyte_columns_count_scalar_values() {
        // "héllo" — é is two UTF-8 bytes. Column counts scalar values, not bytes.
        let text = "héllo";
        let index = LineIndex::new(text);
        // offset 3 is just after "hé" (1 + 2 bytes)
        assert_eq!(index.position(text, 3), Position::new(0, 2));
        assert_eq!(index.offset(text, Position::new(0, 2)), Some(3));
    }
}
