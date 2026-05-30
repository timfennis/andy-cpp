use ndc_lexer::Span;
use tower_lsp::lsp_types::{Position, Range};

/// Precomputed line-start byte offsets for a document, enabling O(log n)
/// offset<->position conversion instead of rescanning the text from the start
/// on every call.
///
/// Character columns are counted in UTF-16 code units, which is the default LSP
/// position encoding (we advertise no alternative). A BMP scalar is one unit; an
/// astral scalar (e.g. an emoji) is two. Counting scalar values instead would
/// resolve positions after such characters to the wrong byte offset.
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
        let character: usize = text[line_start..offset].chars().map(char::len_utf16).sum();
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

        // `col` accumulates UTF-16 code units, matching `position`. A column that
        // lands inside an astral character's surrogate pair clamps to that
        // character's start boundary.
        let mut offset = line_start;
        let mut col = 0u32;
        for c in text[line_start..line_end].chars() {
            if c == '\n' || col >= pos.character {
                return Some(offset);
            }
            let width = c.len_utf16() as u32;
            if col + width > pos.character {
                // Target splits this character's surrogate pair → clamp to its start.
                return Some(offset);
            }
            offset += c.len_utf8();
            col += width;
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
    fn bmp_char_is_one_utf16_unit() {
        // "héllo" — é is two UTF-8 bytes but a single UTF-16 unit (BMP).
        let text = "héllo";
        let index = LineIndex::new(text);
        // byte offset 3 is just after "hé" (1 + 2 bytes) → column 2.
        assert_eq!(index.position(text, 3), Position::new(0, 2));
        assert_eq!(index.offset(text, Position::new(0, 2)), Some(3));
    }

    #[test]
    fn astral_char_is_two_utf16_units() {
        // 😀 is U+1F600: 4 UTF-8 bytes, 2 UTF-16 units (a surrogate pair).
        let text = "a😀b";
        let index = LineIndex::new(text);
        // `b` is at byte 5; in UTF-16 columns that is a(1) + 😀(2) = 3.
        assert_eq!(index.position(text, 5), Position::new(0, 3));
        assert_eq!(index.offset(text, Position::new(0, 3)), Some(5));
        // A column landing inside the surrogate pair clamps to the char boundary.
        assert_eq!(index.offset(text, Position::new(0, 2)), Some(1));
    }

    #[test]
    fn position_to_offset_roundtrip_with_astral() {
        // Round-trip every char-boundary offset through UTF-16 columns.
        let text = "x = 😀\ny";
        let index = LineIndex::new(text);
        for (offset, _) in text.char_indices().chain([(text.len(), ' ')]) {
            let pos = index.position(text, offset);
            assert_eq!(
                index.offset(text, pos),
                Some(offset),
                "roundtrip failed for offset {offset}"
            );
        }
    }
}
