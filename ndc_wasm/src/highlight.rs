use ndc_highlight::Kind;
use wasm_bindgen::prelude::*;

/// `source` as HTML, each piece in a `<span>` whose class says what it is:
/// `let x` becomes `<span class="tok-keyword">let</span> <span class="tok-variable">x</span>`.
#[wasm_bindgen]
#[must_use]
pub fn highlight(source: &str) -> String {
    let mut html = String::with_capacity(source.len() * 3);
    let mut end_of_previous = 0;
    for piece in ndc_highlight::highlight(source) {
        escape_into(&source[end_of_previous..piece.range.start], &mut html);
        html.push_str("<span class=\"tok-");
        html.push_str(class(piece.kind));
        html.push_str("\">");
        escape_into(&source[piece.range.clone()], &mut html);
        html.push_str("</span>");
        end_of_previous = piece.range.end;
    }
    escape_into(&source[end_of_previous..], &mut html);
    html
}

/// Every kind, in the order `highlight_spans` numbers them.
const KINDS: [Kind; 10] = [
    Kind::String,
    Kind::Literal,
    Kind::Keyword,
    Kind::Function,
    Kind::Variable,
    Kind::Assignment,
    Kind::Punctuation,
    Kind::Operator,
    Kind::Comment,
    Kind::Invalid,
];

/// The pieces of `source` for an editor, three numbers each:
/// `[start, end, kind]`. Start and end count UTF-16 units, the way JavaScript
/// indexes strings, and kind indexes `highlight_classes()`: `let x` gives
/// `[0, 3, 2, 4, 5, 4]`, a keyword and a variable.
#[wasm_bindgen]
#[must_use]
pub fn highlight_spans(source: &str) -> Vec<u32> {
    let pieces = ndc_highlight::highlight(source);
    let mut spans = Vec::with_capacity(pieces.len() * 3);
    // The pieces come in order, so the UTF-16 offset is counted forward once.
    let (mut byte, mut utf16) = (0, 0);
    let mut to_utf16 = |to: usize| {
        utf16 += source[byte..to].encode_utf16().count();
        byte = to;
        u32::try_from(utf16).unwrap_or(u32::MAX)
    };
    for piece in pieces {
        let kind = KINDS.iter().position(|k| *k == piece.kind).unwrap_or(0);
        spans.push(to_utf16(piece.range.start));
        spans.push(to_utf16(piece.range.end));
        spans.push(u32::try_from(kind).unwrap_or(0));
    }
    spans
}

/// The class of each kind `highlight_spans` reports, without the `tok-`
/// prefix the HTML from `highlight` gives them.
#[wasm_bindgen]
#[must_use]
pub fn highlight_classes() -> Vec<String> {
    KINDS.iter().map(|kind| class(*kind).to_string()).collect()
}

fn class(kind: Kind) -> &'static str {
    match kind {
        Kind::String => "string",
        Kind::Literal => "literal",
        Kind::Keyword => "keyword",
        Kind::Function => "function",
        Kind::Variable => "variable",
        Kind::Assignment => "assignment",
        Kind::Punctuation => "punctuation",
        Kind::Operator => "operator",
        Kind::Comment => "comment",
        Kind::Invalid => "invalid",
    }
}

fn escape_into(text: &str, html: &mut String) {
    for c in text.chars() {
        match c {
            '<' => html.push_str("&lt;"),
            '>' => html.push_str("&gt;"),
            '&' => html.push_str("&amp;"),
            '"' => html.push_str("&quot;"),
            c => html.push(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pieces_are_wrapped_and_text_is_escaped() {
        assert_eq!(
            highlight("a < \"<b>\" // &"),
            "<span class=\"tok-variable\">a</span> \
             <span class=\"tok-operator\">&lt;</span> \
             <span class=\"tok-string\">&quot;&lt;b&gt;&quot;</span> \
             <span class=\"tok-comment\">// &amp;</span>"
        );
    }

    #[test]
    fn spans_count_utf16_units_and_index_the_classes() {
        let classes = highlight_classes();
        assert_eq!(highlight_spans("let x"), [0, 3, 2, 4, 5, 4]);
        assert_eq!(classes[2], "keyword");
        // "é" is 2 bytes but 1 UTF-16 unit; "😀" is 4 bytes but 2 units.
        let spans = highlight_spans("\"é😀\" x");
        assert_eq!(spans, [0, 5, 0, 6, 7, 4]);
    }
}
