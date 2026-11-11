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
}
