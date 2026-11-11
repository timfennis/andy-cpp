use ndc_highlight::{Kind, highlight};
use yansi::{Paint, Painted};

pub(crate) struct AndycppHighlighter;

impl AndycppHighlighter {
    /// The source in pieces, coloured by what each piece is; the whitespace
    /// between them is left as it is.
    pub fn highlight_parsed(line: &str) -> Vec<Painted<&str>> {
        let mut out = Vec::new();
        let mut end_of_previous = 0;
        for piece in highlight(line) {
            if end_of_previous < piece.range.start {
                out.push(Painted::new(&line[end_of_previous..piece.range.start]));
            }
            end_of_previous = piece.range.end;
            let text = &line[piece.range];
            out.push(match piece.kind {
                Kind::String => text.rgb(152, 195, 121),
                Kind::Literal => text.rgb(209, 154, 102),
                Kind::Keyword => text.rgb(224, 108, 117),
                Kind::Function => text.rgb(229, 192, 123),
                Kind::Variable => text.rgb(97, 175, 239),
                Kind::Assignment => text.rgb(86, 182, 194),
                Kind::Punctuation => text.rgb(171, 178, 191),
                Kind::Operator => text.rgb(198, 120, 221),
                Kind::Comment => text.rgb(127, 132, 142),
                Kind::Invalid => text.red(),
            });
        }
        if end_of_previous < line.len() {
            out.push(Painted::new(&line[end_of_previous..]));
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yansi::Color;

    #[test]
    fn the_pieces_add_up_to_the_line() {
        let line = "  let x = 1; // one\n\tx.len  ";
        let highlighted = AndycppHighlighter::highlight_parsed(line);
        let joined: String = highlighted.iter().map(|p| p.value).collect();
        assert_eq!(joined, line);
    }

    #[test]
    fn number_literals_are_highlighted_as_numbers() {
        let highlighted = AndycppHighlighter::highlight_parsed("0xffn");
        assert_eq!(highlighted.len(), 1);
        assert_eq!(
            highlighted[0].style.foreground,
            Some(Color::Rgb(209, 154, 102))
        );
    }
}
