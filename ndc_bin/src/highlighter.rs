use itertools::Itertools;
use ndc_lexer::{Lexer, Token};
use yansi::{Paint, Painted};

pub(crate) struct AndycppHighlighter;

impl AndycppHighlighter {
    pub fn highlight_line(line: &str) -> Vec<Painted<&str>> {
        let Ok(tokens) = Lexer::new(line).collect::<Result<Vec<_>, _>>() else {
            return vec![line.red()];
        };

        let mut start = 0;
        let mut ranges = vec![];
        for token in tokens.iter().dropping(1) {
            let end = token.span.offset();
            ranges.push(start..end);
            start = end;
        }
        if !line.is_empty() {
            ranges.push(start..line.len());
        }

        let mut out = Vec::new();
        for (range, token) in ranges.into_iter().zip(tokens.into_iter()) {
            let substring = &line[range.start..(range.start + range.len())];

            let colored = match &token.token {
                Token::String(_) => substring.rgb(70, 200, 128),
                Token::BigInt(_)
                | Token::Int64(_)
                | Token::Float64(_)
                | Token::Complex(_)
                | Token::True
                | Token::False => substring.rgb(253, 151, 31),
                Token::LeftSquareBracket
                | Token::RightSquareBracket
                | Token::LeftCurlyBracket
                | Token::RightCurlyBracket
                | Token::LeftParentheses
                | Token::RightParentheses
                | Token::MapOpen => substring.rgb(229, 181, 103),
                Token::Identifier(_) => substring.rgb(51, 177, 255),
                _ => substring.rgb(140, 182, 255).bold(),
            };

            out.push(colored);
        }

        out
    }
}
