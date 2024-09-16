use itertools::Itertools;
use miette::{
    highlighters::{Highlighter, HighlighterState},
    SpanContents,
};
use ndc_lib::lexer::{Lexer, Token};
use owo_colors::{Rgb, Style, Styled};

#[derive(Default)]
pub struct AndycppHighlighter {}

impl Highlighter for AndycppHighlighter {
    fn start_highlighter_state<'h>(
        &'h self,
        _source: &dyn SpanContents<'_>,
    ) -> Box<dyn HighlighterState + 'h> {
        Box::new(CustomHighlighterState {})
    }
}

pub(crate) struct CustomHighlighterState {}

const NUMBER_LITERAL_COLOR: owo_colors::Rgb = Rgb(253, 151, 31);
const PARENTHESES_COLOR: owo_colors::Rgb = Rgb(229, 181, 103);

const STRING_LITERAL_COLOR: owo_colors::DynColors =
    owo_colors::DynColors::Ansi(owo_colors::AnsiColors::BrightGreen);

impl HighlighterState for CustomHighlighterState {
    fn highlight_line<'s>(&mut self, line: &'s str) -> Vec<Styled<&'s str>> {
        let Ok(tokens) = Lexer::new(line).collect::<Result<Vec<_>, _>>() else {
            return vec![Style::new().red().style(line)];
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
                Token::String(_) => Style::new().color(STRING_LITERAL_COLOR).style(substring),
                Token::BigInt(_) | Token::Int64(_) | Token::Float64(_) | Token::Complex(_) => {
                    Style::new().color(NUMBER_LITERAL_COLOR).style(substring)
                }
                Token::True | Token::False => {
                    Style::new().color(NUMBER_LITERAL_COLOR).style(substring)
                }
                Token::LeftSquareBracket
                | Token::RightSquareBracket
                | Token::LeftCurlyBracket
                | Token::RightCurlyBracket
                | Token::LeftParentheses
                | Token::RightParentheses
                | Token::MapOpen => Style::new().color(PARENTHESES_COLOR).style(substring),
                Token::Identifier(_) => Style::new().bright_cyan().style(substring),
                _ => Style::new().bright_blue().bold().style(substring),
            };

            out.push(colored);
        }

        out
    }
}
