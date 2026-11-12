//! What the formatter needs to know about the source beyond the AST: where
//! the parentheses, semicolons and comments are, and where blank lines are.

use ndc_lexer::{Span, Token, TokenLocation};
use std::ops::Range;

pub struct Comment {
    pub range: Range<usize>,
    /// The comment with its marker, without trailing whitespace: `// note`
    pub text: String,
}

pub struct Source<'a> {
    pub text: &'a str,
    /// The tokens other than comments
    tokens: Vec<TokenLocation>,
    /// For a bracket that opens, the index of the one that closes it
    closing: Vec<Option<usize>>,
    pub comments: Vec<Comment>,
    /// The ranges of all tokens, comments included, in order
    all: Vec<Range<usize>>,
}

impl<'a> Source<'a> {
    pub fn new(text: &'a str, tokens: Vec<TokenLocation>) -> Self {
        let all = tokens.iter().map(|token| token.span.range()).collect();
        let (comments, tokens): (Vec<_>, Vec<_>) = tokens
            .into_iter()
            .partition(|token| matches!(token.token, Token::Comment(_)));
        let comments = comments
            .into_iter()
            .map(|token| Comment {
                text: text[token.span.range()].trim_end().to_string(),
                range: token.span.range(),
            })
            .collect();

        let mut closing = vec![None; tokens.len()];
        let mut open = Vec::new();
        for (i, token) in tokens.iter().enumerate() {
            match token.token {
                Token::LeftParentheses
                | Token::LeftSquareBracket
                | Token::LeftCurlyBracket
                | Token::MapOpen => open.push(i),
                Token::RightParentheses | Token::RightSquareBracket | Token::RightCurlyBracket => {
                    if let Some(opening) = open.pop() {
                        closing[opening] = Some(i);
                    }
                }
                _ => {}
            }
        }

        Self {
            text,
            tokens,
            closing,
            comments,
            all,
        }
    }

    /// The source text of `span`
    pub fn slice(&self, span: Span) -> &'a str {
        &self.text[span.range()]
    }

    /// `span` without the parentheses around it, and whether it had any:
    /// `(a + b)` gives `a + b` and `true`. `(a) + (b)` has none around it.
    pub fn without_parentheses(&self, span: Span) -> (Range<usize>, bool) {
        let mut range = span.range();
        let mut wrapped = false;
        while let Some((first, last)) = self.bracketed(&range, &Token::LeftParentheses)
            && last > first + 1
        {
            range = self.tokens[first + 1].span.offset()..self.tokens[last - 1].span.end();
            wrapped = true;
        }
        (range, wrapped)
    }

    /// The indices of the first and last token of `range` if these are `open`
    /// and the bracket that closes it.
    pub fn bracketed(&self, range: &Range<usize>, open: &Token) -> Option<(usize, usize)> {
        let first = self
            .tokens
            .binary_search_by_key(&range.start, |token| token.span.offset())
            .ok()?;
        let last = self
            .tokens
            .binary_search_by_key(&range.end, |token| token.span.end())
            .ok()?;
        (&self.tokens[first].token == open && self.closing[first] == Some(last))
            .then_some((first, last))
    }

    /// The first token at or after `offset`, comments left out
    pub fn token_after(&self, offset: usize) -> Option<&TokenLocation> {
        let i = self
            .tokens
            .partition_point(|token| token.span.offset() < offset);
        self.tokens.get(i)
    }

    /// The last token before `offset`, comments left out
    pub fn token_before(&self, offset: usize) -> Option<&TokenLocation> {
        let i = self
            .tokens
            .partition_point(|token| token.span.offset() < offset);
        i.checked_sub(1).map(|i| &self.tokens[i])
    }

    /// The tokens from `range.start` up to `range.end`, comments left out
    pub fn tokens_in(&self, range: Range<usize>) -> &[TokenLocation] {
        let start = self
            .tokens
            .partition_point(|token| token.span.offset() < range.start);
        let end = self
            .tokens
            .partition_point(|token| token.span.offset() < range.end);
        &self.tokens[start..end.max(start)]
    }

    /// Whether there is an empty line between `offset` and the token or
    /// comment before it
    pub fn blank_line_before(&self, offset: usize) -> bool {
        let i = self.all.partition_point(|range| range.end <= offset);
        let Some(previous) = i.checked_sub(1).map(|i| &self.all[i]) else {
            return false;
        };
        self.text[previous.end..offset].matches('\n').count() >= 2
    }

    /// Whether nothing but whitespace comes before `offset` on its line
    pub fn starts_line(&self, offset: usize) -> bool {
        let before = self.text[..offset].trim_end_matches([' ', '\t']);
        before.is_empty() || before.ends_with('\n')
    }

    /// Whether a comment starts in `range`
    pub fn has_comment_in(&self, range: Range<usize>) -> bool {
        let i = self
            .comments
            .partition_point(|comment| comment.range.start < range.start);
        self.comments
            .get(i)
            .is_some_and(|comment| comment.range.start < range.end)
    }

    /// Whether `range` crosses a line break
    pub fn spans_lines(&self, range: Range<usize>) -> bool {
        self.text[range].contains('\n')
    }
}
