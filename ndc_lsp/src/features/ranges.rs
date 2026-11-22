//! Folding ranges and selection ranges, both read off the AST: what an editor
//! can fold, and what expanding the selection grows to, from the name under
//! the cursor through each expression around it.

use ls_types::{FoldingRange, FoldingRangeKind, Position, Range, SelectionRange};
use ndc_lexer::{Lexer, Span, Token};
use ndc_parser::{Expression, ExpressionLocation};

use crate::state::DocumentState;

/// What can be folded: every expression that spans lines and is a block, a
/// function, a struct, a list, map or tuple, or a call's arguments, and every
/// run of comment lines. A fold leaves the closing bracket's line showing
/// when the bracket starts it.
pub fn folding_ranges(state: &DocumentState) -> Vec<FoldingRange> {
    let mut folds = Vec::new();
    let mut stack: Vec<&ExpressionLocation> = state.ast.iter().collect();
    while let Some(expr) = stack.pop() {
        if folds_as_a_whole(&expr.expression)
            && let Some(fold) = fold(state, expr.span, None)
        {
            folds.push(fold);
        }
        stack.extend(expr.children());
    }
    folds.extend(comment_runs(state));
    folds.sort_by_key(|fold| (fold.start_line, fold.end_line));
    // Several nodes can span the same lines, as a function and its body.
    folds.dedup_by_key(|fold| (fold.start_line, fold.end_line));
    folds
}

/// Whether an expression of this kind is one an editor folds.
fn folds_as_a_whole(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Block { .. }
            | Expression::FunctionDeclaration { .. }
            | Expression::StructDeclaration { .. }
            | Expression::List { .. }
            | Expression::Map { .. }
            | Expression::Tuple { .. }
            | Expression::Call { .. }
            | Expression::MethodCall { .. }
            | Expression::For { .. }
            | Expression::While { .. }
            | Expression::If { .. }
    )
}

/// A fold of `span`, if it spans lines.
fn fold(state: &DocumentState, span: Span, kind: Option<FoldingRangeKind>) -> Option<FoldingRange> {
    let start = state.line_index.position(&state.source, span.offset());
    let end = state.line_index.position(&state.source, span.end());
    let mut end_line = end.line;
    // `}` starting its line stays showing, so the fold reads `{ … }`.
    let closing_starts_line = state.source[..span.end()]
        .trim_end()
        .rsplit('\n')
        .next()
        .is_some_and(|last| matches!(last.trim(), "}" | "]" | ")" | "};" | "]," | "},"));
    if closing_starts_line {
        end_line = end_line.saturating_sub(1);
    }
    (end_line > start.line).then_some(FoldingRange {
        start_line: start.line,
        start_character: None,
        end_line,
        end_character: None,
        kind,
        collapsed_text: None,
    })
}

/// The runs of two or more comment lines in a row.
fn comment_runs(state: &DocumentState) -> Vec<FoldingRange> {
    let Some(first) = state.ast.first() else {
        return Vec::new();
    };
    let comments: Vec<u32> = Lexer::new(&state.source, first.span.source_id())
        .map_while(Result::ok)
        .filter(|token| matches!(token.token, Token::Comment(_)))
        .map(|token| {
            state
                .line_index
                .position(&state.source, token.span.offset())
                .line
        })
        .collect();
    let mut runs = Vec::new();
    let mut start = 0;
    for i in 1..=comments.len() {
        if i == comments.len() || comments[i] != comments[i - 1] + 1 {
            if comments[i - 1] > comments[start] {
                runs.push(FoldingRange {
                    start_line: comments[start],
                    start_character: None,
                    end_line: comments[i - 1],
                    end_character: None,
                    kind: Some(FoldingRangeKind::Comment),
                    collapsed_text: None,
                });
            }
            start = i;
        }
    }
    runs
}

/// For each of `positions`, what expanding the selection from it grows to:
/// the name there, then each expression around it, innermost first.
pub fn selection_ranges(state: &DocumentState, positions: &[Position]) -> Vec<SelectionRange> {
    positions
        .iter()
        .map(|&position| selection_range(state, position))
        .collect()
}

fn selection_range(state: &DocumentState, position: Position) -> SelectionRange {
    let empty = SelectionRange {
        range: Range::new(position, position),
        parent: None,
    };
    let Some(offset) = state.line_index.offset(&state.source, position) else {
        return empty;
    };
    // Everything that holds the cursor, outermost first.
    let mut spans: Vec<Span> = Vec::new();
    let mut level: Vec<&ExpressionLocation> = state.ast.iter().collect();
    // Between two expressions, as in `a|+b`, the one the cursor is in comes
    // before the one it's just after.
    let inside =
        |expr: &&&ExpressionLocation| expr.span.offset() <= offset && offset < expr.span.end();
    let after = |expr: &&&ExpressionLocation| expr.span.end() == offset;
    while let Some(expr) = level
        .iter()
        .find(inside)
        .or_else(|| level.iter().find(after))
    {
        spans.push(expr.span);
        level = expr.children();
    }
    // The name, if it's within the innermost expression rather than just
    // next to it.
    if let Some(name) = state.names.at(offset)
        && spans.last().is_none_or(|innermost| {
            innermost.offset() <= name.span.offset() && name.span.end() <= innermost.end()
        })
    {
        spans.push(name.span);
    }
    // Innermost first, each strictly larger than the one before.
    spans.sort_by_key(|span| std::cmp::Reverse(span.end() - span.offset()));
    spans.dedup_by_key(|span| (span.offset(), span.end()));
    spans.into_iter().fold(empty, |parent, span| {
        let range = state.line_index.range(&state.source, span);
        if parent.range.start == parent.range.end && parent.parent.is_none() {
            SelectionRange {
                range,
                parent: None,
            }
        } else {
            SelectionRange {
                range,
                parent: Some(Box::new(parent)),
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn folds(source: &str) -> Vec<(u32, u32, Option<FoldingRangeKind>)> {
        folding_ranges(&DocumentState::analysed(source))
            .into_iter()
            .map(|fold| (fold.start_line, fold.end_line, fold.kind))
            .collect()
    }

    #[test]
    fn blocks_fold_and_leave_their_closing_bracket_showing() {
        let source = "fn f(x) {\n    let y = x;\n    y\n}\nprint(f(1));\n";
        assert_eq!(folds(source), [(0, 2, None)]);
    }

    #[test]
    fn nested_blocks_and_literals_fold() {
        let source = "fn f(x) {\n    if x > 0 {\n        x\n    } else {\n        0\n    }\n}\nlet xs = [\n    1,\n    2,\n];\nprint(f(1), xs);\n";
        let found = folds(source);
        assert!(found.contains(&(0, 5, None)), "{found:?}");
        assert!(found.contains(&(1, 2, None)), "{found:?}");
        assert!(found.contains(&(7, 9, None)), "{found:?}");
    }

    #[test]
    fn runs_of_comment_lines_fold() {
        let source = "// one\n// two\n// three\nlet x = 1;\n// alone\n";
        assert_eq!(folds(source), [(0, 2, Some(FoldingRangeKind::Comment))]);
    }

    #[test]
    fn a_one_line_expression_does_not_fold() {
        assert!(folds("fn f(x) { x }\nprint([1, 2]);\n").is_empty());
    }

    /// The text of each range expanding the selection at `$0` goes through.
    fn expansions(text: &str) -> Vec<String> {
        let offset = text.find("$0").expect("a cursor");
        let source = text.replacen("$0", "", 1);
        let state = DocumentState::analysed(&source);
        let position = state.line_index.position(&source, offset);
        let mut range = Some(selection_ranges(&state, &[position]).remove(0));
        let mut texts = Vec::new();
        while let Some(current) = range {
            let start = state
                .line_index
                .offset(&source, current.range.start)
                .unwrap();
            let end = state.line_index.offset(&source, current.range.end).unwrap();
            texts.push(source[start..end].to_string());
            range = current.parent.map(|parent| *parent);
        }
        texts
    }

    #[test]
    fn the_selection_grows_from_the_name_through_each_expression() {
        let found = expansions("let total = 1 + 2 * coun$0t;\nlet count = 3;");
        assert_eq!(
            found,
            [
                "count",
                "2 * count",
                "1 + 2 * count",
                "let total = 1 + 2 * count"
            ]
        );
    }

    #[test]
    fn each_range_holds_the_one_before() {
        let found = expansions("fn f(x) {\n    if x > 0 { print(x$0) } else { 0 }\n}");
        for pair in found.windows(2) {
            assert!(pair[1].contains(&pair[0]), "{found:?}");
            assert_ne!(pair[0], pair[1], "{found:?}");
        }
        assert_eq!(found[0], "x");
    }

    #[test]
    fn between_two_names_the_selection_still_nests() {
        let found = expansions("let total = 1;\nlet count = 2;\nlet s = total$0+count;");
        for pair in found.windows(2) {
            assert!(pair[1].contains(&pair[0]), "{found:?}");
        }
        assert!(found.contains(&"total+count".to_string()), "{found:?}");
    }
}
