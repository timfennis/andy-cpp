//! Prints a [`Doc`], breaking a group's lines only when the group doesn't fit
//! in the rest of the line.

use crate::doc::{Doc, Line};

const INDENT: usize = 4;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    Flat,
    Break,
}

/// What is left to print: a document, how deeply it's indented and whether
/// its group broke. The last one is printed first.
type Stack<'a> = Vec<(usize, Mode, &'a Doc)>;

/// `doc` as text whose lines are at most `width` columns wide where possible,
/// with no trailing whitespace and ending in a newline.
pub fn print(doc: &Doc, width: usize) -> String {
    let mut out = String::new();
    let mut column = 0;
    // Comments waiting for the end of the line
    let mut line_suffix: Vec<&str> = Vec::new();
    let mut stack: Stack<'_> = vec![(0, Mode::Break, doc)];

    while let Some((indent, mode, doc)) = stack.pop() {
        match doc {
            Doc::Text(text) => {
                out.push_str(text);
                // A string literal may hold line breaks
                column = match text.rsplit_once('\n') {
                    Some((_, last_line)) => last_line.chars().count(),
                    None => column + text.chars().count(),
                };
            }
            Doc::Concat(docs) => {
                stack.extend(docs.iter().rev().map(|doc| (indent, mode, doc)));
            }
            // A group on one line breaks no lines of its own, so a group
            // broken inside it is indented from the line they're on
            Doc::Indent(doc) => {
                let indent = if mode == Mode::Break {
                    indent + INDENT
                } else {
                    indent
                };
                stack.push((indent, mode, doc));
            }
            Doc::Group { doc, breaks } => {
                let flat = !breaks
                    && (mode == Mode::Flat || fits(doc, width.saturating_sub(column), &stack));
                let mode = if flat { Mode::Flat } else { Mode::Break };
                stack.push((indent, mode, doc));
            }
            Doc::Conditional(states) => {
                let (last, tried) = states.split_last().expect("a conditional has a layout");
                let fitting = if mode == Mode::Flat {
                    tried.first()
                } else {
                    let width = width.saturating_sub(column);
                    tried.iter().find(|state| fits(state, width, &stack))
                };
                stack.push(match fitting {
                    Some(state) => (indent, Mode::Flat, state),
                    None => (indent, Mode::Break, last),
                });
            }
            Doc::Shared(doc) => stack.push((indent, mode, doc)),
            Doc::IfBreak { broken, flat } => {
                let doc = if mode == Mode::Break { broken } else { flat };
                stack.push((indent, mode, doc));
            }
            Doc::LineSuffix(text) => line_suffix.push(text),
            Doc::BreakParent => {}
            Doc::Line(line) => match (mode, line) {
                (Mode::Flat, Line::Soft) => {}
                (Mode::Flat, Line::Space) => {
                    out.push(' ');
                    column += 1;
                }
                (Mode::Break, _) | (_, Line::Hard) => {
                    for text in line_suffix.drain(..) {
                        out.push_str(text);
                    }
                    trim_end(&mut out);
                    out.push('\n');
                    out.extend(std::iter::repeat_n(' ', indent));
                    column = indent;
                }
            },
        }
    }

    for text in line_suffix {
        out.push_str(text);
    }
    trim_end(&mut out);
    out.push('\n');
    out
}

/// Whether `doc` printed flat, followed by what comes after it up to the next
/// line break, takes at most `width` columns.
fn fits(doc: &Doc, width: usize, rest: &Stack<'_>) -> bool {
    let mut width = width as isize;
    let mut stack: Vec<(Mode, &Doc)> = vec![(Mode::Flat, doc)];
    let mut rest = rest.iter().rev();

    loop {
        let Some((mode, doc)) = stack.pop().or_else(|| rest.next().map(|&(_, m, d)| (m, d))) else {
            return true;
        };
        match doc {
            Doc::Text(text) => {
                let (first_line, more_lines) = match text.split_once('\n') {
                    Some((first_line, _)) => (first_line, true),
                    None => (text.as_str(), false),
                };
                width -= first_line.chars().count() as isize;
                if width < 0 {
                    return false;
                }
                if more_lines {
                    return true;
                }
            }
            Doc::Concat(docs) => stack.extend(docs.iter().rev().map(|doc| (mode, doc))),
            Doc::Indent(doc) => stack.push((mode, doc)),
            Doc::Group { doc, breaks } => {
                let mode = if *breaks { Mode::Break } else { mode };
                stack.push((mode, doc));
            }
            Doc::Conditional(states) => {
                let state = if mode == Mode::Break {
                    states.last()
                } else {
                    states.first()
                };
                stack.extend(state.map(|state| (mode, state)));
            }
            Doc::Shared(doc) => stack.push((mode, doc)),
            Doc::IfBreak { broken, flat } => {
                stack.push((mode, if mode == Mode::Break { broken } else { flat }));
            }
            Doc::Line(line) => {
                if mode == Mode::Break || *line == Line::Hard {
                    return true;
                }
                if *line == Line::Space {
                    width -= 1;
                }
            }
            Doc::LineSuffix(_) | Doc::BreakParent => {}
        }
    }
}

fn trim_end(out: &mut String) {
    let trimmed = out.trim_end_matches([' ', '\t']).len();
    out.truncate(trimmed);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc::*;

    fn call(args: &[&str]) -> Doc {
        let mut items = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                items.push(text(","));
                items.push(line());
            }
            items.push(text(*arg));
        }
        group(concat([
            text("f("),
            indent(concat([
                softline(),
                concat(items),
                if_break(text(","), nil()),
            ])),
            softline(),
            text(")"),
        ]))
    }

    #[test]
    fn a_group_that_fits_stays_on_one_line() {
        assert_eq!(print(&call(&["a", "b"]), 80), "f(a, b)\n");
    }

    #[test]
    fn a_group_that_does_not_fit_breaks_every_line() {
        assert_eq!(
            print(&call(&["aaaa", "bbbb"]), 10),
            "f(\n    aaaa,\n    bbbb,\n)\n"
        );
    }

    #[test]
    fn a_hard_line_breaks_its_group() {
        let doc = group(concat([
            text("{"),
            indent(concat([line(), text("a"), hardline(), text("b")])),
            line(),
            text("}"),
        ]));
        assert_eq!(print(&doc, 80), "{\n    a\n    b\n}\n");
    }

    #[test]
    fn only_the_last_line_of_a_text_counts_towards_the_next() {
        // Only `"ab` has to fit before the line break in the string
        let long = format!("\"ab\n{}\"", "x".repeat(100));
        let doc = concat([call(&[&long, "a"]), text(";")]);
        assert_eq!(print(&doc, 20), format!("f({long}, a);\n"));
    }

    #[test]
    fn a_line_suffix_waits_for_the_end_of_the_line() {
        let doc = concat([
            text("a"),
            Doc::LineSuffix(" // c".into()),
            text(";"),
            hardline(),
            text("b"),
        ]);
        assert_eq!(print(&doc, 80), "a; // c\nb\n");
    }
}
