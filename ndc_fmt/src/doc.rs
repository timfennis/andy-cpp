//! A layout that can be printed on one line or broken over several.
//!
//! `group([text("f("), indent([softline(), text("a,"), line(), text("b")]), softline(), text(")")])`
//! prints as `f(a, b)` when that fits on the line, and otherwise as
//!
//! ```text
//! f(
//!     a,
//!     b
//! )
//! ```
//!
//! This is the document model of Wadler's "A prettier printer", as used by
//! Prettier and mago.

use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Doc {
    Text(String),
    Concat(Vec<Self>),
    /// Lines inside it start one indentation level deeper.
    Indent(Box<Self>),
    /// Printed on one line if it fits, otherwise with its lines broken.
    /// `breaks` is set when it holds a hard line or [`Doc::BreakParent`],
    /// which can't be printed on one line.
    Group {
        doc: Box<Self>,
        breaks: bool,
    },
    Line(Line),
    /// `broken` where the enclosing group breaks, `flat` where it doesn't.
    IfBreak {
        broken: Box<Self>,
        flat: Box<Self>,
    },
    /// The first of these layouts that fits, or else the last one, broken.
    /// A call tries its arguments on one line, then with only its last
    /// argument broken, then each on its own line. What must break inside
    /// it doesn't break the groups around it, which lets a call end on the
    /// line its last argument ends on: `f(x, fn() {` ... `}) + 1`.
    Conditional(Vec<Self>),
    /// A part of more than one layout of a [`Doc::Conditional`]
    Shared(Rc<Self>),
    /// Text printed at the end of the current line, just before it breaks:
    /// a comment after code.
    LineSuffix(String),
    /// Breaks every enclosing group.
    BreakParent,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Line {
    /// A space, or a line break.
    Space,
    /// Nothing, or a line break.
    Soft,
    /// Always a line break.
    Hard,
}

pub fn text(text: impl Into<String>) -> Doc {
    Doc::Text(text.into())
}

pub fn concat(docs: impl IntoIterator<Item = Doc>) -> Doc {
    Doc::Concat(docs.into_iter().collect())
}

pub fn indent(doc: Doc) -> Doc {
    Doc::Indent(Box::new(doc))
}

pub fn group(doc: Doc) -> Doc {
    let breaks = must_break(&doc);
    Doc::Group {
        doc: Box::new(doc),
        breaks,
    }
}

pub fn line() -> Doc {
    Doc::Line(Line::Space)
}

pub fn softline() -> Doc {
    Doc::Line(Line::Soft)
}

pub fn hardline() -> Doc {
    Doc::Line(Line::Hard)
}

pub fn if_break(broken: Doc, flat: Doc) -> Doc {
    Doc::IfBreak {
        broken: Box::new(broken),
        flat: Box::new(flat),
    }
}

pub fn nil() -> Doc {
    Doc::Concat(Vec::new())
}

/// `doc` with its outermost group broken: `[1, 2]` as `[`, `1,`, `2,`, `]`
/// on lines of their own, whether or not it would fit on one
pub fn broken(doc: &Doc) -> Doc {
    match doc {
        Doc::Group { doc, .. } => Doc::Group {
            doc: doc.clone(),
            breaks: true,
        },
        Doc::Concat(docs) => {
            let mut docs = docs.clone();
            // The group is the last part; comments may come before it
            if let Some(last) = docs.iter_mut().rev().find(|doc| !is_nil(doc)) {
                *last = broken(last);
            }
            Doc::Concat(docs)
        }
        Doc::Shared(doc) => broken(doc),
        doc => doc.clone(),
    }
}

fn is_nil(doc: &Doc) -> bool {
    matches!(doc, Doc::Concat(docs) if docs.iter().all(is_nil))
}

/// Whether `doc` can't be printed on one line. A nested group has already
/// worked this out for itself, so each part of a document is looked at once.
pub fn must_break(doc: &Doc) -> bool {
    match doc {
        Doc::Text(_) | Doc::LineSuffix(_) | Doc::Conditional(_) => false,
        Doc::Line(line) => *line == Line::Hard,
        Doc::BreakParent => true,
        Doc::Concat(docs) => docs.iter().any(must_break),
        Doc::Indent(doc) => must_break(doc),
        Doc::Shared(doc) => must_break(doc),
        Doc::Group { breaks, .. } => *breaks,
        Doc::IfBreak { broken, flat } => must_break(broken) || must_break(flat),
    }
}
