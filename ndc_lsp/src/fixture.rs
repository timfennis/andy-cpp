//! Test fixtures written the way an editor sees a document: the texts it held
//! in turn, with `$0` marking the cursor in the last one.

use ls_types::{CompletionItem, CompletionResponse, Position};

use crate::Document;
use crate::util::LineIndex;

/// Marks the cursor in a fixture's text.
const CURSOR: &str = "$0";

/// A document that held each of `texts` in turn, as typing them would leave
/// it, and the position of `$0` in the last. A text that doesn't parse keeps
/// the analysis of the last one that did, so
/// `document(&["let xs = [1];\n", "let xs = [1];\nxs.$0"])` is the moment
/// after typing the dot.
pub fn document(texts: &[&str]) -> (Document, Position) {
    let (last, earlier) = texts.split_last().expect("at least one text");
    let mut document = Document::new(ndc_stdlib::register);
    for text in earlier {
        document.set_text(text);
    }
    let offset = last
        .find(CURSOR)
        .expect("the last text marks the cursor with `$0`");
    let text = last.replacen(CURSOR, "", 1);
    document.set_text(&text);
    let position = LineIndex::new(&text).position(&text, offset);
    (document, position)
}

/// What completion offers at the cursor of [`document`]`(texts)`.
pub fn completions(texts: &[&str]) -> Vec<CompletionItem> {
    let (document, position) = document(texts);
    match document.completion(position) {
        CompletionResponse::Array(items) => items,
        CompletionResponse::List(list) => list.items,
    }
}

/// The completion item labelled `label`, if there's one.
pub fn item<'a>(items: &'a [CompletionItem], label: &str) -> Option<&'a CompletionItem> {
    items.iter().find(|item| item.label == label)
}
