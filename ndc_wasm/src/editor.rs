use ndc_lsp::Document;
use ndc_lsp::lsp_types::Position;
use serde::Serialize;
use wasm_bindgen::prelude::*;

/// The language features over the editor's text: diagnostics, completion,
/// hover and go-to-definition, the way the language server offers them.
/// Positions are a line and a UTF-16 column, both from 0, and results are the
/// language server protocol's types as JSON.
///
/// ```js
/// const editor = new Editor();
/// JSON.parse(editor.set_text("let x: Int = \"no\";"));  // [{ range, message, … }]
/// JSON.parse(editor.hover(0, 4));                         // { contents, … } or null
/// ```
#[wasm_bindgen]
pub struct Editor {
    document: Document,
}

#[wasm_bindgen]
impl Editor {
    #[wasm_bindgen(constructor)]
    #[must_use]
    pub fn new() -> Self {
        Self {
            document: Document::new(ndc_stdlib::register),
        }
    }

    /// Replaces the text and returns its diagnostics.
    pub fn set_text(&mut self, text: &str) -> String {
        json(&self.document.set_text(text))
    }

    #[must_use]
    pub fn completion(&self, line: u32, character: u32) -> String {
        json(&self.document.completion(Position::new(line, character)))
    }

    #[must_use]
    pub fn hover(&self, line: u32, character: u32) -> String {
        json(&self.document.hover(Position::new(line, character)))
    }

    #[must_use]
    pub fn definition(&self, line: u32, character: u32) -> String {
        json(&self.document.definition(Position::new(line, character)))
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

fn json(value: &impl Serialize) -> String {
    serde_json::to_string(value).unwrap_or_else(|_| "null".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn results_are_lsp_json() {
        let mut editor = Editor::new();
        let diagnostics = editor.set_text("let total: Int = 5;\ntotal");
        assert_eq!(diagnostics, "[]");
        assert!(editor.hover(1, 2).contains("Int"), "{}", editor.hover(1, 2));
        assert_eq!(
            editor.definition(1, 2),
            r#"{"start":{"line":0,"character":4},"end":{"line":0,"character":9}}"#
        );
        assert_eq!(editor.hover(9, 9), "null");
    }
}
