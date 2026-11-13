use lsp_types::{
    CompletionResponse, Diagnostic, DocumentSymbol, Hover, InlayHint, Position, Range,
};
use ndc_analysis::Analysis;
use ndc_core::FunctionRegistry;
use ndc_interpreter::{Interpreter, NativeFunction};
use ndc_parser::ExpressionLocation;
use std::rc::Rc;

use crate::diagnostics;
use crate::features::completion::FunctionInfo;
use crate::features::{completion, definition, hover, inlay_hints, symbols};
use crate::state::DocumentState;

/// The registered functions, the way an interpreter's registry is configured.
pub type Configure = fn(&mut FunctionRegistry<Rc<NativeFunction>>);

/// An interpreter that knows the registered functions and nothing else.
pub(crate) fn interpreter(configure: Configure) -> Interpreter {
    let mut interpreter = Interpreter::capturing();
    interpreter.configure(configure);
    interpreter
}

/// What one analysis of `text` produced: the diagnostics to show, and the AST
/// with its analysis when the text parsed.
pub(crate) fn analyse(
    configure: Configure,
    text: &str,
) -> (Vec<Diagnostic>, Option<(Vec<ExpressionLocation>, Analysis)>) {
    let analysed = interpreter(configure).analyse_str(text);
    let diagnostics = diagnostics::analysis_diagnostics(text, &analysed);
    let ast = analysed.ok().map(|(ast, result)| (ast, result.analysis));
    (diagnostics, ast)
}

/// One document and the language features over it, for a caller that holds
/// the text itself rather than talking LSP over stdio, such as the browser
/// playground. Positions are LSP positions: a line and a UTF-16 column.
///
/// ```
/// # use ndc_lsp::{Document, lsp_types::Position};
/// let mut document = Document::new(|_| {});
/// let diagnostics = document.set_text("let x = 1;\nx.");
/// // `x.` doesn't parse, but completion still knows `x` from the analysis
/// // of the last text that did.
/// # let _ = (diagnostics, document.completion(Position::new(1, 2)));
/// ```
pub struct Document {
    configure: Configure,
    functions: Vec<FunctionInfo>,
    state: DocumentState,
}

impl Document {
    #[must_use]
    pub fn new(configure: Configure) -> Self {
        Self {
            configure,
            functions: FunctionInfo::collect(&interpreter(configure)),
            state: DocumentState::from_source(String::new()),
        }
    }

    /// Replaces the text and analyses it, returning its diagnostics. When the
    /// text doesn't parse, completion keeps what the last analysis of a text
    /// that did worked out, and the AST-backed features return nothing.
    pub fn set_text(&mut self, text: &str) -> Vec<Diagnostic> {
        self.state.set_source(text);
        let (diagnostics, analysed) = analyse(self.configure, text);
        if let Some((ast, analysis)) = analysed {
            self.state.set_analysis(ast, analysis);
        }
        diagnostics
    }

    #[must_use]
    pub fn completion(&self, position: Position) -> CompletionResponse {
        completion::complete(Some(&self.state), position, &self.functions)
    }

    #[must_use]
    pub fn hover(&self, position: Position) -> Option<Hover> {
        self.analysed()
            .and_then(|state| hover::hover(state, position, &self.functions))
    }

    #[must_use]
    pub fn inlay_hints(&self) -> Vec<InlayHint> {
        self.analysed().map_or_else(Vec::new, |state| {
            inlay_hints::collect(
                &state.ast,
                &state.analysis,
                &state.source,
                &state.line_index,
            )
        })
    }

    #[must_use]
    pub fn symbols(&self) -> Vec<DocumentSymbol> {
        self.analysed().map_or_else(Vec::new, |state| {
            symbols::document_symbols(
                &state.ast,
                &state.analysis,
                &state.source,
                &state.line_index,
            )
        })
    }

    /// Where the name under `position` is declared.
    #[must_use]
    pub fn definition(&self, position: Position) -> Option<Range> {
        self.analysed()
            .and_then(|state| definition::goto_definition(state, position))
    }

    /// The state, if its analysis is of the current text.
    fn analysed(&self) -> Option<&DocumentState> {
        Some(&self.state).filter(|state| state.analysis_matches_source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{CompletionItem, HoverContents};

    fn labels(response: CompletionResponse) -> Vec<String> {
        let items: Vec<CompletionItem> = match response {
            CompletionResponse::Array(items) => items,
            CompletionResponse::List(list) => list.items,
        };
        items.into_iter().map(|item| item.label).collect()
    }

    #[test]
    fn setting_the_text_returns_its_diagnostics() {
        let mut document = Document::new(ndc_stdlib::register);
        assert!(document.set_text("let x = 1;").is_empty());
        let diagnostics = document.set_text("let x: Int = \"no\";");
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert!(diagnostics[0].message.contains("mismatched types"));
    }

    #[test]
    fn completion_after_a_dot_knows_the_last_text_that_parsed() {
        let mut document = Document::new(ndc_stdlib::register);
        document.set_text("let words = [\"a\"];\n");
        // `words.` doesn't parse, but `words` is still known to be a list.
        assert!(!document.set_text("let words = [\"a\"];\nwords.").is_empty());
        let labels = labels(document.completion(Position::new(1, 6)));
        assert!(labels.iter().any(|l| l == "len"), "{labels:?}");
        assert!(!labels.iter().any(|l| l == "abs"), "{labels:?}");
    }

    #[test]
    fn hover_and_definition_answer_for_the_current_text() {
        let mut document = Document::new(ndc_stdlib::register);
        document.set_text("let total = 5;\ntotal + 1;");
        let hover = document
            .hover(Position::new(1, 2))
            .expect("hover on `total`");
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected markup");
        };
        assert!(content.value.contains("Int"), "{}", content.value);

        let range = document
            .definition(Position::new(1, 2))
            .expect("a definition");
        assert_eq!(
            (range.start, range.end),
            (Position::new(0, 4), Position::new(0, 9))
        );
    }

    #[test]
    fn features_that_need_the_ast_answer_nothing_for_text_that_does_not_parse() {
        let mut document = Document::new(ndc_stdlib::register);
        document.set_text("let total = 5;\ntotal + 1;");
        document.set_text("let total = 5;\ntotal +");
        assert!(document.hover(Position::new(1, 2)).is_none());
        assert!(document.definition(Position::new(1, 2)).is_none());
        assert!(document.inlay_hints().is_empty());
        assert!(document.symbols().is_empty());
    }
}
