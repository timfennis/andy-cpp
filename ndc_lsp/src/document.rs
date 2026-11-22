use ls_types::{
    CompletionResponse, Diagnostic, DocumentHighlight, DocumentSymbol, Hover, InlayHint, Position,
    Range, TextEdit,
};
use ndc_analysis::Analysis;
use ndc_core::FunctionRegistry;
use ndc_interpreter::{Interpreter, NativeSpec};
#[cfg(feature = "server")]
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::ExpressionLocation;
#[cfg(feature = "server")]
use ndc_parser::Parser;

use crate::diagnostics;
use crate::features::completion::FunctionInfo;
use crate::features::{
    completion, definition, formatting, hover, inlay_hints, ranges, references, semantic_tokens,
    signature_help, symbols,
};
use crate::resolved::Resolved;
use crate::state::DocumentState;

/// The registered functions, the way an interpreter's registry is configured.
pub type Configure = fn(&mut FunctionRegistry<NativeSpec>);

/// An interpreter that knows the registered functions and nothing else.
pub(crate) fn interpreter(configure: Configure) -> Interpreter {
    let mut interpreter = Interpreter::capturing();
    interpreter.configure(configure);
    interpreter
}

/// What analysing a text that parsed worked out about it.
pub(crate) struct Analysed {
    pub ast: Vec<ExpressionLocation>,
    pub analysis: Analysis,
    /// What its names refer to.
    pub resolved: Resolved,
}

/// Whether `text` lexes and parses, which is when [`analyse`] works something
/// out about it, without analysing it.
#[cfg(feature = "server")]
pub(crate) fn parses(text: &str) -> bool {
    Lexer::new(text, SourceId::SYNTHETIC)
        .collect::<Result<Vec<_>, _>>()
        .is_ok_and(|tokens| Parser::from_tokens(tokens).parse().is_ok())
}

/// What one analysis of `text` produced: the diagnostics to show, and what
/// the analysis worked out when the text parsed.
pub(crate) fn analyse(configure: Configure, text: &str) -> (Vec<Diagnostic>, Option<Analysed>) {
    let mut interpreter = interpreter(configure);
    let analysed = interpreter.analyse_str(text);
    let diagnostics = diagnostics::analysis_diagnostics(text, &analysed);
    let analysed = analysed.ok().map(|(ast, result)| {
        let resolver = interpreter.into_resolver();
        Analysed {
            ast,
            resolved: Resolved::new(&resolver, &result.picks),
            analysis: result.analysis,
        }
    });
    (diagnostics, analysed)
}

/// One document and the language features over it, for a caller that holds
/// the text itself rather than talking LSP over stdio, such as the browser
/// playground. Positions are LSP positions: a line and a UTF-16 column.
///
/// ```
/// # use ndc_lsp::{Document, ls_types::Position};
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
        if let Some(analysed) = analysed {
            self.state.set_analysis(analysed);
        }
        diagnostics
    }

    #[must_use]
    pub fn completion(&self, position: Position) -> CompletionResponse {
        completion::complete(Some(&self.state), position, &self.functions)
    }

    #[must_use]
    pub fn hover(&self, position: Position) -> Option<Hover> {
        hover::hover(&self.state, position, &self.functions)
    }

    #[must_use]
    pub fn inlay_hints(&self) -> Vec<InlayHint> {
        inlay_hints::collect(&self.state)
    }

    #[must_use]
    pub fn symbols(&self) -> Vec<DocumentSymbol> {
        symbols::document_symbols(&self.state)
    }

    /// Where the name under `position` is declared: usually one place, but a
    /// name in a generic function's body can have one for each instance.
    #[must_use]
    pub fn definition(&self, position: Position) -> Vec<Range> {
        definition::goto_definition(&self.state, position)
    }

    /// Every place the declarations of the name under `position` are named,
    /// with the declarations themselves if `include_declaration`.
    #[must_use]
    pub fn references(&self, position: Position, include_declaration: bool) -> Vec<Range> {
        references::references(&self.state, position, include_declaration)
    }

    /// The places to highlight while the cursor is on the name at
    /// `position`: every name that means the same declarations.
    #[must_use]
    pub fn highlights(&self, position: Position) -> Vec<DocumentHighlight> {
        references::highlights(&self.state, position)
    }

    /// The range of the name under `position`, if it can be renamed, and
    /// `None` when it isn't on a name; an error says why it can't be.
    pub fn prepare_rename(&self, position: Position) -> Result<Option<Range>, String> {
        let state = self.analysed().ok_or(references::NOT_ANALYSED)?;
        references::prepare_rename(state, position)
    }

    /// The edits that rename the name under `position` to `new_name`, and
    /// `None` when it isn't on a name; an error says why it can't be.
    pub fn rename(
        &self,
        position: Position,
        new_name: &str,
    ) -> Result<Option<Vec<TextEdit>>, String> {
        let state = self.analysed().ok_or(references::NOT_ANALYSED)?;
        references::rename(state, self.configure, &self.functions, position, new_name)
    }

    /// What the token types and modifiers of [`Self::semantic_tokens`]
    /// are, by their numbers.
    #[must_use]
    pub fn semantic_tokens_legend() -> ls_types::SemanticTokensLegend {
        semantic_tokens::legend()
    }

    /// A token for each name whose kind is known, encoded as the protocol
    /// wants them. While the text has changed since the last analysis, the
    /// names outside what changed keep theirs.
    #[must_use]
    pub fn semantic_tokens(&self) -> Vec<ls_types::SemanticToken> {
        semantic_tokens::tokens(&self.state)
    }

    /// The parameters of the call being written at `position`. Like
    /// completion, it works while the text doesn't parse, from what the last
    /// analysis of a text that did worked out.
    #[must_use]
    pub fn signature_help(&self, position: Position) -> Option<ls_types::SignatureHelp> {
        signature_help::signature_help(&self.state, &self.functions, position)
    }

    /// What can be folded.
    #[must_use]
    pub fn folding_ranges(&self) -> Vec<ls_types::FoldingRange> {
        ranges::folding_ranges(&self.state)
    }

    /// What expanding the selection at each of `positions` grows to.
    #[must_use]
    pub fn selection_ranges(&self, positions: &[Position]) -> Vec<ls_types::SelectionRange> {
        ranges::selection_ranges(&self.state, positions)
    }

    /// The edit that formats the text, none if it's formatted already; an
    /// error says why it can't be formatted, as when it doesn't parse.
    pub fn format(&self) -> Result<Vec<TextEdit>, String> {
        formatting::format(&self.state.source)
    }

    /// The state, if its analysis is of the current text.
    fn analysed(&self) -> Option<&DocumentState> {
        Some(&self.state).filter(|state| state.analysis_matches_source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ls_types::{CompletionItem, HoverContents};

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

        let ranges = document.definition(Position::new(1, 2));
        let [range] = ranges.as_slice() else {
            panic!("one definition: {ranges:?}");
        };
        assert_eq!(
            (range.start, range.end),
            (Position::new(0, 4), Position::new(0, 9))
        );
    }

    #[test]
    fn while_the_text_does_not_parse_the_features_answer_outside_what_changed() {
        let mut document = Document::new(ndc_stdlib::register);
        document.set_text("let total = 5;\ntotal + 1;");
        document.set_text("let total = 5;\ntotal +");
        assert!(document.hover(Position::new(1, 2)).is_some());
        let ranges = document.definition(Position::new(1, 2));
        assert_eq!(
            ranges.first().map(|range| range.start),
            Some(Position::new(0, 4))
        );
        assert!(!document.inlay_hints().is_empty());
        assert!(!document.symbols().is_empty());
        assert!(!document.semantic_tokens().is_empty());
    }

    #[test]
    fn answers_move_with_the_text_written_before_them() {
        let mut document = Document::new(ndc_stdlib::register);
        document.set_text("let total = 5;\nprint(total);\n");
        // A line being written above, which doesn't parse yet.
        document.set_text("let first = [\nlet total = 5;\nprint(total);\n");
        let ranges = document.definition(Position::new(2, 7));
        assert_eq!(
            ranges.first().map(|range| (range.start, range.end)),
            Some((Position::new(1, 4), Position::new(1, 9)))
        );
        let hint = document.inlay_hints().into_iter().next().expect("a hint");
        assert_eq!(hint.position, Position::new(1, 9));
    }

    #[test]
    fn a_name_that_changed_since_the_analysis_has_no_answer() {
        let mut document = Document::new(ndc_stdlib::register);
        document.set_text("let total = 5;\nprint(total);\n");
        // `total` became `totXal`, and the text no longer parses.
        document.set_text("let total = 5;\nprint(totXal);\nprint(");
        assert!(document.hover(Position::new(1, 8)).is_none());
        assert!(document.definition(Position::new(1, 8)).is_empty());
        // `print` before it still answers.
        assert!(document.hover(Position::new(1, 2)).is_some());
        // The references of `total` leave out the one that changed.
        let references = document.references(Position::new(0, 5), true);
        assert_eq!(references.len(), 1, "{references:?}");
    }
}
