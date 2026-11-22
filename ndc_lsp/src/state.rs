use ndc_core::collections::HashMap;

use ndc_analysis::Analysis;
use ndc_core::{NodeId, StaticType};
#[cfg(test)]
use ndc_interpreter::AnalysisResult;
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;

use crate::util::LineIndex;
use crate::visitor::{AstVisitor, walk_ast};

/// Per-document analysis state cached between edits.
///
/// The analysed AST plus its [`Analysis`] side tables are the source of
/// truth for position-based features (hover, document symbols, go-to-definition).
/// They reflect the last *successful* analysis.
pub struct DocumentState {
    pub source: String,
    pub line_index: LineIndex,
    /// Whether `ast`/`analysis` were produced from the current `source`. False
    /// while the buffer is mid-edit and the last parse failed: the AST and its
    /// spans are then stale relative to `source`, so AST-backed features (hover,
    /// go-to-definition, document symbols, inlay hints) must not run against it —
    /// they would map old spans onto edited text. The completion caches
    /// (`variable_types`/`expression_types`) stay usable regardless.
    pub analysis_matches_source: bool,
    /// Analysed AST from the last successful analysis.
    pub ast: Vec<ExpressionLocation>,
    /// The text `ast` was parsed from, which its spans are offsets into.
    analysed_source: String,
    /// What the last successful analysis worked out about `ast`: types,
    /// bindings and so on, keyed by node.
    pub analysis: Analysis,
    /// Variable name -> inferred type. Derived from `ast`, but kept as a flat map
    /// so that simple `ident.` dot-completion keeps working while the user is
    /// mid-edit and the buffer doesn't parse (the AST is stale then, but the
    /// variable's name and type are not).
    pub variable_types: HashMap<String, StaticType>,
    /// Expression end offset -> inferred type, for dot-completion on arbitrary
    /// receiver expressions (e.g. `read_file("foo").`). Same resilience rationale.
    /// Where several expressions end at one offset, it's the type of the
    /// smallest, as a dot binds tighter than anything around it: `1 < xs.len()`
    /// is `1 < (xs.len())`, so after `1 < xs` the receiver is `xs`.
    pub expression_types: HashMap<usize, StaticType>,
}

impl DocumentState {
    /// Create a state holding only the source text (no analysis yet).
    pub fn from_source(source: String) -> Self {
        let line_index = LineIndex::new(&source);
        Self {
            source,
            line_index,
            analysis_matches_source: false,
            ast: Vec::new(),
            analysed_source: String::new(),
            analysis: Analysis::default(),
            variable_types: HashMap::default(),
            expression_types: HashMap::default(),
        }
    }

    /// Replace the source text. The analysis now predates it, so AST-backed
    /// features stay off until [`set_analysis`](Self::set_analysis) commits one for this text.
    pub fn set_source(&mut self, source: &str) {
        self.source = source.to_string();
        self.line_index = LineIndex::new(source);
        self.analysis_matches_source = false;
    }

    /// Build a state from a successful analysis. Test-only convenience; the
    /// server commits analysis in place via [`set_analysis`](Self::set_analysis).
    #[cfg(test)]
    pub fn from_analysis(
        source: String,
        ast: Vec<ExpressionLocation>,
        result: AnalysisResult,
    ) -> Self {
        let mut state = Self::from_source(source);
        state.set_analysis(ast, result.analysis);
        state
    }

    /// Replace the analysis (AST + side tables + derived completion caches) in
    /// place, leaving `source` and `line_index` untouched, and mark the
    /// analysis as matching the current source. Callers must ensure `ast` was
    /// produced from the current `source`.
    pub fn set_analysis(&mut self, ast: Vec<ExpressionLocation>, analysis: Analysis) {
        let source = self.source.clone();
        self.set_analysis_of(source, ast, analysis);
    }

    /// Replace the analysis with one of `text`, which may be an older text
    /// than `source`: the completion caches still work from it while the
    /// current text doesn't parse, and the AST-backed features stay off unless
    /// `text` is the current source.
    pub fn set_analysis_of(
        &mut self,
        text: String,
        ast: Vec<ExpressionLocation>,
        analysis: Analysis,
    ) {
        let mut collector = MapCollector {
            analysis: &analysis,
            variable_types: HashMap::default(),
            expression_types: HashMap::default(),
        };
        walk_ast(&mut collector, &ast);
        self.variable_types = collector.variable_types;
        self.expression_types = collector
            .expression_types
            .into_iter()
            .map(|(end, (_start, typ))| (end, typ))
            .collect();
        self.ast = ast;
        self.analysis_matches_source = text == self.source;
        self.analysed_source = text;
        self.analysis = analysis;
    }

    /// The length of the text the AST was parsed from.
    pub fn analysed_len(&self) -> usize {
        self.analysed_source.len()
    }

    /// Where `offset` in `source` was in the text the AST was parsed from,
    /// taking everything between the first and the last byte that changed as
    /// one edit. An offset before the edit stays, one after it moves with the
    /// text after it, and one inside it is the start of the edit. While the
    /// text doesn't parse, that's how a cursor finds the scopes it's in: in
    /// `{ n.| }`, typing `n.` moved the brace, and the cursor is still
    /// inside the block.
    pub fn analysed_offset(&self, offset: usize) -> usize {
        let (old, new) = (self.analysed_source.as_bytes(), self.source.as_bytes());
        let prefix = old.iter().zip(new).take_while(|(a, b)| a == b).count();
        if offset <= prefix {
            return offset;
        }
        let max_suffix = old.len().min(new.len()) - prefix;
        let suffix = old
            .iter()
            .rev()
            .zip(new.iter().rev())
            .take(max_suffix)
            .take_while(|(a, b)| a == b)
            .count();
        if offset >= new.len() - suffix {
            old.len() - (new.len() - offset)
        } else {
            prefix
        }
    }
}

/// Collects the flat lookup maps used by dot-completion from an analysed AST.
struct MapCollector<'a> {
    analysis: &'a Analysis,
    variable_types: HashMap<String, StaticType>,
    /// End offset -> start offset and type of the smallest expression found
    /// ending there so far.
    expression_types: HashMap<usize, (usize, StaticType)>,
}

impl AstVisitor for MapCollector<'_> {
    /// Keeps the expression that starts last among those that end at the
    /// same offset. Of two with the same span, it keeps the inner one, which
    /// the walk reaches later: a statement has its expression's span but not
    /// its type.
    fn on_expression(&mut self, expr: &ExpressionLocation) {
        let Some(typ) = self.analysis.expr_types.get(&expr.id) else {
            return;
        };
        let start = expr.span.offset();
        match self.expression_types.get(&expr.span.end()) {
            Some((kept_start, _)) if *kept_start > start => {}
            _ => {
                self.expression_types
                    .insert(expr.span.end(), (start, typ.clone()));
            }
        }
    }

    fn on_declaration(
        &mut self,
        identifier: &str,
        node: NodeId,
        _has_annotation: bool,
        _span: Span,
    ) {
        if let Some(typ) = self.analysis.variable_types.get(&node) {
            self.variable_types
                .insert(identifier.to_string(), typ.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expression_types(source: &str) -> HashMap<usize, StaticType> {
        let mut interpreter = ndc_interpreter::Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let (ast, result) = interpreter
            .analyse_str(source)
            .expect("analysis should succeed");
        DocumentState::from_analysis(source.to_string(), ast, result).expression_types
    }

    #[test]
    fn an_offset_holds_the_type_of_the_smallest_expression_ending_there() {
        // `1 < 2`, its `2` and the statement around it all end at byte 5, and
        // a dot typed there would call a method on the `2`.
        let types = expression_types("1 < 2;");
        assert_eq!(types.get(&5), Some(&StaticType::Int));

        let list = Some(StaticType::List(Box::new(StaticType::Int)));

        // `let xs = [1]` and its `[1]` end together; `len(xs)` ends after `xs`.
        let types = expression_types("let xs = [1]; len(xs); 1 < len(xs); xs = [2];");
        assert_eq!(types.get(&12).cloned(), list);
        assert_eq!(types.get(&21), Some(&StaticType::Int));
        assert_eq!(types.get(&34), Some(&StaticType::Int));
        // An assignment ends with its value too.
        assert_eq!(types.get(&44).cloned(), list);
    }

    #[test]
    fn a_grouping_holds_the_type_of_what_it_groups() {
        // `(1 < 2)` ends after its parenthesis, past the `2`.
        let types = expression_types("(1 < 2);");
        assert_eq!(types.get(&7), Some(&StaticType::Bool));
        assert_eq!(types.get(&6), Some(&StaticType::Int));
    }

    #[test]
    fn a_document_without_analysis_does_not_match_its_source() {
        let state = DocumentState::from_source("let x = 1;".to_string());
        assert!(!state.analysis_matches_source);
    }

    #[test]
    fn an_offset_moves_with_the_text_around_an_edit() {
        let mut state = DocumentState::from_source(String::new());
        state.analysed_source = "{ let n = 5; }".to_string();
        state.set_source("{ let n = 5; n. }");
        // Before the edit, offsets stay.
        assert_eq!(state.analysed_offset(2), 2);
        // After it they move back with the brace, and inside it they are its start.
        assert_eq!(state.analysed_offset(16), 13);
        assert_eq!(state.analysed_offset(15), 13);
        assert_eq!(state.analysed_offset(14), 13);
        // Text that didn't change maps to itself.
        state.set_source("{ let n = 5; }");
        assert_eq!(state.analysed_offset(13), 13);
    }
}
