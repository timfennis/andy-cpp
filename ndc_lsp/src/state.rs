use ahash::AHashMap;

use ndc_core::StaticType;
use ndc_interpreter::AnalysisResult;
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;

use crate::util::LineIndex;
use crate::visitor::{AstVisitor, walk_ast};

/// Per-document analysis state cached between edits.
///
/// The analysed AST plus its [`AnalysisResult`] side tables are the source of
/// truth for position-based features (hover, document symbols, go-to-definition).
/// They reflect the last *successful* analysis.
pub struct DocumentState {
    pub source: String,
    pub line_index: LineIndex,
    /// Monotonic document version from the client (LSP `didChange`). Used to
    /// discard the result of a `validate` that an overlapping later edit has
    /// already superseded, so a slow analysis can't roll the buffer back.
    pub version: i32,
    /// Whether `ast`/`analysis` were produced from the current `source`. False
    /// while the buffer is mid-edit and the last parse failed: the AST and its
    /// spans are then stale relative to `source`, so AST-backed features (hover,
    /// go-to-definition, document symbols, inlay hints) must not run against it —
    /// they would map old spans onto edited text. The completion caches
    /// (`variable_types`/`expression_types`) stay usable regardless.
    pub analysis_matches_source: bool,
    /// Analysed AST from the last successful analysis.
    pub ast: Vec<ExpressionLocation>,
    /// Side tables (per-expression types, declaration spans, ...) from the last
    /// successful analysis.
    pub analysis: AnalysisResult,
    /// Variable name -> inferred type. Derived from `ast`, but kept as a flat map
    /// so that simple `ident.` dot-completion keeps working while the user is
    /// mid-edit and the buffer doesn't parse (the AST is stale then, but the
    /// variable's name and type are not).
    pub variable_types: AHashMap<String, StaticType>,
    /// Expression end offset -> inferred type, for dot-completion on arbitrary
    /// receiver expressions (e.g. `read_file("foo").`). Same resilience rationale.
    pub expression_types: AHashMap<usize, StaticType>,
}

impl DocumentState {
    /// Create a state holding only the source text (no analysis yet).
    pub fn from_source(source: String) -> Self {
        let line_index = LineIndex::new(&source);
        Self {
            source,
            line_index,
            version: 0,
            analysis_matches_source: false,
            ast: Vec::new(),
            analysis: AnalysisResult::default(),
            variable_types: AHashMap::new(),
            expression_types: AHashMap::new(),
        }
    }

    /// Build a state from a successful analysis. Test-only convenience; the
    /// server commits analysis in place via [`set_analysis`].
    #[cfg(test)]
    pub fn from_analysis(
        source: String,
        ast: Vec<ExpressionLocation>,
        analysis: AnalysisResult,
    ) -> Self {
        let mut state = Self::from_source(source);
        state.set_analysis(ast, analysis);
        state
    }

    /// Replace the analysis (AST + side tables + derived completion caches) in
    /// place, leaving `source`/`line_index`/`version` untouched, and mark the
    /// analysis as matching the current source. Callers must ensure `ast` was
    /// produced from the current `source`.
    pub fn set_analysis(&mut self, ast: Vec<ExpressionLocation>, analysis: AnalysisResult) {
        let mut collector = MapCollector {
            analysis: &analysis,
            variable_types: AHashMap::new(),
            expression_types: AHashMap::new(),
        };
        walk_ast(&mut collector, &ast);
        self.variable_types = collector.variable_types;
        self.expression_types = collector.expression_types;
        self.ast = ast;
        self.analysis = analysis;
        self.analysis_matches_source = true;
    }
}

/// Collects the flat lookup maps used by dot-completion from an analysed AST.
struct MapCollector<'a> {
    analysis: &'a AnalysisResult,
    variable_types: AHashMap<String, StaticType>,
    expression_types: AHashMap<usize, StaticType>,
}

impl AstVisitor for MapCollector<'_> {
    fn on_expression(&mut self, expr: &ExpressionLocation) {
        if let Some(typ) = self.analysis.expr_types.get(&expr.id) {
            self.expression_types.insert(expr.span.end(), typ.clone());
        }
    }

    fn on_declaration(
        &mut self,
        identifier: &str,
        inferred_type: Option<&StaticType>,
        _has_annotation: bool,
        _span: Span,
    ) {
        if let Some(typ) = inferred_type {
            self.variable_types
                .insert(identifier.to_string(), typ.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;

    #[test]
    fn from_source_is_not_fresh_until_analysed() {
        let state = DocumentState::from_source("let x = 1;".to_string());
        assert!(!state.analysis_matches_source);
    }

    #[test]
    fn from_analysis_is_fresh() {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let source = "let x = 1;";
        let (ast, analysis) = interpreter.analyse_str(source).expect("analysis succeeds");
        let state = DocumentState::from_analysis(source.to_string(), ast, analysis);
        assert!(state.analysis_matches_source);
    }
}
