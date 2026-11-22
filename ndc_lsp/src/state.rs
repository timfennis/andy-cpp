use ls_types::{Position, Range};
use ndc_core::collections::HashMap;

use ndc_analysis::Analysis;
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;

use crate::document::Analysed;
use crate::names::Names;
use crate::resolved::Resolved;
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
    /// spans are then of an older text. The AST-backed features work in that
    /// text and go through [`Self::offset_at`], [`Self::range_of`] and the
    /// like, which map positions and spans around what changed. Rename needs
    /// this to be true.
    pub analysis_matches_source: bool,
    /// Analysed AST from the last successful analysis.
    pub ast: Vec<ExpressionLocation>,
    /// The text `ast` was parsed from, which its spans are offsets into.
    analysed_source: String,
    /// Where `source` differs from `analysed_source`.
    edit: Edit,
    /// Every name written in `ast`.
    pub names: Names,
    /// What the names in `ast` refer to.
    pub resolved: Resolved,
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
            edit: Edit::default(),
            names: Names::default(),
            resolved: Resolved::default(),
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
        self.edit = Edit::between(&self.analysed_source, &self.source);
    }

    /// The state after analysing `source`, which must parse, with the
    /// standard library. Test-only convenience; the server commits analysis
    /// in place via [`set_analysis`](Self::set_analysis).
    #[cfg(test)]
    pub fn analysed(source: &str) -> Self {
        let (_, analysed) = crate::document::analyse(ndc_stdlib::register, source);
        let mut state = Self::from_source(source.to_string());
        state.set_analysis(analysed.expect("the source parses"));
        state
    }

    /// Replace the analysis (AST + side tables + derived completion caches) in
    /// place, leaving `source` and `line_index` untouched, and mark the
    /// analysis as matching the current source. Callers must ensure `ast` was
    /// produced from the current `source`.
    pub fn set_analysis(&mut self, analysed: Analysed) {
        let source = self.source.clone();
        self.set_analysis_of(source, analysed);
    }

    /// Replace the analysis with one of `text`, which may be an older text
    /// than `source`: the completion caches still work from it while the
    /// current text doesn't parse, and the AST-backed features stay off unless
    /// `text` is the current source.
    pub fn set_analysis_of(&mut self, text: String, analysed: Analysed) {
        let Analysed {
            ast,
            analysis,
            resolved,
        } = analysed;
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
        self.names = Names::collect(&ast, &text);
        self.ast = ast;
        self.analysis_matches_source = text == self.source;
        self.analysed_source = text;
        self.edit = Edit::between(&self.analysed_source, &self.source);
        self.analysis = analysis;
        self.resolved = resolved;
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
        let Edit {
            start,
            old_end,
            new_end,
        } = self.edit;
        if offset <= start {
            offset
        } else if offset >= new_end {
            old_end + (offset - new_end)
        } else {
            start
        }
    }

    // The AST-backed features work in the analysed text, which the analysis's
    // spans are offsets into, and go through these to and from the text the
    // editor has. While the two are one text, they change nothing; while the
    // text has changed since its last analysis, a position or a span outside
    // what changed still maps, and one inside it has no answer.

    /// The text the AST was parsed from.
    pub fn analysed_text(&self) -> &str {
        &self.analysed_source
    }

    /// The offset in the analysed text of `position` in the current one,
    /// unless it's inside what changed.
    pub fn offset_at(&self, position: Position) -> Option<usize> {
        let offset = self.line_index.offset(&self.source, position)?;
        let Edit {
            start,
            old_end,
            new_end,
        } = self.edit;
        if offset <= start {
            Some(offset)
        } else if offset >= new_end {
            Some(old_end + (offset - new_end))
        } else {
            None
        }
    }

    /// Where the point at `offset` in the analysed text is in the current
    /// one, unless the character before it changed.
    pub fn position_of(&self, offset: usize) -> Option<Position> {
        let offset = self.edit.end_to_current(offset)?;
        Some(self.line_index.position(&self.source, offset))
    }

    /// Where `span` of the analysed text is in the current one, unless one
    /// of its ends is inside what changed. A span holding all of what
    /// changed still maps, as a block someone is typing in.
    pub fn range_of(&self, span: Span) -> Option<Range> {
        let start = self.edit.start_to_current(span.offset())?;
        let end = self.edit.end_to_current(span.end())?;
        Some(Range {
            start: self.line_index.position(&self.source, start),
            end: self.line_index.position(&self.source, end.max(start)),
        })
    }

    /// Where the name at `span` of the analysed text is in the current one,
    /// unless any of it changed: typing inside `tot|al` makes another name.
    pub fn name_range(&self, span: Span) -> Option<Range> {
        let Edit { start, old_end, .. } = self.edit;
        let changed = span.offset() < old_end && span.end() > start;
        if changed {
            return None;
        }
        self.range_of(span)
    }
}

/// Where the current text differs from the analysed one, as one edit. The
/// analysed text is the last that parsed, and typing usually breaks a text
/// in one place, so one edit is usually what changed; after changes in two
/// places, all between them counts as changed. The two texts are alike
/// before `start`, and from `old_end` in the analysed text and `new_end` in
/// the current one on. While they're one text, all three are its length.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Edit {
    start: usize,
    old_end: usize,
    new_end: usize,
}

/// Whether `c` can be part of a name.
fn is_word(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '?'
}

fn starts_word(text: &str) -> bool {
    text.chars().next().is_some_and(is_word)
}

fn ends_word(text: &str) -> bool {
    text.chars().next_back().is_some_and(is_word)
}

impl Edit {
    fn between(old: &str, new: &str) -> Self {
        let (old_bytes, new_bytes) = (old.as_bytes(), new.as_bytes());
        let mut start = old_bytes
            .iter()
            .zip(new_bytes)
            .take_while(|(a, b)| a == b)
            .count();
        // Both texts have the same bytes up to `start`, so the same
        // character boundaries.
        while !old.is_char_boundary(start) {
            start -= 1;
        }
        // An edit doesn't start inside a word: typing `let x = (` in front of
        // `l.push(l)` puts it before the `l`, which is still the variable,
        // not after it, as the first letter of `let`.
        while start > 0
            && ends_word(&old[..start])
            && (starts_word(&old[start..]) || starts_word(&new[start..]))
        {
            start -= old[..start].chars().next_back().map_or(1, char::len_utf8);
        }
        let mut suffix = old_bytes[start..]
            .iter()
            .rev()
            .zip(new_bytes[start..].iter().rev())
            .take_while(|(a, b)| a == b)
            .count();
        while !old.is_char_boundary(old.len() - suffix) {
            suffix -= 1;
        }
        // Nor does it end inside one.
        while suffix > 0
            && starts_word(&old[old.len() - suffix..])
            && (ends_word(&old[..old.len() - suffix]) || ends_word(&new[..new.len() - suffix]))
        {
            suffix -= old[old.len() - suffix..]
                .chars()
                .next()
                .map_or(1, char::len_utf8);
        }
        Self {
            start,
            old_end: old.len() - suffix,
            new_end: new.len() - suffix,
        }
    }

    /// The current offset of a span starting at `offset` in the analysed
    /// text: its first character is before what changed or after it.
    fn start_to_current(self, offset: usize) -> Option<usize> {
        if offset >= self.old_end {
            Some(self.new_end + (offset - self.old_end))
        } else if offset < self.start {
            Some(offset)
        } else {
            None
        }
    }

    /// The current offset of a span ending at `offset` in the analysed
    /// text: its last character is before what changed or after it.
    fn end_to_current(self, offset: usize) -> Option<usize> {
        if offset <= self.start {
            Some(offset)
        } else if offset > self.old_end {
            Some(self.new_end + (offset - self.old_end))
        } else {
            None
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
        DocumentState::analysed(source).expression_types
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
