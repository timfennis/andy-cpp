use ndc_core::StaticType;
use ndc_interpreter::AnalysisResult;
use ndc_lexer::Span;
use ndc_parser::{ExpressionLocation, NodeId};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::util::LineIndex;
use crate::visitor::{AstVisitor, walk_ast};

/// Collect inlay type hints from an analysed AST.
///
/// Hints are derived on demand (not cached) so they always reflect the stored
/// AST. Only unannotated declarations and inferred return types get a hint.
pub fn collect(
    expressions: &[ExpressionLocation],
    analysis_result: &AnalysisResult,
    text: &str,
    line_index: &LineIndex,
) -> Vec<InlayHint> {
    let mut collector = HintCollector {
        text,
        line_index,
        analysis_result,
        hints: Vec::new(),
    };
    walk_ast(&mut collector, expressions);
    collector.hints
}

struct HintCollector<'a> {
    text: &'a str,
    line_index: &'a LineIndex,
    analysis_result: &'a AnalysisResult,
    hints: Vec<InlayHint>,
}

impl AstVisitor for HintCollector<'_> {
    fn on_declaration(
        &mut self,
        _identifier: &str,
        inferred_type: Option<&StaticType>,
        has_annotation: bool,
        span: Span,
    ) {
        if let Some(typ) = inferred_type
            && !has_annotation
        {
            self.hints.push(InlayHint {
                position: self.line_index.position(self.text, span.end()),
                label: InlayHintLabel::String(format!(": {typ}")),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: Some(true),
                data: None,
            });
        }
    }

    fn on_function_declaration(
        &mut self,
        return_type: Option<&StaticType>,
        parameters_span: Span,
        node_id: NodeId,
    ) {
        // return_type is Some only when explicitly annotated by the user — skip the hint.
        // Inferred return types are stored in the side table.
        if return_type.is_none()
            && let Some(rt) = self.analysis_result.inferred_return_types.get(&node_id)
        {
            self.hints.push(InlayHint {
                position: self.line_index.position(self.text, parameters_span.end()),
                label: InlayHintLabel::String(format!(" -> {rt}")),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;

    fn collect_hints(source: &str) -> Vec<InlayHint> {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let (expressions, analysis_result) = interpreter
            .analyse_str(source)
            .expect("analysis should succeed");
        let line_index = LineIndex::new(source);
        collect(&expressions, &analysis_result, source, &line_index)
    }

    fn has_label(hints: &[InlayHint], pred: impl Fn(&str) -> bool) -> bool {
        hints
            .iter()
            .any(|hint| matches!(&hint.label, InlayHintLabel::String(label) if pred(label)))
    }

    #[test]
    fn inferred_let_binding_gets_type_inlay() {
        let hints = collect_hints("let value = 1;");
        assert!(has_label(&hints, |l| l == ": Int"));
    }

    #[test]
    fn annotated_let_binding_skips_type_inlay() {
        let hints = collect_hints("let value: Int = 1;");
        assert!(!has_label(&hints, |l| l == ": Int"));
    }

    #[test]
    fn annotated_return_type_skips_inlay() {
        let hints = collect_hints("fn foo(x: Int) -> Int { x + 1 }");
        assert!(!has_label(&hints, |l| l.contains("->")));
    }

    #[test]
    fn inferred_return_type_gets_inlay() {
        let hints = collect_hints("fn foo() { 42 }");
        assert!(has_label(&hints, |l| l == " -> Int"));
    }

    #[test]
    fn annotated_param_skips_inlay() {
        let hints = collect_hints("fn foo(x: Int) { x }");
        assert!(!has_label(&hints, |l| l == ": Int"));
    }

    #[test]
    fn unannotated_param_gets_inlay() {
        let hints = collect_hints("fn foo(x) { x }");
        assert!(has_label(&hints, |l| l == ": Any"));
    }

    #[test]
    fn lambda_inside_call_gets_return_type_inlay() {
        let hints = collect_hints("[1,2,3].map(fn(y) => y / 2.0);");
        assert!(has_label(&hints, |l| l.starts_with(" -> ")));
    }

    #[test]
    fn lambda_inside_list_literal_gets_return_type_inlay() {
        let hints = collect_hints("let fns = [fn(x) => x + 1];");
        assert!(has_label(&hints, |l| l.starts_with(" -> ")));
    }
}
