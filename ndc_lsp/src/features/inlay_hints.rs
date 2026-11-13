use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};
use ndc_analysis::Analysis;
use ndc_core::NodeId;
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;

use crate::util::LineIndex;
use crate::visitor::{AstVisitor, walk_ast};

/// Collect inlay type hints from an analysed AST.
///
/// Hints are derived on demand (not cached) so they always reflect the stored
/// AST. Only unannotated declarations and inferred return types get a hint.
pub fn collect(
    expressions: &[ExpressionLocation],
    analysis: &Analysis,
    text: &str,
    line_index: &LineIndex,
) -> Vec<InlayHint> {
    let mut collector = HintCollector {
        text,
        line_index,
        analysis,
        hints: Vec::new(),
    };
    walk_ast(&mut collector, expressions);
    collector.hints
}

struct HintCollector<'a> {
    text: &'a str,
    line_index: &'a LineIndex,
    analysis: &'a Analysis,
    hints: Vec<InlayHint>,
}

impl AstVisitor for HintCollector<'_> {
    fn on_declaration(
        &mut self,
        _identifier: &str,
        node: NodeId,
        has_annotation: bool,
        span: Span,
    ) {
        if let Some(typ) = self.analysis.variable_types.get(&node)
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
        has_return_annotation: bool,
        parameters_span: Span,
        node_id: NodeId,
    ) {
        // An annotated return type needs no hint; the analysis only infers the
        // return types of functions without one.
        if !has_return_annotation
            && let Some(rt) = self.analysis.inferred_return_types.get(&node_id)
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
        let (expressions, result) = interpreter
            .analyse_str(source)
            .expect("analysis should succeed");
        let line_index = LineIndex::new(source);
        collect(&expressions, &result.analysis, source, &line_index)
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
    fn a_failed_check_is_shown_as_any() {
        // `len(true)` fails, and what it gives is shown as `Any`.
        let hints = collect_hints("let n = len(true);\nlet m = [n, n];");
        assert!(has_label(&hints, |l| l == ": Any"), "{hints:?}");
        assert!(has_label(&hints, |l| l == ": List<Any>"), "{hints:?}");
    }

    #[test]
    fn annotated_let_binding_skips_type_inlay() {
        let hints = collect_hints("let value: Int = 1;");
        assert!(!has_label(&hints, |l| l == ": Int"));
    }

    #[test]
    fn constructed_struct_gets_type_inlay() {
        let hints = collect_hints("struct Point { x: Int, y: Int }\nlet p = Point(1, 2);");
        assert!(has_label(&hints, |l| l == ": Point"));
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
        // `foo` takes any argument, so its parameter's type is `foo`'s type
        // parameter, which it returns.
        let hints = collect_hints("fn foo(x) { x }");
        assert!(has_label(&hints, |l| l == ": A"), "{hints:?}");
        assert!(has_label(&hints, |l| l == " -> A"), "{hints:?}");
    }

    #[test]
    fn a_lambda_nested_in_an_expression_gets_a_return_type_hint() {
        for (source, label) in [
            ("[1, 2, 3].map(fn(y: Int) => y / 2.0);", " -> Float"),
            ("let fns = [fn(x: Int) => x + 1];", " -> Int"),
        ] {
            let hints = collect_hints(source);
            assert!(
                has_label(&hints, |l| l == label),
                "expected {label:?} in {source:?}, got {hints:?}"
            );
        }
    }
}
