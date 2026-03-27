use std::collections::HashMap;

use ndc_core::StaticType;
use ndc_interpreter::AnalysisResult;
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::util::position_from_offset;
use crate::visitor::{AstVisitor, walk_ast};

/// Results of collecting information from an analysed AST.
pub struct AnalysisInfo {
    pub hints: Vec<InlayHint>,
    pub variable_types: HashMap<String, StaticType>,
    /// Maps expression end offset -> inferred type for dot-completion on
    /// arbitrary expressions (e.g. `read_file("foo").`).
    pub expression_types: HashMap<usize, StaticType>,
}

/// Collect inlay hints, variable types, and expression types from an analysed AST.
pub fn collect(
    expressions: &[ExpressionLocation],
    analysis_result: &AnalysisResult,
    text: &str,
) -> AnalysisInfo {
    let mut collector = HintCollector {
        text,
        analysis_result,
        hints: Vec::new(),
        variable_types: HashMap::new(),
        expression_types: HashMap::new(),
    };
    walk_ast(&mut collector, expressions);
    AnalysisInfo {
        hints: collector.hints,
        variable_types: collector.variable_types,
        expression_types: collector.expression_types,
    }
}

struct HintCollector<'a> {
    text: &'a str,
    analysis_result: &'a AnalysisResult,
    hints: Vec<InlayHint>,
    variable_types: HashMap<String, StaticType>,
    expression_types: HashMap<usize, StaticType>,
}

impl AstVisitor for HintCollector<'_> {
    fn on_expression(&mut self, expr: &ExpressionLocation) {
        if let Some(typ) = self.analysis_result.expr_types.get(&expr.id) {
            self.expression_types.insert(expr.span.end(), typ.clone());
        }
    }

    fn on_declaration(
        &mut self,
        identifier: &str,
        inferred_type: Option<&StaticType>,
        has_annotation: bool,
        span: Span,
    ) {
        if let Some(typ) = inferred_type {
            if !has_annotation {
                self.hints.push(InlayHint {
                    position: position_from_offset(self.text, span.end()),
                    label: InlayHintLabel::String(format!(": {typ}")),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: Some(true),
                    data: None,
                });
            }
            self.variable_types
                .insert(identifier.to_string(), typ.clone());
        }
    }

    fn on_function_declaration(&mut self, return_type: Option<&StaticType>, parameters_span: Span) {
        if let Some(rt) = return_type {
            self.hints.push(InlayHint {
                position: position_from_offset(self.text, parameters_span.end()),
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

    fn collect_hints(source: &str) -> AnalysisInfo {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        let (expressions, analysis_result) = interpreter
            .analyse_str(source)
            .expect("analysis should succeed");
        collect(&expressions, &analysis_result, source)
    }

    #[test]
    fn inferred_let_binding_gets_type_inlay() {
        let info = collect_hints("let value = 1;");
        assert!(
            info.hints
                .iter()
                .any(|hint| matches!(&hint.label, InlayHintLabel::String(label) if label == ": Int"))
        );
    }

    #[test]
    fn annotated_let_binding_skips_type_inlay() {
        let info = collect_hints("let value: Int = 1;");
        assert!(
            !info.hints
                .iter()
                .any(|hint| matches!(&hint.label, InlayHintLabel::String(label) if label == ": Int"))
        );
        assert_eq!(info.variable_types.get("value"), Some(&StaticType::Int));
    }
}
