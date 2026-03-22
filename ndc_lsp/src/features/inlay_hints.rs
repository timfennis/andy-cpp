use std::collections::HashMap;

use ndc_core::StaticType;
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::util::position_from_offset;
use crate::visitor::{AstVisitor, walk_ast};

/// Collect inlay hints and variable type mappings from an analysed AST.
pub fn collect(
    expressions: &[ExpressionLocation],
    text: &str,
) -> (Vec<InlayHint>, HashMap<String, StaticType>) {
    let mut collector = HintCollector {
        text,
        hints: Vec::new(),
        variable_types: HashMap::new(),
    };
    walk_ast(&mut collector, expressions);
    (collector.hints, collector.variable_types)
}

struct HintCollector<'a> {
    text: &'a str,
    hints: Vec<InlayHint>,
    variable_types: HashMap<String, StaticType>,
}

impl AstVisitor for HintCollector<'_> {
    fn on_declaration(&mut self, identifier: &str, inferred_type: Option<&StaticType>, span: Span) {
        if let Some(typ) = inferred_type {
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
