use std::collections::HashMap;

use ndc_core::StaticType;
use tower_lsp::lsp_types::InlayHint;

/// Per-document analysis state cached between edits.
pub struct DocumentState {
    pub hints: Vec<InlayHint>,
    pub source: String,
    /// Variable name -> inferred type, collected from analysed AST declarations.
    pub variable_types: HashMap<String, StaticType>,
}
