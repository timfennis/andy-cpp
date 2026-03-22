use std::collections::HashMap;

use ndc_core::StaticType;
use tower_lsp::lsp_types::InlayHint;

/// Per-document analysis state cached between edits.
pub struct DocumentState {
    pub hints: Vec<InlayHint>,
    pub source: String,
    /// Variable name -> inferred type, collected from analysed AST declarations.
    pub variable_types: HashMap<String, StaticType>,
    /// Expression end offset -> inferred type, for looking up the receiver type
    /// of dot-completion on arbitrary expressions (e.g. `read_file("foo").`).
    pub expression_types: HashMap<usize, StaticType>,
}
