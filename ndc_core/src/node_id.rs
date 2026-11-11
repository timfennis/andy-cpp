use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identity for an AST node. Everything the analyser works out about a
/// node is keyed by it, so the AST itself stays purely syntactic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

static NEXT_NODE_ID: AtomicU32 = AtomicU32::new(0);

impl NodeId {
    pub fn next() -> Self {
        Self(NEXT_NODE_ID.fetch_add(1, Ordering::Relaxed))
    }
}
