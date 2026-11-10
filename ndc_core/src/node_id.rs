use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identity for an AST node. Used as a key in side tables (e.g. the
/// analyser's expression type map) so that tooling data doesn't bloat the AST.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

static NEXT_NODE_ID: AtomicU32 = AtomicU32::new(0);

impl NodeId {
    pub fn next() -> Self {
        Self(NEXT_NODE_ID.fetch_add(1, Ordering::Relaxed))
    }
}
