//! Language features for Andy C++ (completion, hover, diagnostics, inlay hints,
//! document symbols) as plain functions over a [`Document`], and with the
//! `server` feature, the language server that answers them over stdio.

#[cfg(feature = "server")]
mod backend;
mod diagnostics;
mod document;
mod features;
#[cfg(test)]
mod fixture;
mod scope_resolve;
mod state;
mod util;
mod visitor;

pub use document::{Configure, Document};
pub use ls_types;

/// Serves the language server over stdin and stdout until the client exits.
#[cfg(feature = "server")]
pub use backend::serve as start_lsp;
