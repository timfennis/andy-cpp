//! Language features for Andy C++ (completion, hover, diagnostics, inlay hints,
//! document symbols) as plain functions over a [`Document`], and with the
//! `server` feature, the language server that answers them over stdio.

#[cfg(feature = "server")]
mod backend;
mod diagnostics;
mod document;
mod features;
mod scope_resolve;
mod state;
mod util;
mod visitor;

pub use document::{Configure, Document};
pub use lsp_types;

/// Serves the language server over stdin and stdout until the client exits.
#[cfg(feature = "server")]
pub async fn start_lsp(configure: Configure) {
    use tower_lsp::{LspService, Server};

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(move |client| backend::Backend::new(client, configure));
    Server::new(stdin, stdout, socket).serve(service).await;
}
