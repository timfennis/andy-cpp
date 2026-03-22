mod backend;
mod diagnostics;
mod features;
mod state;
mod util;
mod visitor;

use crate::backend::Backend;
use ndc_core::FunctionRegistry;
use ndc_interpreter::NativeFunction;
use std::rc::Rc;
use tower_lsp::{LspService, Server};

pub async fn start_lsp(configure: fn(&mut FunctionRegistry<Rc<NativeFunction>>)) {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(move |client| Backend::new(client, configure));
    Server::new(stdin, stdout, socket).serve(service).await;
}
