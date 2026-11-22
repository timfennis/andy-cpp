//! The language server: a synchronous JSON-RPC loop over stdio (`lsp-server`)
//! that answers requests on the main thread, and one worker thread that
//! analyses documents, so a slow analysis never holds up completion or hover.

use std::panic::{self, AssertUnwindSafe};
use std::sync::mpsc;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::thread;

use ndc_core::collections::HashMap;

use ls_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, LogMessage,
    PublishDiagnostics,
};
use ls_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, InlayHintRequest,
};
use ls_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, HoverProviderCapability, InlayHint, InlayHintParams, Location,
    LogMessageParams, MessageType, OneOf, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};

use crate::document::{self, Configure};
use crate::features::completion::FunctionInfo;
use crate::features::{completion, definition, hover, inlay_hints, symbols};
use crate::state::DocumentState;

type Error = Box<dyn std::error::Error + Send + Sync>;

/// Serves the language server over stdin and stdout until the client exits.
pub fn serve(configure: Configure) -> Result<(), Error> {
    let (connection, io_threads) = Connection::stdio();
    connection.initialize(serde_json::to_value(capabilities())?)?;

    let documents = Documents::default();
    let (jobs, queue) = mpsc::channel();
    let worker = {
        let documents = documents.clone();
        let sender = connection.sender.clone();
        thread::Builder::new()
            .name("analysis".to_string())
            .spawn(move || analyse_jobs(&queue, &documents, &sender, configure))?
    };

    let server = Server {
        connection,
        documents,
        jobs,
        functions: FunctionInfo::collect(&document::interpreter(configure)),
    };
    notify::<LogMessage>(
        &server.connection.sender,
        LogMessageParams {
            typ: MessageType::INFO,
            message: "server initialized".to_string(),
        },
    );
    server.run()?;

    // Dropping the server closes the job queue, which ends the worker, and the
    // connection, which ends the writer thread.
    drop(server);
    worker
        .join()
        .map_err(|_panic| "the analysis thread panicked".to_string())?;
    io_threads.join()?;
    Ok(())
}

/// What the server can do, as it tells the client in `initialize`.
fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string()]),
            ..Default::default()
        }),
        inlay_hint_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

/// An open document: its state, and the client's version of the text in it.
struct OpenDocument {
    /// Monotonic document version from the client (LSP `didChange`). Used to
    /// discard the result of an analysis that a later edit has already
    /// superseded, so a slow analysis can't roll the buffer back.
    version: i32,
    state: DocumentState,
}

/// The open documents, shared by the thread that answers requests and the one
/// that analyses.
#[derive(Clone, Default)]
struct Documents(Arc<RwLock<HashMap<Uri, OpenDocument>>>);

impl Documents {
    // A panic while holding the lock leaves the map as it was between two
    // whole updates, so a poisoned lock is still safe to use.
    fn read(&self) -> RwLockReadGuard<'_, HashMap<Uri, OpenDocument>> {
        self.0.read().unwrap_or_else(PoisonError::into_inner)
    }

    fn write(&self) -> RwLockWriteGuard<'_, HashMap<Uri, OpenDocument>> {
        self.0.write().unwrap_or_else(PoisonError::into_inner)
    }
}

/// A version of a document's text, waiting to be analysed.
struct Job {
    uri: Uri,
    text: String,
    version: i32,
}

// Staleness & concurrency model
// ------------------------------
// The main thread handles messages in order. An edit updates the document's
// source and `version` right away, so completion (triggered by `.`) always sees
// the latest text, then queues the text for the analysis thread. That thread
// analyses off the lock and commits the result and publishes its diagnostics
// only if the stored `version` still equals the one it analysed, so a slow run
// can't roll the buffer back to older text. It holds the lock across the commit
// and the publish, as `didClose` does across its removal and its publish, so a
// close can't slip in between and leave stale diagnostics on a closed file.
// AST-backed features (hover, go-to-def, symbols, inlay hints) additionally
// gate on `analysis_matches_source`, so they never map stale spans onto edited
// text. The completion caches (`variable_types` by name, `expression_types` by
// end-offset) are intentionally resilient: dot completion fires on a buffer
// that doesn't parse (`x.`), and the data it reads sits at the cursor where
// offsets are stable for the appended `.`.

/// Analyses the documents queued on `queue` until the queue closes. When edits
/// come in faster than it analyses, it skips to the latest text of each document.
fn analyse_jobs(
    queue: &mpsc::Receiver<Job>,
    documents: &Documents,
    sender: &crossbeam_channel::Sender<Message>,
    configure: Configure,
) {
    while let Ok(first) = queue.recv() {
        let mut latest = HashMap::default();
        for job in std::iter::once(first).chain(queue.try_iter()) {
            latest.insert(job.uri.clone(), job);
        }
        for job in latest.into_values() {
            // A bug in the analyser mustn't end this thread, or diagnostics
            // would stop for the rest of the session: skip the text instead.
            let Ok((diagnostics, analysed)) =
                panic::catch_unwind(AssertUnwindSafe(|| document::analyse(configure, &job.text)))
            else {
                notify::<LogMessage>(
                    sender,
                    LogMessageParams {
                        typ: MessageType::ERROR,
                        message: format!(
                            "the analyser panicked on version {} of {}",
                            job.version,
                            job.uri.as_str()
                        ),
                    },
                );
                continue;
            };

            let mut docs = documents.write();
            let Some(open) = docs.get_mut(&job.uri) else {
                continue;
            };
            // A reopened document starts its versions over, so the version
            // alone could match a job queued before the close.
            if open.version != job.version || open.state.source != job.text {
                continue;
            }
            // On success, commit in place (keeps the current source/line_index).
            // On parse failure, keep the last good AST; `analysis_matches_source`
            // is already false, so AST-backed features stay disabled.
            if let Some((ast, analysis)) = analysed {
                open.state.set_analysis(ast, analysis);
            }
            publish_diagnostics(sender, job.uri, diagnostics, Some(job.version));
        }
    }
}

fn publish_diagnostics(
    sender: &crossbeam_channel::Sender<Message>,
    uri: Uri,
    diagnostics: Vec<Diagnostic>,
    version: Option<i32>,
) {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version,
    };
    notify::<PublishDiagnostics>(sender, params);
}

fn notify<N: ls_types::notification::Notification>(
    sender: &crossbeam_channel::Sender<Message>,
    params: N::Params,
) {
    // The send fails only once the client has gone, and then nobody's listening.
    let _ = sender.send(Notification::new(N::METHOD.to_string(), params).into());
}

struct Server {
    connection: Connection,
    documents: Documents,
    jobs: mpsc::Sender<Job>,
    /// Native-function metadata, snapshotted once at startup. The set of native
    /// functions never changes, so completion and hover read this instead of
    /// rebuilding an interpreter per request.
    functions: Vec<FunctionInfo>,
}

impl Server {
    fn run(&self) -> Result<(), Error> {
        for message in &self.connection.receiver {
            match message {
                Message::Request(request) => {
                    if self.connection.handle_shutdown(&request)? {
                        return Ok(());
                    }
                    self.handle_request(request);
                }
                Message::Notification(notification) => self.handle_notification(notification),
                Message::Response(_) => {}
            }
        }
        Ok(())
    }

    /// Answers a request. A new request is one more `on` here, with its
    /// capability in [`capabilities`].
    fn handle_request(&self, request: Request) {
        Requests {
            server: self,
            request: Some(request),
        }
        .on::<Completion>(Self::completion)
        .on::<HoverRequest>(Self::hover)
        .on::<GotoDefinition>(Self::goto_definition)
        .on::<DocumentSymbolRequest>(Self::document_symbol)
        .on::<InlayHintRequest>(Self::inlay_hint)
        .finish();
    }

    /// Acts on a notification; one the server doesn't know is ignored.
    fn handle_notification(&self, notification: Notification) {
        Notifications {
            server: self,
            notification: Some(notification),
        }
        .on::<DidOpenTextDocument>(Self::did_open)
        .on::<DidChangeTextDocument>(Self::did_change)
        .on::<DidCloseTextDocument>(Self::did_close);
    }

    /// Updates the document's source and version right away, then queues the
    /// new text for analysis.
    fn update_source(&self, uri: Uri, text: String, version: i32) {
        {
            let mut docs = self.documents.write();
            if let Some(open) = docs.get_mut(&uri) {
                // The AST now predates this edit; the analysis commits a new
                // one if the new source parses.
                open.state.set_source(&text);
                open.version = version;
            } else {
                let state = DocumentState::from_source(text.clone());
                docs.insert(uri.clone(), OpenDocument { version, state });
            }
        }
        // The send fails only once the worker has gone, at shutdown.
        let _ = self.jobs.send(Job { uri, text, version });
    }

    fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;
        self.update_source(document.uri, document.text, document.version);
    }

    fn did_change(&self, params: DidChangeTextDocumentParams) {
        // Full-document sync: the last change carries the whole buffer, so only
        // the final one matters.
        if let Some(change) = params.content_changes.into_iter().next_back() {
            let document = params.text_document;
            self.update_source(document.uri, change.text, document.version);
        }
    }

    fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        // Drop cached state so the map doesn't grow unbounded, and clear the
        // document's diagnostics, holding the lock across both (see the model
        // above `analyse_jobs`).
        let mut docs = self.documents.write();
        docs.remove(&uri);
        publish_diagnostics(&self.connection.sender, uri, Vec::new(), None);
    }

    fn inlay_hint(&self, params: &InlayHintParams) -> Option<Vec<InlayHint>> {
        let docs = self.documents.read();
        docs.get(&params.text_document.uri)
            .map(|open| &open.state)
            .map(|state| {
                if !state.analysis_matches_source {
                    return Vec::new();
                }
                inlay_hints::collect(
                    &state.ast,
                    &state.analysis,
                    &state.source,
                    &state.line_index,
                )
            })
    }

    fn hover(&self, params: &HoverParams) -> Option<Hover> {
        let docs = self.documents.read();
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        docs.get(uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)
            .and_then(|state| hover::hover(state, position, &self.functions))
    }

    fn goto_definition(&self, params: &GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let docs = self.documents.read();
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        docs.get(uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)
            .and_then(|state| {
                definition::goto_definition(state, position).map(|range| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range,
                    })
                })
            })
    }

    fn document_symbol(&self, params: &DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
        let docs = self.documents.read();
        docs.get(&params.text_document.uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)
            .map(|state| {
                DocumentSymbolResponse::Nested(symbols::document_symbols(
                    &state.ast,
                    &state.analysis,
                    &state.source,
                    &state.line_index,
                ))
            })
    }

    fn completion(&self, params: &CompletionParams) -> Option<CompletionResponse> {
        let docs = self.documents.read();
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        Some(completion::complete(
            docs.get(uri).map(|open| &open.state),
            position,
            &self.functions,
        ))
    }
}

/// Hands a request to the handler of its method, and answers it.
struct Requests<'a> {
    server: &'a Server,
    /// The request, until a handler takes it.
    request: Option<Request>,
}

impl Requests<'_> {
    fn on<R: ls_types::request::Request>(
        mut self,
        handler: fn(&Server, &R::Params) -> R::Result,
    ) -> Self {
        let Some(request) = self.request.take_if(|request| request.method == R::METHOD) else {
            return self;
        };
        let response = match serde_json::from_value(request.params) {
            Ok(params) => Response::new_ok(request.id, handler(self.server, &params)),
            Err(error) => Response::new_err(
                request.id,
                ErrorCode::InvalidParams as i32,
                error.to_string(),
            ),
        };
        let _ = self.server.connection.sender.send(response.into());
        self
    }

    /// Answers a request that no handler took.
    fn finish(self) {
        if let Some(request) = self.request {
            let response = Response::new_err(
                request.id,
                ErrorCode::MethodNotFound as i32,
                format!("unhandled method: {}", request.method),
            );
            let _ = self.server.connection.sender.send(response.into());
        }
    }
}

/// Hands a notification to the handler of its method.
struct Notifications<'a> {
    server: &'a Server,
    /// The notification, until a handler takes it.
    notification: Option<Notification>,
}

impl Notifications<'_> {
    fn on<N: ls_types::notification::Notification>(
        mut self,
        handler: fn(&Server, N::Params),
    ) -> Self {
        let taken = self
            .notification
            .take_if(|notification| notification.method == N::METHOD);
        // A notification has no answer, so one with bad params is dropped.
        if let Some(Ok(params)) = taken.map(|n| serde_json::from_value(n.params)) {
            handler(self.server, params);
        }
        self
    }
}
