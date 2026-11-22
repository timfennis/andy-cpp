//! The language server: a synchronous JSON-RPC loop over stdio (`lsp-server`)
//! that answers requests on the main thread, and one worker thread that
//! analyses documents, so a slow analysis never holds up completion or hover.

use std::cell::Cell;
use std::panic::{self, AssertUnwindSafe};
use std::sync::mpsc;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::thread;

use ndc_core::collections::HashMap;

use ls_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Exit, LogMessage,
    Notification as _, PublishDiagnostics,
};
use ls_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, InlayHintRefreshRequest,
    InlayHintRequest, Request as _,
};
use ls_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, HoverProviderCapability, InitializeParams, InlayHint, InlayHintParams,
    Location, LogMessageParams, MessageType, OneOf, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};

use crate::document::{self, Configure};
use crate::features::completion::FunctionInfo;
use crate::features::{completion, definition, hover, inlay_hints, symbols};
use crate::state::DocumentState;

type Error = Box<dyn std::error::Error + Send + Sync>;

/// Serves the language server over stdin and stdout until the client exits.
pub fn serve(configure: Configure) -> Result<(), Error> {
    let (connection, io_threads) = Connection::stdio();
    let params: InitializeParams =
        serde_json::from_value(connection.initialize(serde_json::to_value(capabilities())?)?)?;
    // An inlay hint request that comes in before the analysis of its text has
    // nothing to answer with, so the client is told to ask again once there is.
    let refresh_inlay_hints = params
        .capabilities
        .workspace
        .and_then(|workspace| workspace.inlay_hint)
        .and_then(|inlay_hint| inlay_hint.refresh_support)
        .unwrap_or(false);

    let documents = Documents::default();
    let (jobs, queue) = mpsc::channel();
    let worker = {
        let documents = documents.clone();
        let sender = connection.sender.clone();
        thread::Builder::new()
            .name("analysis".to_string())
            .spawn(move || {
                analyse_jobs(&queue, &documents, &sender, configure, refresh_inlay_hints);
            })?
    };

    let server = Server {
        connection,
        documents,
        jobs,
        openings: Cell::new(0),
        functions: FunctionInfo::collect(&document::interpreter(configure)),
    };
    notify::<LogMessage>(
        &server.connection.sender,
        LogMessageParams {
            typ: MessageType::INFO,
            message: "server initialized".to_string(),
        },
    );
    // The server doesn't wait for the analysis thread, or a large document's
    // analysis could hold up the exit for seconds, nor for the writer thread,
    // which the analysis thread keeps open: the client has had the answer to
    // `shutdown` by the time it sends `exit`, and the threads end with the
    // process.
    drop((worker, io_threads));
    server.run()
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
    /// publish diagnostics only for the current text.
    version: i32,
    /// Which opening of the document this is, as the client starts the
    /// versions over when it opens a document again.
    opening: u64,
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
    opening: u64,
}

// Staleness & concurrency model
// ------------------------------
// The main thread handles messages in order. An edit updates the document's
// source and `version` right away, so completion (triggered by `.`) always sees
// the latest text, then queues the text for the analysis thread. That thread
// analyses off the lock. When it's free it takes everything queued since it
// last looked, and of each document analyses only the newest text: while it's
// keeping up that's every text, and when it falls behind it skips to the
// newest instead of building a backlog. If the newest text doesn't parse (`x.`,
// just after typing the dot), it also analyses the newest skipped text that
// does, so completion keeps what the last text that parsed worked out.
//
// It commits an analysis that parsed even when a later edit has superseded
// it, as the last good one: the completion caches (`variable_types` by name,
// `expression_types` by end offset) work from it while the current text
// doesn't parse, mapping the cursor back into the text it was of, and
// `analysis_matches_source` stays false, so the AST-backed features (hover,
// go-to-def, symbols, inlay hints) never map its spans onto edited text. Each
// text it commits is newer than any it committed before, so a commit never
// replaces a newer analysis. It publishes diagnostics only for the current
// text. It holds the lock across the commit and the publish, as `didClose`
// does across its removal and its publish, so a close can't slip in between
// and leave stale diagnostics on a closed file.

/// Analyses the texts queued on `queue` until the queue closes.
fn analyse_jobs(
    queue: &mpsc::Receiver<Job>,
    documents: &Documents,
    sender: &crossbeam_channel::Sender<Message>,
    configure: Configure,
    refresh_inlay_hints: bool,
) {
    let mut worker = Worker {
        documents,
        sender,
        configure,
        refresh_inlay_hints,
        refreshes: 0,
    };
    while let Ok(first) = queue.recv() {
        // What came in since the last look, by opening of a document, oldest first.
        let mut queued: Vec<Vec<Job>> = Vec::new();
        for job in std::iter::once(first).chain(queue.try_iter()) {
            let same =
                |texts: &&mut Vec<Job>| texts[0].uri == job.uri && texts[0].opening == job.opening;
            match queued.iter_mut().find(same) {
                Some(texts) => texts.push(job),
                None => queued.push(vec![job]),
            }
        }
        for texts in queued {
            worker.catch_up(texts);
        }
    }
}

/// The analysis thread's state.
struct Worker<'a> {
    documents: &'a Documents,
    sender: &'a crossbeam_channel::Sender<Message>,
    configure: Configure,
    refresh_inlay_hints: bool,
    /// The number of inlay hint refreshes sent, which numbers each one.
    refreshes: u64,
}

impl Worker<'_> {
    /// Analyses the newest of `texts`, the texts of one opening of a document
    /// in the order they came in, and if it doesn't parse, the newest of the
    /// others that does.
    fn catch_up(&mut self, mut texts: Vec<Job>) {
        let Some(newest) = texts.pop() else {
            return;
        };
        let open = self
            .documents
            .read()
            .get(&newest.uri)
            .is_some_and(|open| open.opening == newest.opening);
        if !open {
            return;
        }
        // An analysis of a text that doesn't parse stops at the parser, so it
        // costs next to nothing: analyse the last text that parsed first, as
        // that's what a completion right after the dot waits for.
        if !document::parses(&newest.text)
            && let Some(last_good) = texts
                .into_iter()
                .rev()
                .find(|job| document::parses(&job.text))
        {
            self.analyse(last_good, false);
        }
        self.analyse(newest, true);
    }

    /// Analyses `job`'s text and commits what it works out, and publishes its
    /// diagnostics if `publish` and it's the current text.
    fn analyse(&mut self, job: Job, publish: bool) {
        // A bug in the analyser mustn't end this thread, or diagnostics would
        // stop for the rest of the session: skip the text instead.
        let Ok((diagnostics, analysed)) = panic::catch_unwind(AssertUnwindSafe(|| {
            document::analyse(self.configure, &job.text)
        })) else {
            notify::<LogMessage>(
                self.sender,
                LogMessageParams {
                    typ: MessageType::ERROR,
                    message: format!(
                        "the analyser panicked on version {} of {}",
                        job.version,
                        job.uri.as_str()
                    ),
                },
            );
            return;
        };

        let mut docs = self.documents.write();
        let Some(open) = docs.get_mut(&job.uri) else {
            return;
        };
        // The text of an earlier opening of the document is no text of this one.
        if open.opening != job.opening {
            return;
        }
        let current = open.version == job.version;
        // On parse failure, keep the last good AST.
        if let Some(analysed) = analysed {
            open.state.set_analysis_of(job.text, analysed);
        }
        if publish && current {
            if self.refresh_inlay_hints && open.state.analysis_matches_source {
                self.refreshes += 1;
                let id = RequestId::from(format!("inlay-hint-refresh-{}", self.refreshes));
                let request = Request::new(id, InlayHintRefreshRequest::METHOD.to_string(), ());
                // The send fails only once the client has gone.
                let _ = self.sender.send(request.into());
            }
            publish_diagnostics(self.sender, job.uri, diagnostics, Some(job.version));
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
    /// The number of `didOpen`s so far, which numbers each opening.
    openings: Cell<u64>,
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
                // The spec has a server that's told to exit before it's told
                // to shut down exit with an error.
                Message::Notification(notification) if notification.method == Exit::METHOD => {
                    return Err("told to exit without a shutdown request first".into());
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
        let opening = {
            let mut docs = self.documents.write();
            if let Some(open) = docs.get_mut(&uri) {
                // The AST now predates this edit; the analysis commits a new
                // one if the new source parses.
                open.state.set_source(&text);
                open.version = version;
                open.opening
            } else {
                let opening = self.openings.get() + 1;
                self.openings.set(opening);
                let state = DocumentState::from_source(text.clone());
                docs.insert(
                    uri.clone(),
                    OpenDocument {
                        version,
                        opening,
                        state,
                    },
                );
                opening
            }
        };
        // The send fails only once the worker has gone, at shutdown.
        let _ = self.jobs.send(Job {
            uri,
            text,
            version,
            opening,
        });
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
        let state = docs
            .get(uri)
            .map(|open| &open.state)
            .filter(|state| state.analysis_matches_source)?;
        let locations: Vec<Location> = definition::goto_definition(state, position)
            .into_iter()
            .map(|range| Location {
                uri: uri.clone(),
                range,
            })
            .collect();
        match <[Location; 1]>::try_from(locations) {
            Ok([location]) => Some(GotoDefinitionResponse::Scalar(location)),
            Err(locations) if locations.is_empty() => None,
            Err(locations) => Some(GotoDefinitionResponse::Array(locations)),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    /// Runs the analysis thread's loop over the texts `texts` of one document
    /// that came in before it got to any of them, and returns the document.
    fn analyse_in_turn(texts: &[&str]) -> (OpenDocument, Vec<Message>) {
        analyse_with(texts, false, ndc_stdlib::register)
    }

    fn analyse_in_turn_refreshing(
        texts: &[&str],
        refresh_inlay_hints: bool,
    ) -> (OpenDocument, Vec<Message>) {
        analyse_with(texts, refresh_inlay_hints, ndc_stdlib::register)
    }

    fn analyse_with(
        texts: &[&str],
        refresh_inlay_hints: bool,
        configure: Configure,
    ) -> (OpenDocument, Vec<Message>) {
        let uri = Uri::from_str("file:///t.ndc").expect("a valid uri");
        let documents = Documents::default();
        let (jobs, queue) = mpsc::channel();
        for (version, text) in (1..).zip(texts) {
            documents.write().insert(
                uri.clone(),
                OpenDocument {
                    version,
                    opening: 1,
                    state: DocumentState::from_source((*text).to_string()),
                },
            );
            let job = Job {
                uri: uri.clone(),
                text: (*text).to_string(),
                version,
                opening: 1,
            };
            jobs.send(job).expect("the queue is open");
        }
        drop(jobs);
        let (sender, messages) = crossbeam_channel::unbounded();
        analyse_jobs(&queue, &documents, &sender, configure, refresh_inlay_hints);
        let open = documents.write().remove(&uri).expect("the document");
        (open, messages.try_iter().collect())
    }

    #[test]
    fn a_superseded_text_that_parses_still_feeds_dot_completion() {
        let (open, messages) =
            analyse_in_turn(&["let words = [\"a\"];\n", "let words = [\"a\"];\nwords."]);
        // The current text doesn't parse, so the analysis is of the earlier one…
        assert!(!open.state.analysis_matches_source);
        assert!(open.state.variable_types.contains_key("words"));
        // …and only the current text's diagnostics are published.
        assert_eq!(messages.len(), 1);
    }

    #[test]
    fn the_analysis_of_the_current_text_matches_it() {
        let (open, _) = analyse_in_turn(&["let x = 1 +;", "let x = 1;"]);
        assert!(open.state.analysis_matches_source);
    }

    fn refreshes(messages: &[Message]) -> usize {
        messages
            .iter()
            .filter(|message| {
                matches!(message, Message::Request(r) if r.method == InlayHintRefreshRequest::METHOD)
            })
            .count()
    }

    #[test]
    fn the_client_is_told_to_refresh_inlay_hints_once_the_current_text_is_analysed() {
        let (_, messages) = analyse_in_turn_refreshing(&["let x = 1 +;", "let x = 1;"], true);
        assert_eq!(refreshes(&messages), 1);
    }

    #[test]
    fn inlay_hints_are_not_refreshed_while_the_current_text_does_not_parse() {
        let (_, messages) = analyse_in_turn_refreshing(&["let x = 1;", "let x = 1 +;"], true);
        assert_eq!(refreshes(&messages), 0);
    }

    #[test]
    fn inlay_hints_are_not_refreshed_for_a_client_that_cannot() {
        let (_, messages) = analyse_in_turn(&["let x = 1;"]);
        assert_eq!(refreshes(&messages), 0);
    }

    // Each analysis configures one interpreter, so a `Configure` that counts
    // its calls counts the analyses. Each test has its own, as tests run at
    // the same time.
    macro_rules! counting_configure {
        ($name:ident, $count:ident) => {
            static $count: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
            fn $name(registry: &mut ndc_core::FunctionRegistry<ndc_interpreter::NativeSpec>) {
                $count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                ndc_stdlib::register(registry);
            }
        };
    }

    #[test]
    fn a_burst_of_edits_is_analysed_once() {
        counting_configure!(configure, ANALYSES);
        let (open, messages) = analyse_with(
            &["let x = 1;", "let x = 12;", "let x = 123;"],
            false,
            configure,
        );
        assert_eq!(ANALYSES.load(std::sync::atomic::Ordering::Relaxed), 1);
        assert!(open.state.analysis_matches_source);
        assert_eq!(messages.len(), 1);
    }

    #[test]
    fn a_burst_ending_in_a_text_that_does_not_parse_also_analyses_the_newest_that_does() {
        counting_configure!(configure, ANALYSES);
        let texts = [
            "let words = [\"a\"];\n",
            "let words = [\"a\"];\nwords.len();",
            "let words = [\"a\"];\nwords.len();\nwords.",
        ];
        let (open, messages) = analyse_with(&texts, false, configure);
        assert_eq!(ANALYSES.load(std::sync::atomic::Ordering::Relaxed), 2);
        assert!(!open.state.analysis_matches_source);
        assert!(open.state.variable_types.contains_key("words"));
        // Only the current text's diagnostics are published.
        assert_eq!(messages.len(), 1);
    }

    #[test]
    fn a_burst_in_which_nothing_parses_is_analysed_once() {
        counting_configure!(configure, ANALYSES);
        let (open, _) = analyse_with(
            &["let x = 1 +;", "let x = 1 + ;", "let x = 1 +  ;"],
            false,
            configure,
        );
        assert_eq!(ANALYSES.load(std::sync::atomic::Ordering::Relaxed), 1);
        assert!(!open.state.analysis_matches_source);
    }
}
