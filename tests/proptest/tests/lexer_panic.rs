//! Property-based panic test for the Andy C++ lexer.
//!
//! Generates random `String` inputs and pushes them through `Lexer::new(...)`,
//! draining the iterator. Errors are expected and ignored; only panics,
//! `unwrap`s, `expect`s, and `unreachable!`s count as failures.
//!
//! Each generated input runs in a worker thread with a 500ms timeout. The
//! lexer always advances by at least one character per `next()` call, so a
//! genuine hang would indicate a regression — the timeout exists as a
//! safety net rather than because we expect to hit it.
//!
//! Two test functions share `lexer_panic.regressions`:
//!
//! * `known_regressions_do_not_panic` runs by default — it generates 0 fresh
//!   cases and only replays seeds saved in the regressions file, guarding
//!   against future commits that resurrect a known panic.
//! * `fuzz_random_strings` is `#[ignore]`d. Opt in with:
//!
//!   ```sh
//!   cargo test -p proptest_tests -- --ignored             # fuzz only
//!   cargo test -p proptest_tests -- --include-ignored     # regressions + fuzz
//!   PROPTEST_CASES=100000 cargo test -p proptest_tests -- --ignored
//!   ```

use ndc_analyser as _;
use ndc_core as _;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser as _;
use ndc_stdlib as _;
use ndc_vm as _;
use num as _;
use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use std::fmt;
use std::panic::{self, AssertUnwindSafe};
use std::sync::Once;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

const PER_CASE_TIMEOUT: Duration = Duration::from_millis(500);
const MAX_INPUT_LEN: usize = 200;

/// Wrapper around the generated input with a compact `Debug` impl that
/// shows byte length plus the escaped input, so a failure prints
/// `[12B] "0r\"\\#"` instead of a wall of escapes.
#[derive(Clone)]
struct Source(String);

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}B] {:?}", self.0.len(), self.0)
    }
}

const WORKER_THREAD_NAME: &str = "proptest-lexer-fuzz-worker";

/// Replace the default panic hook with one that stays silent on the named
/// worker thread but delegates to the original hook everywhere else.
/// Without this, every shrinking attempt floods stderr with the same
/// panic message; with it, only the final shrunk failure (resumed on the
/// test thread) is printed.
fn install_quiet_panic_hook() {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if thread::current().name() != Some(WORKER_THREAD_NAME) {
                default_hook(info);
            }
        }));
    });
}

/// Single character drawn from a weighted union biased toward bytes that
/// exercise lexer branches:
///   - delimiters and escape: `"`, `\`, `#`
///   - raw-string starter and number suffixes: `r`, `i`, `j`
///   - number prefixes/separators: digits, `0`, `b`, `x`, `o`, `_`, `.`
///   - identifier and operator chars
///   - comment starts: `/`, `#`, `!`
///   - whitespace incl. `\r` and `\n`
///
/// The other arms add general ASCII printable and full-range unicode so
/// multi-byte offset arithmetic gets exercised too.
fn arb_char() -> impl Strategy<Value = char> {
    let high_signal: Vec<char> = vec![
        '"', '\\', '#', 'r', '.', '_', '?', '+', '-', '*', '/', '%', '=', '<', '>', '!', '&', '|',
        '^', '~', '(', ')', '[', ']', '{', '}', ',', ';', ':', ' ', '\t', '\n', '\r', 'a', 'b',
        'c', 'f', 'g', 'i', 'j', 'n', 'o', 'x', 'z', '0', '1', '2', '7', '9',
    ];

    prop_oneof![
        20 => prop::sample::select(high_signal),
        // Any ASCII printable, to widen coverage of the single-char token try-from path.
        4 => (32u32..=126u32).prop_filter_map("ascii printable", char::from_u32),
        // Full unicode range minus surrogates and out-of-range codepoints.
        1 => (0u32..=0x10_FFFF).prop_filter_map("valid scalar value", char::from_u32),
    ]
}

fn arb_source() -> impl Strategy<Value = Source> {
    prop::collection::vec(arb_char(), 0..=MAX_INPUT_LEN)
        .prop_map(|chars| Source(chars.into_iter().collect()))
}

/// Drain the lexer over `input`. Returns `()` on any error because the
/// test only cares about panics, not error variants.
fn run_lexer(input: &str) {
    for token in Lexer::new(input, SourceId::new(0)) {
        let _ = token;
    }
}

/// Run the lexer in a worker thread with a hard timeout. Both genuine
/// panics and timeouts propagate to the proptest harness so it can shrink
/// them.
fn run_with_timeout(source: Source) {
    install_quiet_panic_hook();
    let (tx, rx) = mpsc::channel();
    thread::Builder::new()
        .name(WORKER_THREAD_NAME.into())
        .spawn(move || {
            let result = panic::catch_unwind(AssertUnwindSafe(|| run_lexer(&source.0)));
            let _ = tx.send(result);
        })
        .expect("spawn worker thread");

    match rx.recv_timeout(PER_CASE_TIMEOUT) {
        Err(mpsc::RecvTimeoutError::Timeout) => panic!(
            "lexer did not terminate within {} ms",
            PER_CASE_TIMEOUT.as_millis()
        ),
        Ok(Err(payload)) => panic::resume_unwind(payload),
        Ok(Ok(())) | Err(mpsc::RecvTimeoutError::Disconnected) => {}
    }
}

fn persistence_config(cases: u32, max_shrink_iters: u32) -> ProptestConfig {
    ProptestConfig {
        cases,
        max_shrink_iters,
        failure_persistence: Some(Box::new(FileFailurePersistence::WithSource("regressions"))),
        ..ProptestConfig::default()
    }
}

proptest! {
    // cases=0 skips fresh generation; proptest still replays every seed in
    // `lexer_panic.regressions` before checking the case count, so this
    // acts as a regression test for known panics.
    #![proptest_config(persistence_config(0, 0))]

    #[test]
    fn known_regressions_do_not_panic(source in arb_source()) {
        run_with_timeout(source);
    }
}

proptest! {
    #![proptest_config(persistence_config(1024, 4096))]

    #[test]
    #[ignore = "fuzz test; opt-in via `cargo test ... -- --ignored`"]
    fn fuzz_random_strings(source in arb_source()) {
        run_with_timeout(source);
    }
}
