//! Property-based panic test for the Andy C++ pipeline.
//!
//! Generates random `Vec<TokenLocation>` and pushes each through
//! parser → analyser → compiler → VM. Errors are expected and ignored;
//! only panics, `unwrap`s, and `unreachable!`s count as failures.
//!
//! Each generated program runs in a worker thread with a 500ms timeout,
//! so generated infinite loops don't hang the test. Panics are caught
//! and re-raised on the test thread so proptest can shrink them.
//!
//! Two test functions share the same `panic.regressions` file:
//!
//! * `known_regressions_do_not_panic` runs by default — it generates 0
//!   fresh cases and only replays seeds saved in `panic.regressions`, so
//!   any future commit that resurrects a known panic fails immediately.
//! * `fuzz_random_tokens` is `#[ignore]`d because each run typically turns
//!   up new panics that haven't been triaged yet. Opt in with:
//!
//!   ```sh
//!   cargo test -p proptest_tests -- --ignored             # fuzz only
//!   cargo test -p proptest_tests -- --include-ignored     # regressions + fuzz
//!   PROPTEST_CASES=100000 cargo test -p proptest_tests -- --ignored
//!   ```
//!
//! When the fuzz run finds a new panic, proptest shrinks it and appends
//! the seed to `panic.regressions`; the next default `cargo test` run
//! will guard against the regression automatically.

use ndc_analyser::{Analyser, ScopeTree};
use ndc_lexer::{Span, Token, TokenLocation};
use ndc_parser::Parser;
use ndc_vm::compiler::Compiler;
use ndc_vm::{OutputSink, Vm};
use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use std::fmt;
use std::panic::{self, AssertUnwindSafe};
use std::sync::Once;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

const PER_CASE_TIMEOUT: Duration = Duration::from_millis(500);
const MAX_PROGRAM_LEN: usize = 30;

fn loc(token: Token) -> TokenLocation {
    TokenLocation {
        token,
        span: Span::synthetic(),
    }
}

/// Wrapper around the generated token stream with a compact `Debug` impl,
/// so a failure prints `[3] a not +` instead of three nested `TokenLocation`
/// blocks repeating `SourceId(4294967295), offset: 0, length: 0`.
#[derive(Clone)]
struct Program(Vec<TokenLocation>);

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "[0] (empty)");
        }
        write!(f, "[{}]", self.0.len())?;
        for loc in &self.0 {
            write!(f, " {:?}", loc.token)?;
        }
        Ok(())
    }
}

const WORKER_THREAD_NAME: &str = "proptest-fuzz-worker";

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

/// All unit-variant tokens, identifiers/strings/numbers from small pools,
/// and a couple of `BigInt`/`Complex` literals. Combined uniformly with a
/// low-weight `OpAssign` wrapping an augmentable inner token.
fn arb_token() -> impl Strategy<Value = Token> {
    let mut atoms: Vec<Token> = vec![
        // Literals (unit)
        Token::Infinity,
        Token::True,
        Token::False,
        // Operators
        Token::EqualsSign,
        Token::EqualsEquals,
        Token::BangEquals,
        Token::Greater,
        Token::GreaterEquals,
        Token::Less,
        Token::LessEquals,
        Token::Spaceship,
        Token::InverseSpaceship,
        Token::Plus,
        Token::Minus,
        Token::Asterix,
        Token::ForwardSlash,
        Token::Backslash,
        Token::Percent,
        Token::PercentPercent,
        Token::Caret,
        Token::Ampersand,
        Token::Pipe,
        Token::Tilde,
        Token::LessLess,
        Token::GreaterGreater,
        Token::Bang,
        Token::Dot,
        Token::DotDot,
        Token::DotDotEquals,
        Token::PlusPlus,
        Token::Diamond,
        Token::RightArrow,
        Token::FatArrow,
        // Logic
        Token::LogicAnd,
        Token::LogicOr,
        Token::LogicNot,
        // Keywords
        Token::Let,
        Token::Fn,
        Token::If,
        Token::Else,
        Token::Return,
        Token::Break,
        Token::Continue,
        Token::For,
        Token::In,
        Token::While,
        Token::Pure,
        // Structural
        Token::LeftParentheses,
        Token::RightParentheses,
        Token::LeftSquareBracket,
        Token::RightSquareBracket,
        Token::LeftCurlyBracket,
        Token::RightCurlyBracket,
        Token::Semicolon,
        Token::Comma,
        Token::Colon,
        Token::MapOpen,
    ];
    for s in ["a", "b", "c", "x", "f", "g"] {
        atoms.push(Token::Identifier(s.to_string()));
    }
    for s in ["", "x", "ab"] {
        atoms.push(Token::String(s.to_string()));
    }
    for i in [-2_i64, -1, 0, 1, 2, 42, i64::MAX] {
        atoms.push(Token::Int64(i));
    }
    for f in [0.0_f64, 1.0, -1.0, 0.5, f64::INFINITY, f64::NAN] {
        atoms.push(Token::Float64(f));
    }
    atoms.push(Token::BigInt(num::BigInt::from(0)));
    atoms.push(Token::BigInt(num::BigInt::from(i128::MAX)));
    atoms.push(Token::Complex(num::complex::Complex64::new(0.0, 1.0)));
    atoms.push(Token::Complex(num::complex::Complex64::new(2.0, -3.0)));

    // Inner token of an `OpAssign`. Lexer invariant: only augmentable tokens
    // appear here, so we mirror that to avoid finding "panics" that no real
    // source could produce.
    let mut augmentable: Vec<Token> = vec![
        Token::Plus,
        Token::Minus,
        Token::Asterix,
        Token::ForwardSlash,
        Token::Backslash,
        Token::Percent,
        Token::PercentPercent,
        Token::Caret,
        Token::Ampersand,
        Token::Pipe,
        Token::Tilde,
        Token::PlusPlus,
        Token::Diamond,
        Token::LessLess,
        Token::GreaterGreater,
    ];
    for s in ["a", "b", "f"] {
        augmentable.push(Token::Identifier(s.to_string()));
    }

    prop_oneof![
        9 => prop::sample::select(atoms),
        1 => prop::sample::select(augmentable).prop_map(|t| Token::OpAssign(Box::new(loc(t)))),
    ]
}

fn arb_program() -> impl Strategy<Value = Program> {
    prop::collection::vec(arb_token().prop_map(loc), 0..=MAX_PROGRAM_LEN).prop_map(Program)
}

/// Drive parser → analyser → compiler → VM. Returns `()` on any error
/// because the test only cares about panics, not error variants. The
/// analyser runs against an empty global scope (no stdlib registered);
/// undefined-identifier failures are expected and silently ignored.
fn run_pipeline(tokens: Vec<TokenLocation>) {
    let mut expressions = match Parser::from_tokens(tokens).parse() {
        Ok(e) => e,
        Err(_) => return,
    };

    let mut analyser = Analyser::from_scope_tree(ScopeTree::from_global_scope(vec![]));
    for e in &mut expressions {
        if analyser.analyse(e).is_err() {
            return;
        }
    }
    if analyser.has_errors() {
        return;
    }

    let compiled = match Compiler::compile(expressions.into_iter()) {
        Ok(c) => c,
        Err(_) => return,
    };

    let mut vm = Vm::new(compiled, Vec::new()).with_output(OutputSink::Buffer(Vec::new()));
    let _ = vm.run();
}

/// Run the pipeline in a worker thread with a hard timeout. Hangs are
/// silently dropped (the worker leaks; the OS reaps it on process exit).
/// Genuine panics propagate to the proptest harness so it can shrink.
fn run_with_timeout(program: Program) {
    install_quiet_panic_hook();
    let (tx, rx) = mpsc::channel();
    thread::Builder::new()
        .name(WORKER_THREAD_NAME.into())
        .spawn(move || {
            let result = panic::catch_unwind(AssertUnwindSafe(|| run_pipeline(program.0)));
            let _ = tx.send(result);
        })
        .expect("spawn worker thread");

    if let Ok(Err(payload)) = rx.recv_timeout(PER_CASE_TIMEOUT) {
        panic::resume_unwind(payload);
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
    // `panic.regressions` before checking the case count, so this acts as
    // a regression test for known panics.
    #![proptest_config(persistence_config(0, 0))]

    #[test]
    fn known_regressions_do_not_panic(program in arb_program()) {
        run_with_timeout(program);
    }
}

proptest! {
    #![proptest_config(persistence_config(1024, 4096))]

    #[test]
    #[ignore = "fuzz test; opt-in via `cargo test ... -- --ignored`"]
    fn fuzz_random_tokens(program in arb_program()) {
        run_with_timeout(program);
    }
}
