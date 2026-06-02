//! Regression test: deeply nested input must not crash the interpreter.
//!
//! Each nesting level costs a native stack frame in the parser and in every
//! later phase that walks the AST. On the default main-thread stack this used
//! to overflow and abort the process (SIGABRT) at a few hundred levels. `ndc`
//! now runs the interpreter on a worker thread with a large explicit stack, so
//! input far deeper than that evaluates normally. See
//! <https://github.com/timfennis/andy-cpp/issues/175>.

// This integration test links `ndc_bin`'s dependencies but drives the built
// binary instead of calling them, so silence `unused-crate-dependencies`.
#![allow(unused_crate_dependencies)]

use std::fs;
use std::process::Command;

#[test]
fn deeply_nested_input_does_not_overflow_the_stack() {
    // Deep enough to overflow the default main-thread stack (which gave up
    // around a few hundred levels), shallow enough to fit the worker thread's
    // large stack with room to spare.
    let depth = 2000;
    let source = format!("print({}1{})", "(".repeat(depth), ")".repeat(depth));

    let path = std::env::temp_dir().join(format!("ndc_deep_nesting_{}.ndc", std::process::id()));
    fs::write(&path, &source).expect("write temp script");

    let output = Command::new(env!("CARGO_BIN_EXE_ndc"))
        .arg(&path)
        .output()
        .expect("run ndc");

    let _ = fs::remove_file(&path);

    assert!(
        output.status.success(),
        "ndc crashed on {depth}-deep nesting (status {:?}); a stack overflow would show up here.\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "1",
        "the nested expression should evaluate to 1",
    );
}
