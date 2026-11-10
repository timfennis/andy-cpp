//! Drives the REPL of the built `ndc` binary, as a user would, to check that a
//! line that fails leaves no trace in the session.
#![cfg(unix)]
#![allow(unused_crate_dependencies)]

use std::io::Write;
use std::process::{Command, Stdio};

/// Feeds `lines` to the REPL and returns what it printed to stdout, without the
/// closing "Bye!". Errors go to stderr, so a failed line prints nothing here.
fn repl(lines: &[&str]) -> Vec<String> {
    let mut child = Command::new(env!("CARGO_BIN_EXE_ndc"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("ndc starts");
    let mut input = lines.join("\n");
    input.push('\n');
    child
        .stdin
        .take()
        .expect("stdin is piped")
        .write_all(input.as_bytes())
        .expect("input is written");
    let output = child.wait_with_output().expect("ndc exits");
    assert!(output.status.success(), "ndc exited with {}", output.status);
    let stdout = String::from_utf8(output.stdout).expect("output is UTF-8");
    let mut printed: Vec<String> = stdout.lines().map(str::to_string).collect();
    assert_eq!(printed.pop().as_deref(), Some("Bye!"));
    printed
}

#[test]
fn lines_that_succeed_keep_their_effects() {
    assert_eq!(repl(&["let x = 1;", "x = 2;", "print(x)"]), ["2"]);
}

#[test]
fn a_failed_line_leaves_variables_as_they_were() {
    let printed = repl(&["let x = 1;", "x = \"abc\"; [1][5]", "print(x)"]);
    assert_eq!(printed, ["1"]);
}

#[test]
fn a_failed_line_leaves_the_heap_as_it_was() {
    let printed = repl(&[
        "let xs = [1];",
        "xs[0] = \"a\"; xs.push(2); [1][5]",
        "print(xs)",
    ]);
    assert_eq!(printed, ["[1]"]);
}

#[test]
fn a_failed_line_declares_nothing() {
    // The failed `let` must not leave `y` behind, so `y` is an unknown name
    // and the last line prints nothing.
    assert!(repl(&["let y = [1][5];", "print(y)"]).is_empty());
}

// A closure on the failed line boxed `x` for itself before the line failed.
#[test]
fn a_failed_line_leaves_captures_as_they_were() {
    let printed = repl(&["let x = 1;", "let f = fn() => x; [1][5]", "print(x + 1)"]);
    assert_eq!(printed, ["2"]);
}

#[test]
fn a_rejected_line_leaves_the_session_as_it_was() {
    assert_eq!(repl(&["let x = 41;", "break;", "print(x + 1)"]), ["42"]);
}

#[test]
fn what_a_failed_line_printed_stays_printed() {
    assert_eq!(repl(&["print(\"before\"); [1][5]"]), ["before"]);
}

#[test]
fn the_session_survives_a_stack_overflow() {
    let deep = format!("{}1{}", "(".repeat(100_000), ")".repeat(100_000));
    assert_eq!(repl(&["let y = 7;", &deep, "print(y)"]), ["7"]);
}
