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
fn a_struct_declared_on_a_failed_line_is_gone() {
    let printed = repl(&[
        "struct Foo { x: Int }; [1][5]",
        "struct Foo { y: Int }",
        "print(Foo(42).y)",
    ]);
    assert_eq!(printed, ["42"]);
}

#[test]
fn a_line_rejected_by_analysis_leaves_the_session_as_it_was() {
    let printed = repl(&["let x = 42;", "let y: Unknwn = 1;", "print(x)"]);
    assert_eq!(printed, ["42"]);
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

// Stopping `ndc` by its PID, as `timeout` or `Popen.terminate()` do, must stop
// the REPL processes too, even in the middle of an endless line.
#[test]
fn terminating_ndc_stops_the_repl() {
    use std::os::unix::process::ExitStatusExt;
    use std::time::{Duration, Instant};

    /// The PIDs of the processes whose parent is `pid`.
    fn children(pid: &str) -> Vec<String> {
        let output = Command::new("pgrep").args(["-P", pid]).output();
        let output = output.expect("pgrep runs");
        String::from_utf8(output.stdout)
            .expect("pgrep prints PIDs")
            .split_whitespace()
            .map(str::to_string)
            .collect()
    }
    fn is_running(pid: &str) -> bool {
        let status = Command::new("kill")
            .args(["-0", pid])
            .stderr(Stdio::null())
            .status();
        status.expect("kill runs").success()
    }

    let mut child = Command::new(env!("CARGO_BIN_EXE_ndc"))
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("ndc starts");
    let mut stdin = child.stdin.take().expect("stdin is piped");
    stdin
        .write_all(b"while true { }\n")
        .expect("input is written");
    std::thread::sleep(Duration::from_millis(300));

    // The REPL process, which holds the session, and the copy running the line.
    let supervisor = child.id().to_string();
    let repl = children(&supervisor);
    assert_eq!(repl.len(), 1, "one REPL process");
    let running = children(&repl[0]);
    assert_eq!(running.len(), 1, "one process running the line");

    let killed = Command::new("kill").args(["-TERM", &supervisor]).status();
    assert!(killed.expect("kill runs").success());

    let deadline = Instant::now() + Duration::from_secs(5);
    let status = loop {
        if let Some(status) = child.try_wait().expect("ndc can be waited on") {
            break status;
        }
        assert!(Instant::now() < deadline, "ndc didn't stop");
        std::thread::sleep(Duration::from_millis(20));
    };
    assert_eq!(status.signal(), Some(15), "ndc should end by SIGTERM");
    for pid in repl.iter().chain(&running) {
        assert!(!is_running(pid), "REPL process {pid} is still running");
    }
}
