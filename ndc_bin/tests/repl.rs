//! Drives the REPL of the built `ndc` binary, as a user would, to check that a
//! line that fails leaves no trace in the session.
#![cfg(unix)]
#![allow(unused_crate_dependencies)]

use std::io::{Read, Write};
use std::os::unix::process::ExitStatusExt;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

/// Feeds `lines` to the REPL and returns what it printed to stdout, without the
/// closing "Bye!". Errors go to stderr, so a failed line prints nothing here.
fn repl(lines: &[&str]) -> Vec<String> {
    repl_with_errors(lines).0
}

/// Like [`repl`], and also returns what the REPL printed to stderr.
fn repl_with_errors(lines: &[&str]) -> (Vec<String>, String) {
    let mut child = Command::new(env!("CARGO_BIN_EXE_ndc"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
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
    let errors = String::from_utf8(output.stderr).expect("errors are UTF-8");
    (printed, errors)
}

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

/// Sends `signal` (e.g. `"-TERM"`) to `pid`, or to a process group if `pid`
/// starts with a `-`.
fn send(signal: &str, pid: &str) {
    let sent = Command::new("kill").args([signal, "--", pid]).status();
    assert!(sent.expect("kill runs").success(), "kill {signal} {pid}");
}

/// A running `ndc` that gets its input a piece at a time. Dropping it before it
/// has exited terminates it, so a failing test leaves no REPL running.
struct Ndc(Child);

impl Ndc {
    fn start(stdout: Stdio) -> Self {
        let child = Command::new(env!("CARGO_BIN_EXE_ndc"))
            .stdin(Stdio::piped())
            .stdout(stdout)
            .stderr(Stdio::null())
            .spawn()
            .expect("ndc starts");
        Self(child)
    }

    fn pid(&self) -> String {
        self.0.id().to_string()
    }

    fn write(&mut self, input: &str) {
        let stdin = self.0.stdin.as_mut().expect("stdin is open");
        stdin.write_all(input.as_bytes()).expect("input is written");
    }

    /// Closes stdin and waits up to five seconds for `ndc` to exit.
    fn wait(&mut self) -> ExitStatus {
        drop(self.0.stdin.take());
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            if let Some(status) = self.0.try_wait().expect("ndc can be waited on") {
                return status;
            }
            assert!(Instant::now() < deadline, "ndc didn't stop");
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    /// What `ndc` printed, once it has exited.
    fn stdout(&mut self) -> String {
        let mut stdout = String::new();
        let mut out = self.0.stdout.take().expect("stdout is piped");
        out.read_to_string(&mut stdout).expect("output is UTF-8");
        stdout
    }
}

impl Drop for Ndc {
    fn drop(&mut self) {
        if let Ok(None) = self.0.try_wait() {
            send("-TERM", &self.pid());
            let _ = self.0.wait();
        }
    }
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

// The analyser rejects `break` outside a loop, so the line never runs, but
// the analyser has seen its `let` by then.
#[test]
fn a_line_rejected_before_running_leaves_the_session_as_it_was() {
    let printed = repl(&[
        "let x = 41;",
        "let y = 1; break;",
        "print(x + 1)",
        "print(y)",
    ]);
    assert_eq!(printed, ["42"]);
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
    let mut ndc = Ndc::start(Stdio::null());
    ndc.write("while true { }\n");
    std::thread::sleep(Duration::from_millis(300));

    // The REPL process, which holds the session, and the copy running the line.
    let supervisor = ndc.pid();
    let repl = children(&supervisor);
    assert_eq!(repl.len(), 1, "one REPL process");
    let running = children(&repl[0]);
    assert_eq!(running.len(), 1, "one process running the line");

    send("-TERM", &supervisor);
    let status = ndc.wait();
    assert_eq!(status.signal(), Some(15), "ndc should end by SIGTERM");
    for pid in repl.iter().chain(&running) {
        assert!(!is_running(pid), "REPL process {pid} is still running");
    }
}

// In a terminal, Ctrl-C sends SIGINT to the REPL's process group. That ends
// the line that is running, and the session carries on as it was before it.
#[test]
fn ctrl_c_ends_only_the_running_line() {
    let mut ndc = Ndc::start(Stdio::piped());
    // The first REPL process leads the process group that every later one
    // joins, and that a terminal sends Ctrl-C to.
    let supervisor = ndc.pid();
    let deadline = Instant::now() + Duration::from_secs(5);
    let group = loop {
        if let [repl] = children(&supervisor).as_slice() {
            break repl.clone();
        }
        assert!(Instant::now() < deadline, "the REPL didn't start");
        std::thread::sleep(Duration::from_millis(20));
    };

    ndc.write("let x = 1;\nx = 2; while true { }\n");
    std::thread::sleep(Duration::from_millis(300));

    // The REPL process, which holds the session, has forked the copy running
    // the line. (The group may also hold the first REPL process, exited but
    // not yet reaped, which has no children.)
    let members = Command::new("pgrep").args(["-g", &group]).output();
    let members = String::from_utf8(members.expect("pgrep runs").stdout).expect("PIDs");
    let running: Vec<String> = members.split_whitespace().flat_map(children).collect();
    assert_eq!(running.len(), 1, "one process running the line");

    send("-INT", &format!("-{group}"));
    ndc.write("print(x)\n");
    let status = ndc.wait();
    assert!(status.success(), "ndc exited with {status}");
    assert_eq!(ndc.stdout().lines().collect::<Vec<_>>(), ["1", "Bye!"]);
}

// REPL lines declare into one scope, so a later line can add an overload to a
// function from an earlier line, but not one with the same parameter types.
#[test]
fn later_lines_overload_functions_of_earlier_lines() {
    let (printed, errors) = repl_with_errors(&[
        "fn g(a: Int) { \"int\" }",
        "fn g(a: String) { \"string\" }",
        "print(g(1), g(\"a\"))",
        "fn g(b: Int) { \"again\" }",
        "print(g(1))",
    ]);
    assert_eq!(printed, ["int string", "int"]);
    assert!(
        errors.contains("Illegal redefinition of function 'g' with parameters (Int)"),
        "unexpected errors: {errors}"
    );
}

// A top-level `let` declares a new global even when the name exists, and a
// function keeps reading the variable it saw when it was declared.
#[test]
fn a_let_on_a_later_line_is_a_new_variable_earlier_functions_dont_see() {
    let printed = repl(&["let x = 1;", "fn f() { x }", "let x = 2;", "print(f(), x)"]);
    assert_eq!(printed, ["1 2"]);
}
