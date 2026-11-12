//! Runs programs given on the command line with `ndc run --eval`.
#![allow(unused_crate_dependencies)]

use std::process::{Command, Output};

fn ndc(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_ndc"))
        .args(args)
        .output()
        .expect("ndc runs")
}

#[test]
fn eval_runs_the_given_source() {
    for flag in ["--eval", "-e"] {
        let output = ndc(&["run", flag, "let x = 20;\nprint(x * 2 + 2);"]);
        assert!(output.status.success(), "ndc exited with {}", output.status);
        assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
    }
}

#[test]
fn eval_errors_point_into_the_given_source() {
    let output = ndc(&["run", "--eval", "let x = 1;\nprint(x + nope);"]);
    assert_eq!(output.status.code(), Some(1));
    let errors = String::from_utf8_lossy(&output.stderr);
    assert!(errors.contains("<eval>:2:"), "{errors}");
}
