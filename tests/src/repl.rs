use ndc_interpreter::Interpreter;

/// Run a sequence of REPL lines against a shared interpreter and return the
/// collected stdout output after all lines have been executed.
fn repl_output(lines: &[&str]) -> String {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    for line in lines {
        interp.eval(line).expect("line should not error");
    }
    String::from_utf8(interp.get_output().expect("interpreter must have output"))
        .expect("output must be valid UTF-8")
}

/// Run a sequence of REPL lines and expect the last one to produce an error
/// containing the given substring.
fn repl_error(lines: &[&str], expected_error: &str) {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    let n = lines.len();
    for line in &lines[..n - 1] {
        interp.eval(line).expect("line should not error");
    }
    let err = interp.eval(lines[n - 1]).expect_err("expected an error");
    let msg = format!("{err:?}");
    assert!(
        msg.contains(expected_error),
        "expected error containing {:?}, got {:?}",
        expected_error,
        msg,
    );
}

#[test]
fn variable_persists_across_lines() {
    let out = repl_output(&["let x = 10;", "print(x)"]);
    assert_eq!(out.trim(), "10");
}

#[test]
fn arithmetic_on_previous_variable() {
    let out = repl_output(&["let x = 10;", "print(x + 3)"]);
    assert_eq!(out.trim(), "13");
}

#[test]
fn reassignment_is_visible_on_next_line() {
    let out = repl_output(&["let x = 1;", "x = 42;", "print(x)"]);
    assert_eq!(out.trim(), "42");
}

#[test]
fn multiple_variables_persist() {
    let out = repl_output(&["let a = 3;", "let b = 4;", "print(a * b)"]);
    assert_eq!(out.trim(), "12");
}

#[test]
fn function_defined_on_earlier_line_is_callable() {
    let out = repl_output(&["fn double(x) => x * 2", "print(double(7))"]);
    assert_eq!(out.trim(), "14");
}

#[test]
fn print_on_first_line_is_not_repeated() {
    // Ensure the persistent-VM approach does not re-execute old lines.
    let out = repl_output(&["print(1);", "print(2)"]);
    assert_eq!(out.trim(), "1\n2");
}

#[test]
fn error_on_undefined_variable() {
    repl_error(
        &["let x = 10;", "y + 1"],
        "has not previously been declared",
    );
}

#[test]
fn expression_result_is_returned() {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    let result = interp.eval("2 + 3").unwrap();
    assert_eq!(result.to_string(), "5");
}

#[test]
fn statement_returns_unit() {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    let result = interp.eval("2 + 3;").unwrap();
    assert!(result.is_unit());
}

#[test]
fn let_returns_unit() {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    let result = interp.eval("let x = 5;").unwrap();
    assert!(result.is_unit());
}

#[test]
fn error_does_not_corrupt_state() {
    // After a failed line the previously-defined variable should still work.
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    interp.eval("let x = 99;").unwrap();
    let _ = interp.eval("undefined_var"); // this should error, ignore it
    interp.eval("print(x)").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "99");
}
