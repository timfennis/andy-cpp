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
fn variables_persist_across_lines() {
    let out = repl_output(&["let a = 3;", "let b = 4;", "print(a * b)"]);
    assert_eq!(out.trim(), "12");
}

#[test]
fn reassignment_is_visible_on_next_line() {
    let out = repl_output(&["let x = 1;", "x = 42;", "print(x)"]);
    assert_eq!(out.trim(), "42");
}

#[test]
fn function_defined_on_earlier_line_is_callable() {
    let out = repl_output(&["fn double(x) => x * 2", "print(double(7))"]);
    assert_eq!(out.trim(), "14");
}

#[test]
fn earlier_lines_are_not_run_again() {
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

// The REPL runs each line in a forked process, so a failed line costs it
// nothing; `ndc_bin/tests/repl.rs` tests that. Evaluated in one process, a
// failed line may have declared or changed some things and not others, so a
// resumable interpreter starts over after one.
#[test]
fn a_failed_evaluation_starts_the_session_over() {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    interp.eval("let x = 1;").unwrap();
    assert!(interp.eval("struct Foo { y: Int }\nx = 2; [1][5]").is_err());
    let err = interp.eval("x").expect_err("x should be forgotten");
    assert!(
        format!("{err:?}").contains("has not previously been declared"),
        "unexpected error: {err:?}"
    );
    let err = interp
        .eval("let f: Foo = 1;")
        .expect_err("Foo should be forgotten");
    assert!(
        format!("{err:?}").contains("unknown type `Foo`"),
        "unexpected error: {err:?}"
    );
    // The names are free again.
    interp.eval("struct Foo { y: Int }").unwrap();
    interp.eval("let x = 3; print(Foo(x).y)").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "3");
}

// Code that doesn't parse is never analysed or run, so the session stays.
#[test]
fn a_syntax_error_keeps_the_session() {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    interp.eval("let x = 1;").unwrap();
    assert!(interp.eval("let = ;").is_err());
    interp.eval("print(x)").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "1");
}

#[test]
fn output_of_a_failed_evaluation_is_kept() {
    let mut interp = {
        let mut i = Interpreter::capturing();
        i.configure(ndc_stdlib::register);
        i
    };
    assert!(interp.eval("print(\"before\"); [1][5]").is_err());
    interp.eval("print(\"after\")").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "before\nafter");
}

#[test]
fn struct_usable_as_type_on_later_line() {
    let out = repl_output(&[
        "struct Point { x: Int, y: Int }",
        "fn dist(p: Point) => p.x + p.y",
        "print(dist(Point(3, 4)))",
    ]);
    assert_eq!(out.trim(), "7");
}

#[test]
fn struct_redefinition_on_later_line_errors() {
    repl_error(
        &["struct Point { x: Int }", "struct Point { y: Int }"],
        "Illegal redefinition of struct 'Point'",
    );
}

// A function from one line and the top level of later lines read and write
// the same global.
#[test]
fn functions_share_globals_with_later_lines() {
    let out = repl_output(&[
        "let count = 0;",
        "let inc = fn() { count += 1; };",
        "inc(); inc();",
        "print(count);",
        "count = 10;",
        "inc();",
        "print(count)",
    ]);
    assert_eq!(out.trim(), "2\n11");
}

// `j` is a local of the line's own top-level code, which is gone once the
// line has run.
#[test]
fn closures_keep_the_loop_variables_of_a_finished_line() {
    let out = repl_output(&[
        "let fs: List<Fn() -> Int> = []; let i = 0;",
        "while i < 3 { let j = i; fs.push(fn() { j }); i += 1; }",
        "print([f() for f in fs])",
    ]);
    assert_eq!(out.trim(), "[0,1,2]");
}

// Registering functions after a line already ran must reach the REPL's VM too.
#[test]
fn configure_after_a_line_ran_updates_the_globals() {
    let mut interp = Interpreter::capturing();
    interp.eval("1;").unwrap();
    interp.configure(ndc_stdlib::register);
    interp.eval("print(1)").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "1");
}

#[test]
fn a_generic_function_from_an_earlier_line_takes_a_call_with_new_types() {
    // The line that declared `inc` compiled its instance for Ints. A later
    // line calls it with a Float, which compiles the instance for Floats.
    let out = repl_output(&[
        "fn inc(x) => x + 1;",
        "print(inc(1));",
        "print(inc(2.5), inc(1));",
    ]);
    assert_eq!(out.trim(), "2\n3.5 2");
}

#[test]
fn a_generic_function_on_one_line_uses_its_instances() {
    let out = repl_output(&["fn inc(x) => x + 1; print(inc(1), inc(2.5));"]);
    assert_eq!(out.trim(), "2 3.5");
}

#[test]
fn a_later_line_gets_the_instance_for_any_of_an_earlier_lines_function() {
    let out = repl_output(&[
        r#"fn pick(x: Int) => 1; fn pick(x: Any) => "fallback"; fn wrap(x) => pick(x);"#,
        "let a: Any = 1; let s: String = wrap(a); print(s);",
    ]);
    assert_eq!(out.trim(), "fallback");
}

#[test]
fn a_later_line_gets_an_instance_whose_lambda_takes_its_types() {
    let out = repl_output(&[
        "fn is_safe(ns) { ns.all(fn(x) { x > 0 }) }",
        "print(is_safe([1, 2]))",
    ]);
    assert_eq!(out.trim(), "true");
}

#[test]
fn an_earlier_lines_function_used_as_a_value_is_the_instance_for_its_type() {
    let out = repl_output(&[
        "fn is_safe(ns) { ns.all(fn(x) { x > 0 }) }",
        "print([[1, 2]].count(is_safe))",
    ]);
    assert_eq!(out.trim(), "1");
}

#[test]
fn a_later_line_reuses_the_instance_an_earlier_line_made() {
    let out = repl_output(&[
        "fn inc(x) => x + 1;",
        "print(inc(2.5));",
        "print(inc(3.5), [1.5].map(inc));",
    ]);
    assert_eq!(out.trim(), "3.5\n4.5 [2.5]");
}

#[test]
fn an_instance_of_an_earlier_line_calls_the_instances_its_types_need() {
    let out = repl_output(&[
        "fn inc(x) => x + 1;",
        "fn twice(x) => inc(inc(x));",
        "print(twice(1), twice(0.5));",
    ]);
    assert_eq!(out.trim(), "3 2.5");
}

#[test]
fn a_function_value_in_an_earlier_lines_generic_body_takes_the_instances_overload() {
    let out = repl_output(&[
        r#"fn choose(x: Int) => "int"; fn choose(x: Any) => "any";"#,
        "fn apply(x) { let g = choose; g(x) }",
        "print(apply(1))",
    ]);
    assert_eq!(out.trim(), "int");
}

#[test]
fn an_earlier_lines_generic_function_used_as_a_value_is_an_instance() {
    let out = repl_output(&[
        "fn signs(xs) => xs.map(signum);",
        "print([[1, -2]].map(signs))",
    ]);
    assert_eq!(out.trim(), "[[1,-1]]");
}

#[test]
fn a_function_value_from_an_earlier_lines_generic_body_cast_from_any_is_checked() {
    repl_error(
        &[
            "fn bad(x) { let g = abs; g(x); let a: Any = g; let b = a as Fn(Int) -> String; b(1) }",
            "print(bad(1))",
        ],
        "cannot cast Fn(Int) -> Int to Fn(Int) -> String",
    );
}

#[test]
fn a_failed_line_takes_the_instances_it_made_with_it() {
    let mut interp = Interpreter::capturing();
    interp.configure(ndc_stdlib::register);
    interp.eval("fn inc(x) => x + 1;").unwrap();
    assert!(interp.eval("print(inc(2.5)); [1][5]").is_err());
    interp.eval("fn inc(x) => x + 1;").unwrap();
    interp.eval("print(inc(2.5))").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "3.5\n3.5");
}

#[test]
fn an_instance_of_an_earlier_line_calls_the_instance_its_types_need_of_another() {
    let out = repl_output(&[
        r#"fn pick(x: Int) => 1; fn pick(x: Any) => "fallback"; fn wrap(x) => pick(x);"#,
        "fn outer(x) => wrap(x);",
        "let a: Any = 1; let s: String = outer(a); print(s);",
    ]);
    assert_eq!(out.trim(), "fallback");
}
