use ndc_interpreter::Interpreter;

/// An interpreter that runs lines as the REPL does.
fn repl_interpreter() -> Interpreter {
    let mut interpreter = Interpreter::capturing().line_by_line();
    interpreter.configure(ndc_stdlib::register);
    interpreter
}

/// Run a sequence of REPL lines against a shared interpreter and return the
/// collected stdout output after all lines have been executed.
fn repl_output(lines: &[&str]) -> String {
    let mut interp = repl_interpreter();
    for line in lines {
        interp.eval(line).expect("line should not error");
    }
    String::from_utf8(interp.get_output().expect("interpreter must have output"))
        .expect("output must be valid UTF-8")
}

/// Run a sequence of REPL lines and expect the last one to produce an error
/// containing the given substring.
fn repl_error(lines: &[&str], expected_error: &str) {
    let mut interp = repl_interpreter();
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
    let mut interp = repl_interpreter();
    let result = interp.eval("2 + 3").unwrap();
    assert_eq!(result.to_string(), "5");
}

#[test]
fn statement_returns_unit() {
    let mut interp = repl_interpreter();
    let result = interp.eval("2 + 3;").unwrap();
    assert!(result.is_unit());
}

#[test]
fn let_returns_unit() {
    let mut interp = repl_interpreter();
    let result = interp.eval("let x = 5;").unwrap();
    assert!(result.is_unit());
}

// The REPL runs each line in a forked process, so a failed line costs it
// nothing; `ndc_bin/tests/repl.rs` tests that. Evaluated in one process, a
// failed line may have declared or changed some things and not others, so a
// resumable interpreter starts over after one.
#[test]
fn a_failed_evaluation_starts_the_session_over() {
    let mut interp = repl_interpreter();
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
    let mut interp = repl_interpreter();
    interp.eval("let x = 1;").unwrap();
    assert!(interp.eval("let = ;").is_err());
    interp.eval("print(x)").unwrap();
    let out = String::from_utf8(interp.get_output().unwrap()).unwrap();
    assert_eq!(out.trim(), "1");
}

#[test]
fn output_of_a_failed_evaluation_is_kept() {
    let mut interp = repl_interpreter();
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

// A `pure fn` from one line keeps the results of its calls for later lines,
// one cache for each instance.
#[test]
fn a_pure_function_keeps_its_results_for_later_lines() {
    let out = repl_output(&[
        "fn inc(x) { x + 1 }",
        "let total = inc(1);",
        "print(inc(2.5), total)",
        "pure fn twice(x) { print(\"once\"); x ++ x }",
        "print(twice(\"a\") ++ twice(\"a\"))",
        "twice([1])",
        "let xs = [];",
        "xs.push(inc(total));",
        "print(xs)",
    ]);
    assert_eq!(out, "3.5 2\nonce\naaaa\nonce\n[3]\n");
}

// `j` is a local of the line's own top-level code, which is gone once the
// line has run.
#[test]
fn closures_keep_the_loop_variables_of_a_finished_line() {
    let out = repl_output(&[
        "let fs: [fn() -> Int] = []; let i = 0;",
        "while i < 3 { let j = i; fs.push(fn() { j }); i += 1; }",
        "print([f() for f in fs])",
    ]);
    assert_eq!(out.trim(), "[0,1,2]");
}

// Natives registered after a line already ran are in the globals of the lines
// after it too.
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
            "fn bad(x) { let g = abs; g(x); let a: Any = g; let b = cast::<fn(Int) -> String>(a); b(1) }",
            "print(bad(1))",
        ],
        "cannot cast fn(Int) -> Int to fn(Int) -> String",
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

#[test]
fn a_later_line_settles_what_an_empty_list_holds() {
    let out = repl_output(&["let xs = [];", "xs.push(1);", "print(xs);"]);
    assert_eq!(out.trim(), "[1]");
}

#[test]
fn a_later_line_settles_what_an_empty_map_holds() {
    let out = repl_output(&["let m = %{};", r#"m["a"] = 1;"#, "print(m);"]);
    assert_eq!(out.trim(), r#"%{"a": 1}"#);
}

#[test]
fn a_later_line_settles_what_none_will_hold() {
    let out = repl_output(&["let best = None;", "best = Some(3);", "print(best);"]);
    assert_eq!(out.trim(), "Some(3)");
}

#[test]
fn a_lambdas_type_is_settled_on_its_line() {
    // The function is made with its type, which a cast checks against as
    // the program runs, so a later line can't change it.
    repl_error(
        &["let identity = fn(x) => x;", "identity(1);"],
        "doesn't accept the arguments 'Int'",
    );
}

#[test]
fn a_pure_lambdas_parameter_is_settled_on_its_line() {
    repl_error(
        &["let same = pure fn(x) => x;", "same(fn(n: Int) => n);"],
        "doesn't accept the arguments",
    );
}

#[test]
fn what_a_functions_type_mentions_is_settled_on_its_line() {
    repl_error(
        &["let xs = [];", "let get = fn() => xs;", "xs.push(1);"],
        "[Unknown], Int",
    );
}

#[test]
fn a_type_a_later_line_settled_is_checked_on_the_next() {
    repl_error(
        &["let xs = [];", "xs.push(1);", r#"xs.push("a");"#],
        "[Int], String",
    );
}

#[test]
fn a_lambda_whose_body_needs_its_parameter_type_is_settled_on_its_line() {
    // The line runs before the next one exists, so `*` is picked now.
    repl_error(&["let double = fn(x) => x * 2;"], "Unknown, Int");
}

#[test]
fn a_lambda_with_a_parameter_its_line_said_nothing_about_asks_for_an_annotation() {
    repl_error(
        &["let identity = fn(x) => x;", "identity(1);"],
        "nothing said what its parameters are when it was made",
    );
}

/// A generic function's body is compiled per instance, so a native only one
/// instance calls is first reached by the line that makes that instance.
#[test]
fn a_later_lines_instance_reaches_its_own_natives() {
    let out = repl_output(&[
        "fn size(x) => x.len",
        "print(size([1, 2]))",
        "print(size(\"abc\"))",
        "print(size(%{1: 2}))",
    ]);
    assert_eq!(out, "2\n3\n1\n");
}
