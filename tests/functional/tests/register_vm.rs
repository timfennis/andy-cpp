//! Runs programs on the stack VM and on the register VM, which must print
//! the same, give the same value, and fail with the same error at the same
//! place. The programs use the part of the language the register VM
//! compiles so far.

use ndc_interpreter::{Backend, Interpreter, InterpreterError};
use std::fs;
use std::path::{Path, PathBuf};

/// What running a program did, in a form two runs can be compared by.
#[derive(Debug, PartialEq, Eq)]
struct Outcome {
    output: String,
    /// The program's value, or its error with the span of the error.
    result: Result<String, String>,
}

fn run(backend: Backend, program: &str) -> Outcome {
    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);
    interpreter.set_backend(backend);
    let result = match interpreter.eval(program) {
        Ok(value) => Ok(value.to_string()),
        Err(InterpreterError::Vm(error)) => Err(format!("{} at {:?}", error.message, error.span)),
        Err(other) => Err(format!("{other:?}")),
    };
    let output = String::from_utf8(interpreter.get_output().expect("captured output"))
        .expect("output is UTF-8");
    Outcome { output, result }
}

/// Runs `program` on both VMs, requires the same outcome, and returns it.
fn same_on_both(program: &str) -> Outcome {
    let stack = run(Backend::Stack, program);
    let register = run(Backend::Register, program);
    assert_eq!(stack, register, "the VMs differ on:\n{program}");
    if let Err(error) = &register.result {
        assert!(
            !error.contains("not yet supported"),
            "the register VM can't compile:\n{program}\n{error}"
        );
    }
    register
}

/// A function compiled for one VM can't run on the other, so changing the
/// backend forgets what was declared before, where calling the function
/// would run code the new VM doesn't have.
#[test]
fn changing_the_backend_forgets_earlier_declarations() {
    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);
    interpreter
        .eval("fn f() -> Int { 1 }")
        .expect("a declaration");
    assert_eq!(interpreter.eval("f()").expect("a call").to_string(), "1");

    interpreter.set_backend(Backend::Register);
    assert!(
        interpreter.eval("f()").is_err(),
        "`f` was declared on the stack VM"
    );
    interpreter
        .eval("fn f() -> Int { 2 }")
        .expect("a declaration");
    assert_eq!(interpreter.eval("f()").expect("a call").to_string(), "2");

    // Setting the backend it already has keeps what was declared.
    interpreter.set_backend(Backend::Register);
    assert_eq!(interpreter.eval("f()").expect("a call").to_string(), "2");
}

#[test]
fn int_loop() {
    let outcome = same_on_both(
        "fn run() {
            let i = 0;
            let total = 0;
            while i < 40000 {
                total = total + i * 2 - 1;
                i = i + 1;
            }
            total
        }
        print(run());",
    );
    assert_eq!(outcome.output, "1599920000\n");
}

#[test]
fn fib() {
    let outcome = same_on_both(
        "fn fib(n) {
            if n <= 1 { 1 } else { fib(n - 2) + fib(n - 1) }
        }
        print(fib(15));",
    );
    assert_eq!(outcome.output, "987\n");
}

#[test]
fn the_program_value_is_its_last_expression() {
    let outcome = same_on_both("fn three() { 3 } three()");
    assert_eq!(outcome.result, Ok("3".to_string()));
    same_on_both("fn three() { 3 } three();");
    same_on_both("fn three() { 3 }");
    same_on_both("let x = 4; if x > 3 { x } else { 0 }");
}

#[test]
fn literals_and_native_ops() {
    let outcome = same_on_both(
        r#"print(1.5 + 2.25, 3.0 / 2.0, 2.5 * 2.0, 1.0 - 0.5, -4, -2.5, !true);
        print(3 == 3, 2 != 2, 1 <= 2, 2 >= 3, 1 > 0, 1 < 0, 1.5 < 2.5);
        print("text", None, false, 7 - 10);"#,
    );
    assert_eq!(
        outcome.output,
        "3.75 1.5 5.0 0.5 -4 -2.5 false\ntrue false true false true false true\ntext None false -3\n"
    );
}

#[test]
fn locals_globals_and_assignment() {
    let outcome = same_on_both(
        "let counter = 1;
        fn bump() { counter = counter + 1; }
        bump();
        bump();
        fn local() {
            let a = 1;
            let b = a + 1;
            a = b * 10;
            { let c = a + b; a = c; }
            let d = 4;
            a + d
        }
        print(counter, local());",
    );
    assert_eq!(outcome.output, "3 26\n");
}

#[test]
fn if_as_expression_and_statement() {
    let outcome = same_on_both(
        "fn sign(n) { if n < 0 { -1 } else if n == 0 { 0 } else { 1 } }
        fn check(n) {
            let out = 0;
            if n > 10 { out = 1; } else { out = 2; }
            if n > 100 { out = 3; }
            out
        }
        let nothing = if true { 5 };
        print(sign(-5), sign(0), sign(9), check(50), check(500), check(1), nothing);",
    );
    assert_eq!(outcome.output, "-1 0 1 1 3 2 \n");
}

#[test]
fn while_with_break_and_continue() {
    let outcome = same_on_both(
        "fn sum_odd_below(limit) {
            let i = 0;
            let total = 0;
            while true {
                i = i + 1;
                if i >= limit { break; }
                if i == i / 2 * 2 { continue; }
                total = total + i;
            }
            total
        }
        print(sum_odd_below(10));",
    );
    assert_eq!(outcome.output, "25\n");
}

#[test]
fn return_from_inside_a_loop() {
    let outcome = same_on_both(
        "fn first_square_above(n) {
            let i = 0;
            while true {
                if i * i > n { return i; }
                i = i + 1;
            }
            0
        }
        print(first_square_above(50));",
    );
    assert_eq!(outcome.output, "8\n");
}

#[test]
fn generic_functions_run_as_instances() {
    let outcome = same_on_both("fn inc(x) { x + 1 } print(inc(1), inc(2.5));");
    assert_eq!(outcome.output, "2 3.5\n");
}

#[test]
fn natives_that_take_no_vm() {
    let outcome = same_on_both("print(max(3, 7), 7 / 2, 5 % 3, abs(-4));");
    assert_eq!(outcome.output, "7 3 2 4\n");
}

#[test]
fn a_local_read_before_an_operand_that_assigns_it() {
    let outcome = same_on_both(
        "fn op() {
            let x = 1;
            let y = x + { x = 5; x };
            print(y, x);
        }
        fn call() {
            let x = 1;
            print(x, { x = 2; x }, x);
        }
        fn reused_slot() {
            let r = { let a = 1; a } + { let b = 2; b };
            print(r);
        }
        fn into_itself() {
            let x = 1;
            x = x + { x = 10; x };
            print(x);
        }
        op();
        call();
        reused_slot();
        into_itself();",
    );
    assert_eq!(outcome.output, "6 5\n1 2 2\n3\n11\n");
}

#[test]
fn integer_overflow() {
    for program in [
        "fn grow(n) { n * 1000000 } print(grow(grow(grow(grow(5)))));",
        "fn grow(n) { n * 1000 } print(grow(grow(grow(grow(grow(grow(grow(5))))))));",
        "fn up(n) { n + 1 } let big = 9223372036854775807; print(up(big));",
        "fn down(n) { n - 1 } let small = -9223372036854775807; print(down(down(small)));",
    ] {
        let outcome = same_on_both(program);
        assert!(
            outcome
                .result
                .as_ref()
                .is_err_and(|error| error.contains("overflowed")),
            "{outcome:?}"
        );
    }
}

#[test]
fn conditions_that_compare() {
    let outcome = same_on_both(
        r#"fn test(a, b) {
            let out = 0;
            if a < b { out = out + 1; }
            if a <= b { out = out + 10; }
            if a > b { out = out + 100; }
            if a >= b { out = out + 1000; }
            if a == b { out = out + 10000; }
            if a != b { out = out + 100000; }
            if a < 3 { out = out + 1000000; }
            if a <= 3 { out = out + 10000000; }
            if a > 3 { out = out + 100000000; }
            if a >= 3 { out = out + 1000000000; }
            if a == 3 { out = out + 10000000000; }
            if a != 3 { out = out + 100000000000; }
            out
        }
        fn float(x) { if x < 2.5 { 1 } else { 2 } }
        fn text(a, b) { if a < b { 1 } else { 2 } }
        print(test(1, 2), test(3, 3), test(5, 4), test(-40000, 40000));
        print(float(1.0), float(3.0), text("a", "b"), text("b", "a"));
        print(if (1 < 2) { 1 } else { 2 });"#,
    );
    assert_eq!(
        outcome.output,
        "100011100011 11010011010 101100101100 100011100011\n1 2 1 2\n1\n"
    );
}

#[test]
fn an_error_in_a_native() {
    let outcome = same_on_both("print(1); print(5 % 0); print(2);");
    assert_eq!(outcome.output, "1\n");
    assert!(outcome.result.is_err());
}

#[test]
fn natives_that_call_back_into_the_program() {
    let outcome = same_on_both(
        r#"fn twice(c) { c ++ c }
        fn not_b(c) { c != "b" }
        fn longer(a, b) { a ++ b }
        print("ab".map(twice), "abcd".filter(not_b), "xyz".fold("", longer));
        print("abc".map(twice).map(twice));"#,
    );
    assert_eq!(
        outcome.output,
        "[\"aa\",\"bb\"] [\"a\",\"c\",\"d\"] xyz\n[\"aaaa\",\"bbbb\",\"cccc\"]\n"
    );
}

#[test]
fn an_error_inside_a_callback() {
    let outcome = same_on_both(
        r#"fn bad(c) { if c == "b" { 5 % 0 } else { 1 } }
        print("a".map(bad));
        print("ab".map(bad));"#,
    );
    assert_eq!(outcome.output, "[1]\n");
    assert!(outcome.result.is_err());
}

#[test]
fn function_values() {
    let outcome = same_on_both(
        "fn double(x) { x * 2 }
        fn pick() { double }
        fn apply(f, x) { f(x) }
        let f = double;
        fn local() { let g = double; g(5) }
        print(f(21), pick()(4), apply(double, 3), local());",
    );
    assert_eq!(outcome.output, "42 8 6 10\n");
    same_on_both("let x = 3; x(1)");
}

#[test]
fn struct_constructors_and_accessors() {
    let outcome = same_on_both(
        "struct Point { x: Int, y: Int }
        fn norm(p) { p.x * p.x + p.y * p.y }
        fn local() {
            struct Pair { a: Int, b: Int }
            let p = Pair(3, 4);
            p.a + p.b
        }
        let p = Point(3, 4);
        print(p, p.x, norm(p), local());",
    );
    assert_eq!(outcome.output, "Point {x: 3, y: 4} 3 25 7\n");
}

#[test]
fn closures_share_the_variables_they_capture() {
    let outcome = same_on_both(
        "fn counter() {
            let n = 0;
            fn next() { n = n + 1; n }
            next
        }
        let c = counter();
        let d = counter();
        print(c(), c(), d(), c());
        fn adder(k) { fn(x) => x + k }
        print(adder(3)(4));
        fn nested() {
            let x = 1;
            fn outer() {
                fn inner() { x = x + 10; x }
                inner()
            }
            outer();
            print(x, outer(), x);
        }
        nested();
        print(\"ab\".map(fn(c) => c ++ \"!\"));",
    );
    assert_eq!(outcome.output, "1 2 1 3\n7\n11 21 21\n[\"a!\",\"b!\"]\n");
}

#[test]
fn functions_that_capture_themselves() {
    let outcome = same_on_both(
        "fn fact(n) {
            fn go(n) { if n <= 1 { 1 } else { n * go(n - 1) } }
            go(n)
        }
        fn even(n) {
            fn is_even(n) { if n == 0 { true } else { !is_even(n - 1) } }
            is_even(n)
        }
        print(fact(10), even(7), even(8));",
    );
    assert_eq!(outcome.output, "3628800 false true\n");
}

/// Each run of a loop's body declares new variables, so each closure made
/// in it has its own.
#[test]
fn closures_made_in_a_loop() {
    let outcome = same_on_both(
        "fn loops() {
            let a = fn() => 0;
            let b = fn() => 0;
            let i = 0;
            while i < 2 {
                let j = i * 10;
                fn get() { j }
                if i == 0 { a = get; } else { b = fn() => j; }
                j = j + 1;
                i = i + 1;
            }
            print(a(), b());
        }
        loops();",
    );
    assert_eq!(outcome.output, "1 11\n");
}

#[test]
fn pure_functions_remember_their_results() {
    let outcome = same_on_both(
        r#"pure fn slow(n) { print("computing", n); n * 2 }
        print(slow(2), slow(2), slow(3));
        fn local() {
            pure fn fib(n) { if n <= 1 { n } else { fib(n - 1) + fib(n - 2) } }
            fib(80)
        }
        print(local());
        pure fn generic(x) { print("once", x); x }
        print(generic(1), generic(1), generic(1.5));"#,
    );
    assert_eq!(
        outcome.output,
        "computing 2\ncomputing 3\n4 4 6\n23416728348467685\nonce 1\nonce 1.5\n1 1 1.5\n"
    );
}

/// A variable that a call can change is read before the call, as the
/// stack VM reads it: a captured local, a closure's captured variable and a
/// global.
#[test]
fn a_variable_a_call_changes_is_read_before_the_call() {
    let outcome = same_on_both(
        "let g = 1;
        fn set_g() { g = 100; 1 }
        print(g + set_g(), g);
        fn cell() {
            let x = 1;
            fn set_x() { x = 100; 1 }
            print(x + set_x(), x);
            x = 1;
            print(max(x, set_x()), x);
        }
        cell();
        fn upvalue() {
            let y = 1;
            fn inner() {
                fn set_y() { y = 100; 1 }
                print(y + set_y(), y);
            }
            inner();
        }
        upvalue();",
    );
    assert_eq!(outcome.output, "2 100\n2 100\n1 100\n2 100\n");
}

#[test]
fn literals() {
    let outcome = same_on_both(
        r#"fn values() {
            let x = 2;
            let t = (1, x, "three");
            let l = [[x], [x + 1]];
            let m = %{"a": [x], "b": [x + 1]};
            let s = %{1, x};
            let d = %{:0, "a": 1};
            let r = 1..4;
            let ri = 1..=x;
            print(t, l, m["b"], s.len(), d["zz"], d["a"], r.list(), ri.list(), (), 1e3, 2.5);
            print(0x10, 0b101, 1_000, (1..).take(3).list());
        }
        values();"#,
    );
    assert_eq!(
        outcome.output,
        "(1,2,\"three\") [[2],[3]] [3] 2 0 1 [1,2,3] [1,2]  1000.0 2.5\n16 5 1000 [1,2,3]\n"
    );
}

/// A literal with more elements than are made at once is the same as a
/// short one.
#[test]
fn long_literals() {
    let elements = (0..150).map(|n| n.to_string()).collect::<Vec<_>>();
    let list = format!("[{}]", elements.join(", "));
    let pairs = (0..150)
        .map(|n| format!("{n}: {}", n * 2))
        .collect::<Vec<_>>()
        .join(", ");
    let outcome = same_on_both(&format!(
        "fn f() {{
            let l = {list};
            let m = %{{{pairs}}};
            let d = %{{:7, {pairs}}};
            print(l.len(), l[149], m.len(), m[149], d[149], d[1000]);
        }}
        f();"
    ));
    assert_eq!(outcome.output, "150 149 150 298 298 7\n");
}

/// `and` and `or` give their value to a local that their right operand
/// reads only once both ran.
#[test]
fn logical_operators() {
    let outcome = same_on_both(
        r#"fn f(a, b) {
            let x = b;
            x = a and x;
            let y = b;
            y = a or y;
            let z = a or { print("ran"); b };
            print(x, y, z, a and b, a or b, !a and !b);
        }
        f(true, false);
        f(false, true);
        f(false, false);"#,
    );
    assert_eq!(
        outcome.output,
        "false true true false true false\nran\nfalse true true false true false\nran\nfalse false false false false true\n"
    );
}

#[test]
fn for_loops() {
    let outcome = same_on_both(
        r#"fn sums() {
            let total = 0;
            for x in [1, 2, 3, 4, 5, 6] {
                if x == 2 { continue; }
                if x == 5 { break; }
                total = total + x;
            }
            let pairs = [];
            for (a, b) in [(1, 2), (3, 4)], c in "xy", if a > 1 {
                pairs.push((a + b, c));
            }
            let words = [];
            for (first, (x, y)) in [(1, (2, 3))] { words.push(first + x + y); }
            for c in "héllo", if c != "l" { words.push(c.len()); }
            let m = %{"a": 1, "b": 2};
            let keys = [];
            for (k, v) in m { keys.push((k, v)); }
            print(total, pairs, words, keys.sorted());
        }
        sums();
        let top = [];
        for i in 1..=3 { top.push(i * i); }
        print(top);"#,
    );
    assert_eq!(
        outcome.output,
        "8 [(7,\"x\"),(7,\"y\")] [6,1,1,1] [(\"a\",1),(\"b\",2)]\n[1,4,9]\n"
    );
}

#[test]
fn comprehensions() {
    let outcome = same_on_both(
        r#"fn f() {
            let xs = [1, 2, 3, 4];
            let squares = [x * x for x in xs, if x % 2 == 0];
            let grid = [(x, y) for x in 1..3, y in 1..3, if x != y];
            let by_parity = %{x % 2: x for x in xs};
            let counts = %{:0, x: x * 10 for x in xs, if x > 2};
            let set = %{x % 3 for x in xs};
            let nested = [[y for y in 0..x] for x in 1..4];
            let stopped = [if x == 3 { break } else { x } for x in xs];
            let skipped = [if x == 2 { continue } else { x } for x in xs];
            print(squares, grid, by_parity[0], by_parity[1], counts[3], counts[9], set.len());
            print(nested, stopped, skipped);
        }
        f();"#,
    );
    assert_eq!(
        outcome.output,
        "[4,16] [(1,2),(2,1)] 4 3 30 0 3\n[[0],[0,1],[0,1,2]] [1,2] [1,3,4]\n"
    );
}

/// Each run of a `for` loop's body declares its variables anew, so each
/// closure made in it has its own.
#[test]
fn closures_made_in_a_for_loop() {
    let outcome = same_on_both(
        "fn f() {
            let getters = [];
            for x in 1..4 {
                let doubled = x * 2;
                getters.push(fn() => x + doubled);
            }
            let comprehended = [fn() => y for y in 1..4];
            print(getters.map(fn(g) => g()), comprehended.map(fn(g) => g()));
        }
        f();",
    );
    assert_eq!(outcome.output, "[3,6,9] [1,2,3]\n");
}

#[test]
fn destructuring_declarations() {
    let outcome = same_on_both(
        r#"fn f() {
            let pair = (1, 2);
            let (a, b) = pair;
            let (c, (d, e)) = (3, [4, 5]);
            let (x, y) = "hi";
            print(a, b, c, d, e, x, y, pair);
        }
        f();
        let (g, h) = (7, 8);
        print(g + h);
        fn bad() { let (p, q) = [1, 2, 3]; p + q }
        bad();"#,
    );
    assert_eq!(outcome.output, "1 2 3 4 5 h i (1,2)\n15\n");
    assert!(outcome.result.is_err());
}

#[test]
fn deep_recursion() {
    let outcome = same_on_both(
        "fn depth(n) { if n == 0 { 0 } else { 1 + depth(n - 1) } }
        print(depth(10000));",
    );
    assert_eq!(outcome.output, "10000\n");
}

#[test]
fn recursion_past_the_call_depth_limit() {
    let outcome = same_on_both("fn forever(n) { forever(n + 1) } forever(0);");
    assert!(
        outcome
            .result
            .as_ref()
            .is_err_and(|error| error.contains("calls go more than")),
        "{outcome:?}"
    );
}

/// Every program under `programs/` that the register VM compiles does on
/// it what it does on the stack VM.
#[test]
fn the_test_programs_it_compiles() {
    fn programs(dir: &Path, out: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(dir).expect("a directory of programs") {
            let path = entry.expect("an entry").path();
            if path.is_dir() {
                programs(&path, out);
            } else if path.extension().is_some_and(|extension| extension == "ndc") {
                out.push(path);
            }
        }
    }
    let mut paths = Vec::new();
    programs(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("programs"),
        &mut paths,
    );
    paths.sort();
    let mut compiled = 0;
    for path in &paths {
        let program = fs::read_to_string(path).expect("a program");
        let register = run(Backend::Register, &program);
        if register
            .result
            .as_ref()
            .is_err_and(|error| error.contains("not yet supported on the register VM"))
        {
            continue;
        }
        compiled += 1;
        let stack = run(Backend::Stack, &program);
        assert_eq!(stack, register, "the VMs differ on {}", path.display());
    }
    #[allow(
        clippy::print_stdout,
        reason = "reports how far the register VM has come"
    )]
    {
        println!("compared {compiled} of {} programs", paths.len());
    }
    assert!(compiled > 0);
}

#[test]
fn constructs_it_cant_compile_yet_are_errors() {
    for (program, construct) in [
        ("let x = 1; x += 1;", "an assignment with an operator"),
        (
            "let xs = [1]; xs[0] = 2;",
            "assigning to anything but a variable",
        ),
        ("let x: Any = 1; cast::<Int>(x)", "intrinsics"),
    ] {
        let outcome = run(Backend::Register, program);
        let error = outcome.result.expect_err(program);
        assert!(
            error.contains("not yet supported on the register VM") && error.contains(construct),
            "{program}: {error}"
        );
    }
}

/// A map hashes a key by what it holds when the key goes in, and every key
/// and value of a map literal is evaluated before any goes in. Here the last
/// value changes the first key, a list, so the map has to find it by what it
/// holds afterwards, however long the literal is.
#[test]
fn a_long_map_literal_hashes_its_keys_after_its_last_expression() {
    let pairs: String = (1..=80).map(|n| format!("[{n}]: {n}, ")).collect();
    let outcome = same_on_both(&format!(
        "let k = [0];
        let m = %{{k: -1, {pairs}[100]: {{ k.push(9); 0 }}}};
        print(m[[0, 9]]);
        print(m.len());"
    ));
    assert_eq!(outcome.output, "-1\n82\n");
}
