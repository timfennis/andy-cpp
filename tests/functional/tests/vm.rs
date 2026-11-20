//! Programs that pin what the VM does where no program under `programs`
//! does: the compare-and-branch instructions of every comparison, a
//! declaration in a later operand, `and` and `or` assigned to their own
//! right operand, and a map literal longer than a chunk. Each checks what
//! the program prints.

use ndc_interpreter::Interpreter;

/// What `program` prints, which must run without an error.
fn run(program: &str) -> String {
    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);
    if let Err(error) = interpreter.eval(program) {
        panic!("the program fails: {error:?}");
    }
    String::from_utf8(interpreter.get_output().expect("captured output")).expect("output is UTF-8")
}

/// A declaration may reuse the register of a block's variable that went
/// out of scope, so an operand before one is read first:
/// `{ let a = 1; a } + { let b = 2; b }` would otherwise add `b` to itself.
#[test]
fn a_local_read_before_an_operand_that_declares_one() {
    let output = run("fn reused_slot() {
            let r = { let a = 1; a } + { let b = 2; b };
            print(r);
        }
        reused_slot();");
    assert_eq!(output, "3\n");
}

#[test]
fn conditions_that_compare() {
    let output = run(r#"fn test(a, b) {
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
        print(if (1 < 2) { 1 } else { 2 });"#);
    assert_eq!(
        output,
        "100011100011 11010011010 101100101100 100011100011\n1 2 1 2\n1\n"
    );
}

/// `and` and `or` give their value to a local that their right operand
/// reads only once both ran.
#[test]
fn logical_operators() {
    let output = run(r#"fn f(a, b) {
            let x = b;
            x = a and x;
            let y = b;
            y = a or y;
            print(x, y);
        }
        f(true, false);
        f(false, true);"#);
    assert_eq!(output, "false true\nfalse true\n");
}

/// A map hashes a key by what it holds when the key goes in, and every key
/// and value of a map literal is evaluated before any goes in. Here the last
/// value changes the first key, a list, so the map has to find it by what it
/// holds afterwards, however long the literal is.
#[test]
fn a_long_map_literal_hashes_its_keys_after_its_last_expression() {
    let pairs: String = (1..=80).map(|n| format!("[{n}]: {n}, ")).collect();
    let output = run(&format!(
        "let k = [0];
        let m = %{{k: -1, {pairs}[100]: {{ k.push(9); 0 }}}};
        print(m[[0, 9]]);
        print(m.len());"
    ));
    assert_eq!(output, "-1\n82\n");
}
