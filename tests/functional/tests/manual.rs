//! Runs the `ndc` code blocks in the manual (`manual/src`), the way its Run
//! buttons do, and checks that each one still says what happens:
//!
//! - A block runs without an error, unless its fence is marked `ndc,fails`,
//!   in which case it must fail. Blocks marked `ndc,norun` aren't run.
//! - A block that runs shows something: it prints, or ends in a value.
//! - `print(...);  // text` says what that print outputs. These comments are
//!   matched to the printed lines in order; a block that prints in a loop
//!   prints more lines than it has prints, so there they aren't checked.
use ndc_interpreter::Interpreter;
use std::fs;
use std::path::Path;

include!(concat!(env!("OUT_DIR"), "/manual_tests.rs"));

/// Runs the block whose opening fence is on `line` of `file` in `manual/src`.
fn run_manual_example(file: &str, line: usize) {
    let place = format!("manual/src/{file}:{line}");
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../manual/src")
        .join(file);
    let markdown = fs::read_to_string(&path).expect("the manual page must be readable");
    let mut lines = markdown.lines().skip(line - 1);
    let fence = lines.next().expect("the block's fence must exist");
    let fails = fence
        .trim_start_matches("```ndc")
        .split(',')
        .any(|flag| flag.trim() == "fails");
    let code: Vec<&str> = lines.take_while(|l| !l.starts_with("```")).collect();

    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);
    let result = interpreter.eval(&code.join("\n"));
    let output = String::from_utf8(interpreter.get_output().expect("output is captured"))
        .expect("output must be valid UTF-8");

    match (result, fails) {
        (Ok(_), true) => panic!("{place} is marked ```ndc,fails but runs without an error"),
        (Err(err), false) => {
            panic!("{place} fails; mark it ```ndc,fails if that's what it shows:\n{err:?}")
        }
        (Ok(value), false) => assert!(
            !output.is_empty() || !value.to_string().is_empty(),
            "{place} shows nothing when run; print what the example is about"
        ),
        (Err(_), true) => {}
    }

    let expected: Vec<&str> = code.iter().filter_map(|l| print_comment(l)).collect();
    if expected.is_empty() {
        return;
    }
    let printed: Vec<&str> = output.lines().collect();
    if expected.len() == printed.len() {
        for (n, (expected, printed)) in expected.iter().zip(&printed).enumerate() {
            assert_eq!(
                expected,
                printed,
                "{place}: the comment on print {} doesn't match what it printed",
                n + 1
            );
        }
    } else {
        let loops = code
            .iter()
            .any(|l| l.contains("for ") || l.contains("while "));
        assert!(
            loops,
            "{place} has {} commented prints but printed {} lines:\n{output}",
            expected.len(),
            printed.len()
        );
    }
}

/// The comment after a `print(...)` call: `foo` for `print(x);  // foo`.
/// The comment is the first `//` after the call's closing parenthesis, so a
/// `//` inside a string argument doesn't count.
fn print_comment(line: &str) -> Option<&str> {
    let line = line.trim_start();
    if !line.starts_with("print(") {
        return None;
    }
    line.match_indices("//").find_map(|(at, _)| {
        let code = line[..at].trim_end();
        (code.ends_with(')') || code.ends_with(");")).then(|| {
            let comment = &line[at + 2..];
            comment.strip_prefix(' ').unwrap_or(comment).trim_end()
        })
    })
}

#[test]
fn a_print_comment_starts_after_the_call() {
    assert_eq!(print_comment("print(1);  // 1"), Some("1"));
    assert_eq!(print_comment(r#"print("a//b");  // a//b"#), Some("a//b"));
    assert_eq!(print_comment("print(1);"), None);
    assert_eq!(print_comment("let x = 1;  // 1"), None);
}
