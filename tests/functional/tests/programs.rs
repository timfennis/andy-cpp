use ndc_interpreter::Interpreter;
use std::fs;
use std::path::Path;

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

fn run_ndc_test(path: &Path) -> Result<(), std::io::Error> {
    let contents = fs::read_to_string(path)?;

    let expect_error = contents
        .lines()
        .find_map(|line| line.trim().strip_prefix("// expect-error:"))
        .map(|s| s.trim().to_string())
        .unwrap_or_default();

    let expect_output = contents
        .lines()
        .filter_map(|line| line.trim().strip_prefix("// expect-output:"))
        .map(|s| s.trim())
        .collect::<Vec<_>>()
        .join("\n");

    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);
    let interpreter_result = interpreter.eval(&contents);

    let program_had_error = interpreter_result.is_err();
    let actual_error = interpreter_result.map_or_else(|err| format!("{err:?}"), |v| v.to_string());

    assert!(
        !expect_error.is_empty() || !program_had_error,
        "Unexpected error when running program: {actual_error}"
    );

    if !expect_error.is_empty() && !actual_error.trim().contains(expect_error.trim()) {
        panic!(
            "\n\tThere was a problem running {path:?}\n\tExpected error:\t{}\n\tActual error:\t{}\n",
            expect_error.trim(),
            actual_error.trim()
        );
    }

    if !expect_output.is_empty() {
        let output = interpreter
            .get_output()
            .expect("interpreter must have output in test context");
        let output = String::from_utf8(output).expect("test output must be valid UTF-8");

        if output.trim_end() != expect_output.trim_end() {
            panic!(
                "\n\tThere was a problem running {path:?}\n\tActual output: {}\n\tdid not match\n\tExpected output: {}\n",
                output.trim_end(),
                expect_output.trim_end()
            );
        }
    }

    Ok(())
}
