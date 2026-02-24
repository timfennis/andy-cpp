use ndc_lib::interpreter::Interpreter;
use owo_colors::OwoColorize;
use std::fs;
use std::path::PathBuf;

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

fn run_ndc_test(path: PathBuf) -> Result<(), std::io::Error> {
    let contents = fs::read_to_string(&path)?;

    let expect_error = contents
        .lines()
        .find_map(|line| line.trim().strip_prefix("// expect-error:"))
        .map(|s| s.trim().to_string())
        .unwrap_or_default();

    print!("Running {path:?}...");

    let mut interpreter = Interpreter::new(Vec::new());
    let interpreter_result = interpreter.run_str(&contents, false);

    let program_had_error = interpreter_result.is_err();
    let actual_error = interpreter_result.unwrap_or_else(|err| format!("{err:?}"));

    assert!(
        !expect_error.is_empty() || !program_had_error,
        "Unexpected error when running program: {actual_error}"
    );

    if !expect_error.is_empty() && !actual_error.trim().contains(expect_error.trim()) {
        println!(" {}", "ERR".red().bold());
        panic!(
            "\n\tThere was a problem running {path:?}\n\tExpected error:\t{}\n\tActual error:\t{}\n",
            expect_error.trim(),
            actual_error.trim()
        );
    }

    println!(" {}", "OK".green().bold());

    Ok(())
}
