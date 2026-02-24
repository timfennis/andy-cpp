use ndc_lib::interpreter::Interpreter;
use owo_colors::OwoColorize;
use std::fs;
use std::path::PathBuf;

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

fn run_test(path: PathBuf) -> Result<(), std::io::Error> {
    let contents = fs::read_to_string(&path)?;

    enum Mode {
        None,
        Program,
        Expect,
        ExpectError,
    }

    let mut mode = Mode::None;
    let mut program = String::new();
    let mut expect_output = String::new();
    let mut expect_error = String::new();

    for line in contents.split_inclusive('\n') {
        match line {
            "--PROGRAM--\n" => mode = Mode::Program,
            "--EXPECT--\n" => mode = Mode::Expect,
            "--EXPECT-ERROR--\n" => mode = Mode::ExpectError,
            _ => match mode {
                Mode::None => panic!("unexpected line in file, not in mode"),
                Mode::Program => program.push_str(line),
                Mode::Expect => expect_output.push_str(line),
                Mode::ExpectError => expect_error.push_str(line),
            },
        }
    }

    print!("Running {path:?}...");

    let mut interpreter = Interpreter::new(Vec::new());
    let interpreter_result = interpreter.run_str(&program, false);

    let program_had_error = interpreter_result.is_err();
    let actual_error = interpreter_result.unwrap_or_else(|err| format!("{err:?}"));

    let environment = interpreter.environment();
    let environment = environment.borrow();
    let output = environment
        .get_output()
        .expect("interpreter must have output in test context");
    let output = String::from_utf8(output).expect("test output must be valid UTF-8");

    assert!(
        !expect_error.is_empty() || !program_had_error,
        "Unexpected error when running program: {actual_error}"
    );

    if !expect_output.is_empty() && output.trim_end() != expect_output.trim_end() {
        println!(" {}", "ERR".red().bold());
        panic!(
            "\n\tThere was a problem running {path:?}\n\tActual output {}\n\tdid not match\n\tExpected output {}\n",
            output.trim_end(),
            expect_output.trim_end()
        );
    }

    if !expect_error.is_empty() && !actual_error.trim().contains(expect_error.trim()) {
        println!(" {}", "ERR".red().bold());
        panic!(
            "\n\tThere was a problem running {path:?}\n\tExpected error:\t{}\n\tActual error:\t{}\n",
            expect_error.trim_end(),
            actual_error.trim_end()
        );
    }

    println!(" {}", "OK".green().bold());

    Ok(())
}
