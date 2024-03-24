use colored::Colorize;
use ndc_lib::interpreter::Interpreter;
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn example_programs() {
    run_dir(Path::new("tests/programs/"));
}

fn run_dir<P: AsRef<Path>>(dir: P) {
    match fs::read_dir(dir) {
        Ok(files) => {
            // Sort the files in to ensure the tests are executed in the intended order
            let mut files = files.collect::<Vec<_>>();
            assert_ne!(files.len(), 0, "no files were found at all");
            files.sort_unstable_by_key(|r| r.as_ref().map(|e| e.path()).ok());

            for file in files {
                match file {
                    Ok(file) if file.path().extension() == Some("ndct".as_ref()) => {
                        run_test(file.path()).expect("something went wrong while running the test")
                    }
                    Ok(file) if file.path().is_dir() => {
                        run_dir(file.path());
                    }
                    _ => panic!("invalid test file: {file:?}"),
                }
            }
        }
        Err(err) => panic!("Error reading test programs {err}"),
    }
}

fn run_test(path: PathBuf) -> Result<(), std::io::Error> {
    let contents = fs::read_to_string(path.clone())?;
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
            "--PROGRAM--\n" => {
                mode = Mode::Program;
            }
            "--EXPECT--\n" => {
                mode = Mode::Expect;
            }
            "--EXPECT-ERROR--\n" => {
                mode = Mode::ExpectError;
            }
            _ => match mode {
                Mode::None => panic!("unexpected line in file, not in mode"),
                Mode::Program => program.push_str(line),
                Mode::Expect => expect_output.push_str(line),
                Mode::ExpectError => expect_error.push_str(line),
            },
        }
    }
    // For now let's trim end both result and expect to ensure that any trailing line breaks don't cause issues
    print!("Running {path:?}...");

    let mut interpreter = Interpreter::new(Box::<Vec<u8>>::default());
    let interpreter_result = interpreter.run_str(&program, false);

    let program_had_error = interpreter_result.is_err();

    let actual_error = interpreter_result.unwrap_or_else(|err| format!("{err}"));

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
        panic!("\n\tThere was a problem running {path:?}\n\tActual output {}\n\tdid not match\n\tExpected output {}\n", output.trim_end(), expect_output.trim_end());
    }

    if !expect_error.is_empty() && !actual_error.trim().contains(expect_error.trim()) {
        println!(" {}", "ERR".red().bold());
        panic!("\n\tThere was a problem running {path:?}\n\tExpected error:\t{}\n\tActual error:\t{}\n",  expect_error.trim_end(), actual_error.trim_end());
    }

    println!(" {}", "OK".green().bold());

    Ok(())
}
