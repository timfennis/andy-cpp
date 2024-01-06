use ndc_lib::interpreter::Interpreter;
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn example_programs() {
    run_dir(Path::new("tests/programs/"));
}

fn run_dir<P: AsRef<Path>>(dir: P) {
    if let Ok(files) = fs::read_dir(dir) {
        for file in files {
            match file {
                Ok(file) if file.path().extension() == Some("ndct".as_ref()) => {
                    run_test(file.path()).expect("something went wrong while running the test")
                }
                Ok(file) if file.path().is_dir() => {
                    run_dir(file.path());
                }
                _ => panic!("invalid test file"),
            }
        }
    }
}

fn run_test(path: PathBuf) -> Result<(), std::io::Error> {
    let contents = fs::read_to_string(path.clone())?;
    enum Mode {
        None,
        Program,
        Expect,
    }

    let mut mode = Mode::None;
    let mut program = String::new();
    let mut expect = String::new();

    for line in contents.split_inclusive('\n') {
        match line {
            "--PROGRAM--\n" => {
                mode = Mode::Program;
            }
            "--EXPECT--\n" => {
                mode = Mode::Expect;
            }
            _ => match mode {
                Mode::None => panic!("unexpected line in file, not in mode"),
                Mode::Program => program.push_str(line),
                Mode::Expect => expect.push_str(line),
            },
        }
    }

    let mut b = Vec::new();
    let mut interpreter = Interpreter::new(&mut b);
    let _ = interpreter
        .run_str(&program, false)
        .unwrap_or_else(|err| format!("{err}"));

    // TODO: should we even require valid UTF-8 here?
    let result = String::from_utf8(b).expect("output of program was not valid UTF-8");

    // For now let's trim end both result and expect to ensure that any trailing line breaks don't cause issues
    print!("Running {path:?}...");
    assert_eq!(result.trim_end(), expect.trim_end(), "There was a problem running {path:?}, actual output '{}' did not match expected output '{}'", result.trim_end(), expect.trim_end());
    println!(" OK");

    Ok(())
}
