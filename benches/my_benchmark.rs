use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs;
use std::path::Path;

use ndc_lib::interpreter::{Interpreter, InterpreterError};

fn run_string(input: &str) -> Result<String, InterpreterError> {
    let buf: Vec<u8> = vec![];
    let mut interpreter = Interpreter::new(Box::new(buf));
    // TODO: Is this black_box needed?
    interpreter.run_str(black_box(input), false)
}

fn criterion_benchmark(c: &mut Criterion) {
    let dir = Path::new("benches/programs/");
    let mut names = Vec::new();
    match fs::read_dir(dir) {
        Ok(files) => {
            for file in files {
                let file = file.unwrap();
                let program = fs::read_to_string(file.path()).unwrap();
                names.push(file.file_name().to_owned());
                let name = names
                    .last()
                    .and_then(|n| n.to_str())
                    .expect("must have a name");
                let name = &name[0..name.len() - 4];
                c.bench_function(name, |b| b.iter(|| run_string(&program)));
            }
        }
        Err(_) => {
            panic!("failed to read files from directory");
        }
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
