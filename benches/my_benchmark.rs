use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use std::fs;
use std::path::Path;

use ndc_lib::interpreter::{Interpreter, InterpreterError};

fn run_string(input: &str) -> Result<String, InterpreterError> {
    let buf: Vec<u8> = vec![];
    let mut interpreter = Interpreter::new(Box::new(buf));
    // TODO: Is this black_box needed?
    interpreter.run_str(black_box(input), false)
}

fn math_benches(c: &mut Criterion) {
    let mut rng = ChaCha8Rng::seed_from_u64(31011991);
    let operators = ["+", "-", "*", "/", "\\", "^", "%", "%%"];
    let types = ["int", "float", "rational", "complex"];

    for typ in types {
        for operator in operators {
            let mut program = String::new();
            for _ in 0..100 {
                let left = rng.gen_range(-10_000..=10_000);
                let right = match operator {
                    "^" => rng.gen_range(1..=25),
                    "/" => rng.gen_range(1..10_000),
                    _ => rng.gen_range(-10_000..=10_000),
                };
                program.push_str(&match typ {
                    "int" => format!("{left} {operator} {right};\n"),
                    "float" => format!("{left}.0 {operator} {right}.0;\n"),
                    "rational" => format!(
                        "({left}/{}) {operator} ({right}/{});\n",
                        rng.gen_range(1..=10_000),
                        rng.gen_range(1..=10_000),
                    ),
                    "complex" => {
                        format!(
                            "({left}.0 + {}.0i) {operator} ({right}.0 + {}.0i);\n",
                            rng.gen_range(-10_000..=10_000),
                            rng.gen_range(-10_000..=10_000),
                        )
                    }
                    _ => unreachable!(),
                });
            }

            c.bench_function(&format!("operator {} {}", operator, typ), |b| {
                b.iter(|| run_string(&program))
            });
            println!("{program}");
        }
    }
}
fn directory_benches(c: &mut Criterion) {
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

criterion_group!(benches, directory_benches, math_benches);
criterion_main!(benches);
