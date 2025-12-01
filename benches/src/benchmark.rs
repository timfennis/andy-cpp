use criterion::{Criterion, criterion_group, criterion_main};
use ndc_lib::interpreter::{Interpreter, InterpreterError};
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use std::fs;
use std::path::Path;
use std::time::Duration;

fn run_string(input: &str) -> Result<String, InterpreterError> {
    let buf: Vec<u8> = vec![];
    let mut interpreter = Interpreter::new(buf);
    interpreter.run_str(std::hint::black_box(input), false)
}

#[allow(unused)]
fn math_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("math");
    group.warm_up_time(Duration::from_secs(2));
    group.measurement_time(Duration::from_secs(2));
    let mut rng = ChaCha8Rng::seed_from_u64(31011991);
    let operators = [
        ("+", "addition"),
        ("-", "subtraction"),
        ("*", "multiplication"),
        ("/", "division"),
        ("\\", "floor division"),
        ("^", "exponentiation"),
        ("%", "modulo"),
        ("%%", "remainder"),
    ];
    let types = [
        "int",
        "float",
        "rational",
        "complex",
        "2-tuple int",
        "2-tuple float",
    ];
    enum Operand {
        Left,
        Right,
    }
    let mut generate = |operand, operation| match (operand, operation) {
        (Operand::Right, "^") => rng.random_range(2..=9),
        (Operand::Right, "/" | "\\" | "%" | "%%") => rng.random_range(1..=10_000),
        _ => rng.random_range(-10_000..=10_000),
    };
    for typ in types {
        for (operator, operator_name) in operators {
            let mut program = String::new();
            for _ in 0..100 {
                program.push_str(&match typ {
                    "int" => format!(
                        "{} {operator} {};\n",
                        generate(Operand::Left, operator),
                        generate(Operand::Right, operator)
                    ),
                    "float" => format!(
                        "{}.0 {operator} {}.0;\n",
                        generate(Operand::Left, operator),
                        generate(Operand::Right, operator)
                    ),
                    "rational" => format!(
                        "({}/{}) {operator} ({}/{});\n",
                        generate(Operand::Left, operator),
                        generate(Operand::Right, operator),
                        generate(Operand::Left, operator),
                        generate(Operand::Right, operator),
                    ),
                    "complex" => {
                        format!(
                            "({}.0 + {}.0i) {operator} ({}.0 + {}.0i);\n",
                            generate(Operand::Left, operator),
                            generate(Operand::Left, operator),
                            generate(Operand::Right, operator),
                            generate(Operand::Right, operator),
                        )
                    }
                    "2-tuple int" => {
                        format!(
                            "({}, {}) {operator} ({}, {});\n",
                            generate(Operand::Left, operator),
                            generate(Operand::Left, operator),
                            generate(Operand::Right, operator),
                            generate(Operand::Right, operator),
                        )
                    }
                    "2-tuple float" => {
                        format!(
                            "({}.0, {}.0) {operator} ({}.0, {}.0);\n",
                            generate(Operand::Left, operator),
                            generate(Operand::Left, operator),
                            generate(Operand::Right, operator),
                            generate(Operand::Right, operator),
                        )
                    }
                    _ => unreachable!(),
                });
            }

            group.bench_function(format!("{typ} {operator_name}"), |b| {
                b.iter(|| run_string(&program))
            });
        }
    }
}
fn directory_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("programs");
    let dir = Path::new("../benches/programs/");
    let mut names = Vec::new();
    match fs::read_dir(dir) {
        Ok(files) => {
            for file in files {
                let file = file.unwrap();
                let program = fs::read_to_string(file.path()).unwrap();
                names.push(file.file_name().clone());
                let name = names
                    .last()
                    .and_then(|n| n.to_str())
                    .expect("must have a name");
                let name = &name[0..name.len() - 4];
                group.bench_function(name, |b| b.iter(|| run_string(&program)));
            }
        }
        Err(std::io::Error { .. }) => {
            panic!("failed to read files from directory");
        }
    }
}

criterion_group!(benches, directory_benches);
criterion_main!(benches);
