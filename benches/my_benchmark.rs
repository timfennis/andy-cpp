use criterion::{black_box, criterion_group, criterion_main, Criterion};

use ndc_lib::interpreter::{Interpreter, InterpreterError};

fn run_string(input: &str) -> Result<String, InterpreterError> {
    let buf: Vec<u8> = vec![];
    let mut interpreter = Interpreter::new(Box::new(buf));
    interpreter.run_str(black_box(input), false)
}

fn addition(a: i32, b: i32) {
    let result = run_string(&format!("{} + {}", a, b)).unwrap();
    assert_eq!(result, format!("{}", a + b))
}

fn fibonacci() {
    let program = "fn fib(n) { if n <= 1 { return 1 }; fib(n - 1) + fib(n - 2) }\nfib(20)";
    let result = run_string(program).unwrap();
    assert_eq!(result, "10946");
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("addition", |b| b.iter(|| addition(5, 5)));
    c.bench_function("fibonacci", |b| b.iter(fibonacci));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
