use crate::interpreter::environment::Environment;
use crate::interpreter::function::Function;
use crate::interpreter::value::Value;
use andy_cpp_macros::export_function;

#[export_function]
fn reverse(string: String) -> String {
    string.chars().rev().collect()
}

#[export_function]
fn lines(string: String) -> Vec<String> {
    string.lines().map(ToString::to_string).collect()
}

pub fn register(env: &mut Environment) {
    env.declare(
        "lines",
        Value::from(Function::GenericFunction { function: lines }),
    );
    env.declare(
        "reverse",
        Value::from(Function::GenericFunction { function: reverse }),
    );
}
