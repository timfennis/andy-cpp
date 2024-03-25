use crate::interpreter::environment::Environment;
use crate::interpreter::function::Function;
use crate::interpreter::value::Value;
use crate::register_fn;
use andy_cpp_macros::export_function;

#[export_function]
fn reversed(string: &str) -> String {
    string.chars().rev().collect()
}

#[export_function]
fn lines(string: &str) -> Vec<String> {
    string.lines().map(ToString::to_string).collect()
}

pub fn register(env: &mut Environment) {
    register_fn!(env, lines);
    register_fn!(env, reversed);
}
