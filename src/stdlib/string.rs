use crate::interpreter::environment::Environment;
use crate::register_fn;
use andy_cpp_macros::export_function;

#[export_function]
fn reversed(string: &str) -> String {
    string.chars().rev().collect()
}

#[export_function]
fn reverse(string: &mut String) {
    *string = string.chars().rev().collect();
}

#[export_function]
fn lines(string: &str) -> Vec<String> {
    string.lines().map(ToString::to_string).collect()
}

#[export_function]
fn split(string: &str) -> Vec<String> {
    string
        .split_whitespace() // TODO: or split_ascii_whitespace?
        .map(ToString::to_string)
        .collect()
}

#[export_function(name = split)]
fn split_with_pattern(string: &str, pattern: &str) -> Vec<String> {
    string.split(pattern).map(ToString::to_string).collect()
}

#[export_function]
fn trim(string: &str) -> &str {
    string.trim()
}

pub fn register(env: &mut Environment) {
    register_fn!(env, lines);
    register_fn!(env, reversed);
    register_fn!(env, reverse);
    register_fn!(env, split);
    register_fn!(env, trim);
}
