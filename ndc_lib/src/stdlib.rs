use crate::interpreter::environment::Environment;

pub mod aoc;
pub mod cmp;
pub mod deque;
pub mod file;
pub mod hash_map;
pub mod heap;
pub mod list;
pub mod math;
pub mod rand;
pub mod regex;
pub mod sequence;
pub mod serde;
pub mod string;
pub mod value;

pub fn register(env: &mut Environment) {
    aoc::register(env);
    cmp::register(env);
    deque::register(env);
    file::register(env);
    file::register_variadic(env);
    hash_map::register(env);
    heap::register(env);
    list::register(env);
    math::f64::register(env);
    math::register(env);
    rand::register(env);
    regex::register(env);
    sequence::extra::register(env);
    sequence::register(env);
    serde::register(env);
    string::register(env);
    value::register(env);
}
