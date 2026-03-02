use ndc_lib::interpreter::Interpreter;
use ndc_lib::interpreter::environment::Environment;

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

#[cfg(feature = "crypto")]
pub mod crypto;

pub fn register(env: &mut Environment) {
    aoc::register(env);
    cmp::register(env);
    #[cfg(feature = "crypto")]
    crypto::register(env);
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

pub trait WithStdlib: Sized {
    fn with_stdlib(self) -> Self;
}

impl WithStdlib for Interpreter {
    fn with_stdlib(mut self) -> Self {
        self.configure(register);
        self
    }
}
