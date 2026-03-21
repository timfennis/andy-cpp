use ndc_core::FunctionRegistry;
use ndc_vm::NativeFunction;
use std::rc::Rc;

pub mod aoc;
pub mod cmp;
pub mod deque;
pub mod file;
pub mod hash_map;
pub mod heap;
pub mod index;
pub mod list;
pub mod math;
pub mod sequence;
pub mod string;
pub mod value;

#[cfg(feature = "crypto")]
pub mod crypto;
#[cfg(feature = "rand")]
pub mod rand;
#[cfg(feature = "regex")]
pub mod regex;
#[cfg(feature = "serde")]
pub mod serde;

pub fn register(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    aoc::register(env);
    cmp::register(env);
    #[cfg(feature = "crypto")]
    crypto::register(env);
    deque::register(env);
    file::register(env);
    file::register_variadic(env);
    hash_map::register(env);
    heap::register(env);
    index::register(env);
    list::ops::register(env);
    list::register(env);
    math::f64::register(env);
    math::register(env);
    #[cfg(feature = "rand")]
    rand::register(env);
    #[cfg(feature = "regex")]
    regex::register(env);
    sequence::extra::register(env);
    sequence::register(env);
    #[cfg(feature = "serde")]
    serde::register(env);
    string::register(env);
    value::register(env);
}
