use crate::interpreter::environment::Environment;
use crate::interpreter::value::Sequence;
use crate::register_fn;
use andy_cpp_macros::export_function;

#[export_function]
fn len(seq: &Sequence) -> usize {
    match seq {
        Sequence::String(s) => s.borrow().len(),
        Sequence::List(l) => l.borrow().len(),
    }
}

pub fn register(env: &mut Environment) {
    register_fn!(env, len);
}
