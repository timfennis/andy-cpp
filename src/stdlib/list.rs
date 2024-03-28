use crate::interpreter::environment::Environment;
use crate::interpreter::value::Value;
use crate::register_fn;
use andy_cpp_macros::export_function;

#[export_function]
fn contains(list: &[Value], elem: &Value) -> bool {
    list.contains(elem)
}

pub fn register(env: &mut Environment) {
    register_fn!(env, contains);
}
