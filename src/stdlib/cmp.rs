use crate::interpreter::value::Value;
use std::cmp::Ordering;

#[andy_cpp_macros::export_module]
mod inner {

    //TODO: Make min and max variadic?
    pub fn max(left: &Value, right: &Value) -> Value {
        match left.partial_cmp(right) {
            Some(Ordering::Equal | Ordering::Greater) => left.clone(),
            Some(Ordering::Less) => right.clone(),
            //TODO: support returning Results from functions exposed to the runtime
            None => panic!("these types cannot be compared"),
        }
    }

    pub fn min(left: &Value, right: &Value) -> Value {
        match left.partial_cmp(right) {
            Some(Ordering::Equal | Ordering::Less) => left.clone(),
            Some(Ordering::Greater) => right.clone(),
            //TODO: support returning Results from functions exposed to the runtime
            None => panic!("these types cannot be compared"),
        }
    }
}
