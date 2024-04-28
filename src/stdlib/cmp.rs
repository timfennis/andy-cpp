use crate::interpreter::value::Value;
use std::cmp::Ordering;

#[andy_cpp_macros::export_module]
mod inner {

    pub fn assert(value: bool) -> Value {
        assert!(value, "failed asserting that argument is true");
        Value::Unit
    }

    #[function(name = "assert")]
    pub fn assert_with_message(value: bool, message: &str) -> Value {
        assert!(value, "{message}");
        Value::Unit
    }

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
