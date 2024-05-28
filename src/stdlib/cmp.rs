#[andy_cpp_macros::export_module]
mod inner {
    use crate::compare::FallibleOrd;
    use crate::interpreter::value::Value;
    use std::cmp::Ordering;

    pub fn assert(value: bool) -> Value {
        assert!(value, "failed asserting that argument is true");
        Value::Unit
    }

    #[function(name = "assert")]
    pub fn assert_with_message(value: bool, message: &str) -> Value {
        assert!(value, "{message}");
        Value::Unit
    }

    pub fn max(left: &Value, right: &Value) -> Result<Value, anyhow::Error> {
        match left.try_cmp(right)? {
            Ordering::Equal | Ordering::Greater => Ok(left.clone()),
            Ordering::Less => Ok(right.clone()),
        }
    }

    pub fn min(left: &Value, right: &Value) -> Result<Value, anyhow::Error> {
        match left.try_cmp(right)? {
            Ordering::Equal | Ordering::Less => Ok(left.clone()),
            Ordering::Greater => Ok(right.clone()),
        }
    }
}
