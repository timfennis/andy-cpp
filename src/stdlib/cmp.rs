#[andy_cpp_macros::export_module]
mod inner {
    use anyhow::anyhow;

    use crate::compare::FallibleOrd;
    use crate::interpreter::value::Value;
    use std::cmp::Ordering;

    pub fn assert(value: bool) -> anyhow::Result<Value> {
        if value {
            Ok(Value::unit())
        } else {
            Err(anyhow!("failed asserting that argument is true"))
        }
    }

    pub fn assert_eq(left: &Value, right: &Value) -> anyhow::Result<Value> {
        if left == right {
            Ok(Value::unit())
        } else {
            Err(anyhow!(format!(
                "failed asserting that {left} equals {right}"
            )))
        }
    }

    pub fn assert_ne(left: &Value, right: &Value) -> anyhow::Result<Value> {
        if left == right {
            Err(anyhow!(format!(
                "failed asserting that {left} does not equal {right}"
            )))
        } else {
            Ok(Value::unit())
        }
    }

    #[function(name = "assert")]
    pub fn assert_with_message(value: bool, message: &str) -> anyhow::Result<Value> {
        if value {
            Ok(Value::unit())
        } else {
            Err(anyhow!(message.to_string()))
        }
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
