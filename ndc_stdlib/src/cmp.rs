use ndc_macros::export_module;
use ndc_vm::value::Value;

#[export_module]
mod inner {
    use anyhow::anyhow;

    use std::cmp::Ordering;

    /// Produces an error if the argument is not true.
    pub fn assert(value: bool) -> anyhow::Result<()> {
        if value {
            Ok(())
        } else {
            Err(anyhow!("failed asserting that argument is true"))
        }
    }

    /// Produces an error if the arguments aren't equal to each other.
    pub fn assert_eq(left: Value, right: Value) -> anyhow::Result<()> {
        if left == right {
            Ok(())
        } else {
            Err(anyhow!(format!(
                "failed asserting that {left} equals {right}"
            )))
        }
    }

    /// Produces an error if the arguments are equal to each other.
    pub fn assert_ne(left: Value, right: Value) -> anyhow::Result<()> {
        if left == right {
            Err(anyhow!(format!(
                "failed asserting that {left} does not equal {right}"
            )))
        } else {
            Ok(())
        }
    }

    /// Produces the error specified by the `message` parameter if the `value` argument is not true.
    #[function(name = "assert")]
    pub fn assert_with_message(value: bool, message: &str) -> anyhow::Result<()> {
        if value {
            Ok(())
        } else {
            Err(anyhow!(message.to_string()))
        }
    }

    /// Returns the larger of `left` and `right`, preferring `left` if they are equal.
    pub fn max(left: Value, right: Value) -> anyhow::Result<Value> {
        match left.partial_cmp(&right) {
            Some(Ordering::Equal) | Some(Ordering::Greater) => Ok(left),
            Some(Ordering::Less) => Ok(right),
            None => Err(anyhow!(
                "cannot compare {} and {}",
                left.static_type(),
                right.static_type()
            )),
        }
    }

    /// Returns the smaller of `left` and `right`, preferring `left` if they are equal.
    pub fn min(left: Value, right: Value) -> anyhow::Result<Value> {
        match left.partial_cmp(&right) {
            Some(Ordering::Equal) | Some(Ordering::Less) => Ok(left),
            Some(Ordering::Greater) => Ok(right),
            None => Err(anyhow!(
                "cannot compare {} and {}",
                left.static_type(),
                right.static_type()
            )),
        }
    }
}
