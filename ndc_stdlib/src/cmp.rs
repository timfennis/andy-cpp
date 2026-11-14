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

    /// Stops the program with the error `message`. It never returns, so it
    /// can end a branch whose other branches give a value.
    #[function(signature = "(String) -> Never")]
    pub fn panic(message: &str) -> anyhow::Result<Value> {
        Err(anyhow!(message.to_string()))
    }

    /// Produces an error if the arguments aren't equal to each other.
    #[function(signature = "<A, B>(A, B)")]
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
    #[function(signature = "<A, B>(A, B)")]
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
    #[function(signature = "<T>(T, T) -> T")]
    pub fn max(left: Value, right: Value) -> anyhow::Result<Value> {
        larger(left, right)
    }

    /// Returns the smaller of `left` and `right`, preferring `left` if they are equal.
    #[function(signature = "<T>(T, T) -> T")]
    pub fn min(left: Value, right: Value) -> anyhow::Result<Value> {
        smaller(left, right)
    }

    fn larger(left: Value, right: Value) -> anyhow::Result<Value> {
        match left.partial_cmp(&right) {
            Some(Ordering::Equal | Ordering::Greater) => Ok(left),
            Some(Ordering::Less) => Ok(right),
            None => Err(incomparable(&left, &right)),
        }
    }

    fn smaller(left: Value, right: Value) -> anyhow::Result<Value> {
        match left.partial_cmp(&right) {
            Some(Ordering::Equal | Ordering::Less) => Ok(left),
            Some(Ordering::Greater) => Ok(right),
            None => Err(incomparable(&left, &right)),
        }
    }

    fn incomparable(left: &Value, right: &Value) -> anyhow::Error {
        anyhow!(
            "cannot compare {} and {}",
            left.static_type(),
            right.static_type()
        )
    }
}
