use ndc_macros::export_module;
use ndc_vm::value::{Object, Value};

#[export_module]
mod inner {
    use std::rc::Rc;

    /// Returns the documentation string for a function, or an empty string if none is available.
    #[function(signature = "<R>(Fn(*) -> R) -> String")]
    pub fn docs(value: Value) -> anyhow::Result<String> {
        match &value {
            Value::Object(obj) => match obj.as_ref() {
                Object::Function(f) => Ok(f.documentation().unwrap_or("").to_string()),
                _ => Err(anyhow::anyhow!(
                    "docs requires a function, got {}",
                    value.static_type()
                )),
            },
            _ => Err(anyhow::anyhow!(
                "docs requires a function, got {}",
                value.static_type()
            )),
        }
    }

    /// Returns the reference count for the value, if the value is not reference counted it will return 0.
    ///
    /// Note: this function does increase the ref count by 1
    #[function(signature = "<A>(A) -> Int")]
    pub fn ref_count(value: Value) -> usize {
        match value {
            Value::Object(rc) => Rc::strong_count(&rc),
            _ => 0,
        }
    }

    /// Creates a new instance of `Some`
    #[function(name = "Some", signature = "<T>(T) -> Option<T>")]
    pub fn some(value: Value) -> Value {
        Value::Object(Rc::new(Object::Some(value)))
    }

    /// Creates a new instance of `None`
    #[function(signature = "<T>() -> Option<T>")]
    pub fn none() -> Value {
        Value::None
    }

    /// Returns true if the argument is `Some`.
    #[function(signature = "<A>(A) -> Bool")]
    pub fn is_some(value: Value) -> bool {
        matches!(
            value,
            Value::Object(ref obj)
                if matches!(obj.as_ref(), Object::Some(_))
        )
    }

    /// Returns true if the argument is `None`.
    #[function(signature = "<A>(A) -> Bool")]
    pub fn is_none(value: Value) -> bool {
        matches!(value, Value::None)
    }

    /// Extracts the value from an Option or errors if it's either None or a non-Option type
    #[function(signature = "<T>(Option<T>) -> T")]
    pub fn unwrap(value: Value) -> anyhow::Result<Value> {
        match value {
            Value::Object(obj) => match obj.as_ref() {
                Object::Some(inner) => Ok(inner.clone()),
                _ => Err(anyhow::anyhow!("incorrect argument to unwrap")),
            },
            Value::None => Err(anyhow::anyhow!("option was none")),
            _ => Err(anyhow::anyhow!("incorrect argument to unwrap")),
        }
    }

    /// Extracts the value from an Option, returning `default` when it is `None`.
    #[function(signature = "<T>(Option<T>, T) -> T")]
    pub fn unwrap_or(value: Value, default: Value) -> anyhow::Result<Value> {
        match value {
            Value::Object(obj) => match obj.as_ref() {
                Object::Some(inner) => Ok(inner.clone()),
                _ => Err(anyhow::anyhow!("incorrect argument to unwrap_or")),
            },
            Value::None => Ok(default),
            _ => Err(anyhow::anyhow!("incorrect argument to unwrap_or")),
        }
    }

    /// Returns a shallow copy of the given value.
    #[function(signature = "<T>(T) -> T")]
    pub fn clone(value: Value) -> Value {
        value.shallow_clone()
    }

    /// Returns a deep copy of the given value, duplicating all nested structures.
    #[function(signature = "<T>(T) -> T")]
    pub fn deepcopy(value: Value) -> Value {
        value.deep_copy()
    }
}
