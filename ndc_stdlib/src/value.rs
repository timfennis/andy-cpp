use ndc_macros::export_module;

#[export_module]
mod inner {
    use ndc_vm::value::{Object as VmObject, Value as VmValue};
    use std::rc::Rc;

    /// Returns the documentation string for a function, or an empty string if none is available.
    pub fn docs(value: ndc_vm::value::Value) -> anyhow::Result<String> {
        match &value {
            VmValue::Object(obj) => match obj.as_ref() {
                VmObject::Function(f) => Ok(f.documentation().unwrap_or("").to_string()),
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
    pub fn ref_count(value: ndc_vm::value::Value) -> usize {
        match value {
            ndc_vm::value::Value::Object(rc) => Rc::strong_count(&rc),
            _ => 0,
        }
    }

    /// Creates a new instance of `Some`
    #[function(name = "Some", return_type = Option<Value>)]
    pub fn some(value: ndc_vm::value::Value) -> ndc_vm::value::Value {
        VmValue::Object(Rc::new(VmObject::Some(value)))
    }

    /// Creates a new instance of `None`
    #[function(return_type = Option<_>)]
    pub fn none() -> ndc_vm::value::Value {
        VmValue::None
    }

    /// Returns true if the argument is `Some`.
    pub fn is_some(value: ndc_vm::value::Value) -> bool {
        matches!(
            value,
            ndc_vm::value::Value::Object(ref obj)
                if matches!(obj.as_ref(), ndc_vm::value::Object::Some(_))
        )
    }

    /// Returns true if the argument is `None`.
    pub fn is_none(value: ndc_vm::value::Value) -> bool {
        matches!(value, ndc_vm::value::Value::None)
    }

    /// Extracts the value from an Option or errors if it's either None or a non-Option type
    pub fn unwrap(value: ndc_vm::value::Value) -> anyhow::Result<ndc_vm::value::Value> {
        match value {
            VmValue::Object(obj) => match obj.as_ref() {
                VmObject::Some(inner) => Ok(inner.clone()),
                _ => Err(anyhow::anyhow!("incorrect argument to unwrap")),
            },
            VmValue::None => Err(anyhow::anyhow!("option was none")),
            _ => Err(anyhow::anyhow!("incorrect argument to unwrap")),
        }
    }

    /// Returns a shallow copy of the given value.
    pub fn clone(value: ndc_vm::value::Value) -> ndc_vm::value::Value {
        value.shallow_clone()
    }

    /// Returns a deep copy of the given value, duplicating all nested structures.
    pub fn deepcopy(value: ndc_vm::value::Value) -> ndc_vm::value::Value {
        value.deep_copy()
    }
}
