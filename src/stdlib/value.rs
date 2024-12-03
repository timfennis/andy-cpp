use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[function(name = "Some")] // <-- fake type constructor
    pub fn some(value: Value) -> Value {
        Value::Option(Some(Box::new(value)))
    }

    pub fn none() -> Value {
        Value::Option(None)
    }

    pub fn is_some(value: &Value) -> Value {
        Value::Bool(matches!(value, Value::Option(Some(_))))
    }

    pub fn is_none(value: &Value) -> Value {
        Value::Bool(matches!(value, Value::Option(None)))
    }

    // TODO: this signature should be Option
    pub fn unwrap(value: Value) -> anyhow::Result<Value> {
        match value {
            Value::Option(Some(val)) => Ok(*val),
            Value::Option(None) => Err(anyhow::anyhow!("option was none")),
            _ => Err(anyhow::anyhow!(
                "incorrect argument to unwrap (temporary error)"
            )),
        }
    }

    pub fn clone(value: &Value) -> Value {
        match value {
            Value::Option(o) => Value::Option(o.clone()),
            number @ Value::Number(_) => number.clone(),
            Value::Bool(b) => Value::Bool(*b),
            Value::Sequence(Sequence::String(string)) => Value::from(string.borrow().to_owned()),
            Value::Sequence(Sequence::List(list)) => Value::from(list.borrow().to_owned()),
            Value::Sequence(Sequence::Map(map, default)) => Value::Sequence(Sequence::Map(
                Rc::new(RefCell::new(map.borrow().clone())),
                default.to_owned(),
            )),
            Value::Sequence(Sequence::Tuple(tuple)) => {
                Value::Sequence(Sequence::Tuple(tuple.clone()))
            }
            Value::Sequence(Sequence::Iterator(iterator)) => {
                Value::Sequence(Sequence::Iterator(iterator.clone()))
            }
            Value::Function(f) => Value::from(f.borrow().to_owned()),
        }
    }
}
