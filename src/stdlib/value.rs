use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::value::Sequence;
    use crate::interpreter::value::Value;

    pub fn clone(value: &Value) -> Value {
        match value {
            Value::Unit => Value::Unit,
            number @ Value::Number(_) => number.clone(),
            Value::Bool(b) => Value::Bool(*b),
            Value::Sequence(Sequence::String(string)) => Value::from(string.borrow().to_owned()),
            Value::Sequence(Sequence::List(list)) => Value::from(list.borrow().to_owned()),
            Value::Sequence(Sequence::Dictionary(_dict)) => todo!("implement this"), // Value::from(dict.borrow().to_owned()),
            Value::Sequence(Sequence::Tuple(tuple)) => {
                Value::Sequence(Sequence::Tuple(tuple.clone()))
            }
            Value::Function(f) => Value::from(f.borrow().to_owned()),
        }
    }
}
