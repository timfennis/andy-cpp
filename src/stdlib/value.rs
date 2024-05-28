use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    pub fn clone(value: &Value) -> Value {
        match value {
            Value::Unit => Value::Unit,
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
            // TODO: is this implementation what we want or do we want a different type of handling
            Value::Sequence(Sequence::Iterator(iterator)) => {
                Value::Sequence(Sequence::Iterator(iterator.clone()))
            }
            Value::Function(f) => Value::from(f.borrow().to_owned()),
        }
    }
}
