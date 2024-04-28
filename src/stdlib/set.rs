#[andy_cpp_macros::export_module]
mod inner {
    use crate::hashmap::HashMap;
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::interpreter::value::{Sequence, Value};

    pub fn insert(set: &mut HashMap<Value, Value>, value: Value) -> Value {
        set.insert(value, Value::Unit);
        Value::Unit
    }

    pub fn union(left: &HashMap<Value, Value>, right: &HashMap<Value, Value>) -> Value {
        let out: HashMap<Value, Value> = left
            .iter()
            .chain(right.iter())
            .map(|x| (x.0.clone(), x.1.clone()))
            .collect();

        Value::Sequence(Sequence::Dictionary(Rc::new(RefCell::new(out))))
    }

    pub fn intersection(left: &HashMap<Value, Value>, right: &HashMap<Value, Value>) -> Value {
        let out: HashMap<Value, Value> = left
            .iter()
            .filter_map(|(key, value)| {
                if right.contains_key(key) {
                    Some((key.clone(), value.clone()))
                } else {
                    None
                }
            })
            .collect();
        Value::Sequence(Sequence::Dictionary(Rc::new(RefCell::new(out))))
    }

    pub fn symmetric_difference(
        left: &HashMap<Value, Value>,
        right: &HashMap<Value, Value>,
    ) -> Value {
        let out: HashMap<Value, Value> = left
            .iter()
            .chain(right.iter())
            .filter_map(|(key, value)| {
                if !right.contains_key(key) || !left.contains_key(key) {
                    Some((key.clone(), value.clone()))
                } else {
                    None
                }
            })
            .collect();
        Value::Sequence(Sequence::Dictionary(Rc::new(RefCell::new(out))))
    }
}
