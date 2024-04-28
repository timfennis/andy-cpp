#[andy_cpp_macros::export_module]
mod inner {
    use crate::hashmap;
    use crate::hashmap::HashMap;
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::interpreter::value::{Sequence, Value};

    // TODO: this function makes a copy when returning some kind of iterator would be better
    pub fn keys(map: &HashMap<Value, Value>) -> Value {
        map.keys().cloned().collect::<Vec<_>>().into()
    }
    pub fn remove(map: &mut HashMap<Value, Value>, key: &Value) {
        map.remove(key);
    }

    #[function(name = "insert")]
    pub fn insert_map(map: &mut HashMap<Value, Value>, key: Value, value: Value) {
        map.insert(key, value);
    }
    #[function(name = "insert")]
    pub fn insert_set(map: &mut HashMap<Value, Value>, key: Value) {
        map.insert(key, Value::Unit);
    }

    pub fn union(left: &HashMap<Value, Value>, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Dictionary(Rc::new(RefCell::new(hashmap::union(
            left, right,
        )))))
    }

    pub fn intersection(left: &HashMap<Value, Value>, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Dictionary(Rc::new(RefCell::new(
            hashmap::intersection(left, right),
        ))))
    }

    pub fn symmetric_difference(
        left: &HashMap<Value, Value>,
        right: &HashMap<Value, Value>,
    ) -> Value {
        Value::Sequence(Sequence::Dictionary(Rc::new(RefCell::new(
            hashmap::symmetric_difference(left, right),
        ))))
    }
}
