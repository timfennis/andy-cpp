use crate::hash_map;
use crate::hash_map::HashMap;
use crate::interpreter::sequence::{DefaultMap, Sequence};
use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

#[andy_cpp_macros::export_module]
mod inner {

    // TODO: this function makes a copy when returning some kind of iterator would be better
    pub fn keys(map: &mut HashMap<Value, Value>) -> Value {
        map.keys().cloned().collect::<Vec<_>>().into()
    }

    // TODO: this function makes a copy when returning some kind of iterator would be better
    pub fn values(map: &mut HashMap<Value, Value>) -> Value {
        map.values().cloned().collect::<Vec<_>>().into()
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

    pub fn union(left: DefaultMap, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::union(left.0, right))),
            left.1,
        ))
    }

    pub fn intersection(left: DefaultMap, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::intersection(left.0, right))),
            left.1,
        ))
    }

    pub fn symmetric_difference(left: DefaultMap, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::symmetric_difference(left.0, right))),
            left.1,
        ))
    }

    pub fn set(seq: &Sequence) -> Value {
        let out: HashMap<Value, Value> = match seq {
            Sequence::String(rc) => rc
                .borrow()
                .chars()
                .map(|c| (c.into(), Value::Unit))
                .collect(),
            // TODO: we could change the implementation so that ref counts of 1 are consumed instead of copied
            Sequence::List(rc) => rc
                .borrow()
                .iter()
                .map(|v| (v.to_owned(), Value::Unit))
                .collect(),
            Sequence::Tuple(rc) => rc.iter().map(|v| (v.to_owned(), Value::Unit)).collect(),
            Sequence::Map(rc, _) => rc
                .borrow()
                .iter()
                .map(|(key, _value)| (key.to_owned(), Value::Unit))
                .collect(),
            Sequence::Iterator(rc) => {
                let mut iter = rc.borrow_mut();
                let mut out = HashMap::new();
                for item in iter.by_ref() {
                    out.insert(item, Value::Unit);
                }
                out
            }
        };

        Value::Sequence(Sequence::Map(Rc::new(RefCell::new(out)), None))
    }
}
