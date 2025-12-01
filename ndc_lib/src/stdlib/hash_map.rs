use crate::hash_map;
use crate::hash_map::HashMap;
use crate::hash_map::HashMapExt;
use crate::interpreter::sequence::{DefaultMap, MapRepr, Sequence};
use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

#[ndc_macros::export_module]
mod inner {

    /// Returns a list of all the keys in the map or set.
    ///
    /// Note that for a set this will return the values in the set.
    pub fn keys(map: &mut HashMap<Value, Value>) -> Value {
        Value::list(map.keys().cloned().collect::<Vec<_>>())
    }

    /// Returns a list of all the values in the map.
    ///
    /// Note that for sets this will return a list of unit types, you should use keys if you want the values in the set.
    pub fn values(map: &mut HashMap<Value, Value>) -> Value {
        Value::list(map.values().cloned().collect::<Vec<_>>())
    }

    /// Removes a key from the map or a value from a set.
    pub fn remove(map: &mut HashMap<Value, Value>, key: &Value) {
        map.remove(key);
    }

    /// Removes all keys from the `left` map/set that are present in the `right` map/set.
    #[function(name = "remove")]
    pub fn remove_map(left: &mut HashMap<Value, Value>, right: &HashMap<Value, Value>) {
        for (key, _) in right {
            left.remove(key);
        }
    }

    /// Insert a value into a map.
    #[function(name = "insert")]
    pub fn insert_map(map: &mut HashMap<Value, Value>, key: Value, value: Value) {
        map.insert(key, value);
    }

    /// Inserts a value into a set.
    #[function(name = "insert")]
    pub fn insert_set(map: &mut HashMap<Value, Value>, key: Value) {
        map.insert(key, Value::unit());
    }

    #[function(name = "&=")]
    pub fn intersect_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) {
        let left_map: &mut HashMap<Value, Value> = &mut lhs
            .try_borrow_mut()
            .expect("Failed to mutably borrow the lhs of &= operator");

        left_map.intersection(
            &*rhs
                .try_borrow()
                .expect("Failed borrow the rhs of &= operator"),
        );
    }

    #[function(name = "|=")]
    pub fn union_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) {
        let left_map: &mut HashMap<Value, Value> = &mut lhs.borrow_mut();

        if Rc::strong_count(rhs) == 1 {
            // Take ownership
            let rhs = std::mem::take(&mut *rhs.borrow_mut());
            left_map.union(rhs);
        } else {
            let right = rhs.borrow();
            for (key, value) in right.iter() {
                left_map.insert(key.clone(), value.clone());
            }
        }
    }

    #[function(name = "-=")]
    pub fn difference_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) {
        let left_map: &mut HashMap<Value, Value> = &mut lhs.borrow_mut();

        left_map.difference(&*rhs.borrow());
    }

    #[function(name = "~=")]
    pub fn symmetric_difference_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) {
        // TODO: is this implementation optimal enough?
        let diff = hash_map::symmetric_difference(
            &*lhs
                .try_borrow()
                .expect("Failed to borrow the lhs of ~= operator"),
            &*rhs
                .try_borrow()
                .expect("Failed borrow the rhs of ~= operator"),
        );

        *lhs.borrow_mut() = diff;
    }

    /// Returns the union (elements that are in either `left` or `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left | right`
    #[function(alias = "|")]
    pub fn union(left: DefaultMap<'_>, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::union(left.0, right))),
            left.1,
        ))
    }

    /// Returns the intersection (elements that are in both `left and `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left & right`.
    #[function(alias = "&")]
    pub fn intersection(left: DefaultMap<'_>, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::intersection(left.0, right))),
            left.1,
        ))
    }

    /// Returns the symmetric difference (elements that are either in `left` or `right` but not both) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left ~ right`.
    #[function(alias = "~")]
    pub fn symmetric_difference(left: DefaultMap<'_>, right: &HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::symmetric_difference(left.0, right))),
            left.1,
        ))
    }

    /// Converts the given sequence to set.
    pub fn set(seq: &mut Sequence) -> Value {
        let out: HashMap<Value, Value> = match seq {
            Sequence::String(rc) => rc
                .borrow()
                .chars()
                .map(|c| (c.into(), Value::unit()))
                .collect(),
            Sequence::List(rc) => rc
                .borrow()
                .iter()
                .map(|v| (v.to_owned(), Value::unit()))
                .collect(),
            Sequence::Tuple(rc) => rc.iter().map(|v| (v.to_owned(), Value::unit())).collect(),
            Sequence::Map(rc, _) => rc
                .borrow()
                .iter()
                .map(|(key, _value)| (key.to_owned(), Value::unit()))
                .collect(),
            Sequence::Iterator(rc) => {
                let mut iter = rc.borrow_mut();
                let mut out = HashMap::new();
                for item in iter.by_ref() {
                    out.insert(item, Value::unit());
                }
                out
            }
            Sequence::MaxHeap(h) => h
                .borrow()
                .iter()
                .map(|value| (value.0.clone(), Value::unit()))
                .collect(),
            Sequence::MinHeap(h) => h
                .borrow()
                .iter()
                .map(|value| (value.0.0.clone(), Value::unit()))
                .collect(),
            Sequence::Deque(rc) => rc
                .borrow()
                .iter()
                .map(|v| (v.to_owned(), Value::unit()))
                .collect(),
        };

        Value::Sequence(Sequence::Map(Rc::new(RefCell::new(out)), None))
    }
}
