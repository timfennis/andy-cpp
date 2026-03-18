use ndc_interpreter::hash_map;
use ndc_interpreter::hash_map::HashMapExt;
use ndc_interpreter::sequence::{DefaultMap, MapRepr, Sequence};
use ndc_interpreter::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

#[ndc_macros::export_module]
mod inner {

    /// Returns a list of all the keys in the map or set.
    ///
    /// Note that for a set this will return the values in the set.
    #[function(return_type = Vec<_>)]
    pub fn keys(
        map: &mut hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> ndc_vm::value::Value {
        ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::list(
            map.keys().cloned().collect::<Vec<_>>(),
        )))
    }

    /// Returns a list of all the values in the map.
    ///
    /// Note that for sets this will return a list of unit types, you should use keys if you want the values in the set.
    #[function(return_type = Vec<_>)]
    pub fn values(
        map: &mut hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> ndc_vm::value::Value {
        ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::list(
            map.values().cloned().collect::<Vec<_>>(),
        )))
    }

    /// Removes a key from the map or a value from a set.
    pub fn remove(
        map: &mut hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
        key: ndc_vm::value::Value,
    ) {
        map.remove(&key);
    }

    /// Removes all keys from the `left` map/set that are present in the `right` map/set.
    #[function(name = "remove")]
    pub fn remove_map(
        left: &mut hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
        right: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) {
        for (key, _) in right {
            left.remove(key);
        }
    }

    /// Insert a value into a map.
    #[function(name = "insert")]
    pub fn insert_map(
        map: &mut hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
        key: ndc_vm::value::Value,
        value: ndc_vm::value::Value,
    ) {
        map.insert(key, value);
    }

    /// Inserts a value into a set.
    #[function(name = "insert")]
    pub fn insert_set(
        map: &mut hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
        key: ndc_vm::value::Value,
    ) {
        map.insert(key, ndc_vm::value::Value::unit());
    }

    /// Returns true if the map or set contains no elements.
    pub fn is_empty(map: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>) -> bool {
        map.is_empty()
    }

    #[function(name = "&=")]
    pub fn intersect_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) -> Value {
        let left_map: &mut hash_map::HashMap<Value, Value> = &mut lhs
            .try_borrow_mut()
            .expect("Failed to mutably borrow the lhs of &= operator");

        left_map.intersection(
            &*rhs
                .try_borrow()
                .expect("Failed borrow the rhs of &= operator"),
        );
        Value::Sequence(Sequence::Map(Rc::clone(lhs), None))
    }

    #[function(name = "|=")]
    pub fn union_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) -> Value {
        let left_map: &mut hash_map::HashMap<Value, Value> = &mut lhs.borrow_mut();

        if Rc::strong_count(rhs) == 1 {
            let rhs = std::mem::take(&mut *rhs.borrow_mut());
            left_map.union(rhs);
        } else {
            let right = rhs.borrow();
            for (key, value) in right.iter() {
                left_map.insert(key.clone(), value.clone());
            }
        }
        Value::Sequence(Sequence::Map(Rc::clone(lhs), None))
    }

    #[function(name = "-=")]
    pub fn difference_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) -> Value {
        let left_map: &mut hash_map::HashMap<Value, Value> = &mut lhs.borrow_mut();
        left_map.difference(&*rhs.borrow());
        Value::Sequence(Sequence::Map(Rc::clone(lhs), None))
    }

    #[function(name = "~=")]
    pub fn symmetric_difference_assign(lhs: &mut MapRepr, rhs: &mut MapRepr) -> Value {
        let diff = hash_map::symmetric_difference(
            &*lhs
                .try_borrow()
                .expect("Failed to borrow the lhs of ~= operator"),
            &*rhs
                .try_borrow()
                .expect("Failed borrow the rhs of ~= operator"),
        );
        *lhs.borrow_mut() = diff;
        Value::Sequence(Sequence::Map(Rc::clone(lhs), None))
    }

    /// Returns the union (elements that are in either `left` or `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left | right`
    #[function(alias = "|", return_type = DefaultMap<'_>)]
    pub fn union(left: DefaultMap<'_>, right: &hash_map::HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::union(left.0, right))),
            left.1,
        ))
    }

    /// Returns the intersection (elements that are in both `left and `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left & right`.
    #[function(alias = "&", return_type = DefaultMap<'_>)]
    pub fn intersection(left: DefaultMap<'_>, right: &hash_map::HashMap<Value, Value>) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::intersection(left.0, right))),
            left.1,
        ))
    }

    /// Returns the symmetric difference (elements that are either in `left` or `right` but not both) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left ~ right`.
    #[function(alias = "~", return_type = DefaultMap<'_>)]
    pub fn symmetric_difference(
        left: DefaultMap<'_>,
        right: &hash_map::HashMap<Value, Value>,
    ) -> Value {
        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(hash_map::symmetric_difference(left.0, right))),
            left.1,
        ))
    }

    /// Converts the given sequence to set.
    pub fn set(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        use ndc_core::hash_map::HashMap;
        use ndc_vm::value::{Object, Value};
        use std::rc::Rc;

        let entries: HashMap<Value, Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow::anyhow!("set requires a sequence"))?
            .map(|v| (v, Value::unit()))
            .collect();

        Ok(Value::Object(Rc::new(Object::map(entries, None))))
    }
}
