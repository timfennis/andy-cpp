use ndc_core::hash_map::{self, HashMapExt};
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
    pub fn intersect_assign(
        lhs: ndc_vm::value::MapValue,
        rhs: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        {
            let ndc_vm::value::Value::Object(ref obj) = lhs else {
                anyhow::bail!("&= requires a map on the left side");
            };
            let ndc_vm::value::Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("&= requires a map on the left side");
            };
            entries.borrow_mut().intersection(rhs);
        }
        Ok(lhs)
    }

    #[function(name = "|=")]
    pub fn union_assign(
        lhs: ndc_vm::value::MapValue,
        rhs: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        {
            let ndc_vm::value::Value::Object(ref obj) = lhs else {
                anyhow::bail!("|= requires a map on the left side");
            };
            let ndc_vm::value::Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("|= requires a map on the left side");
            };
            let mut m = entries.borrow_mut();
            for (key, value) in rhs {
                m.insert(key.clone(), value.clone());
            }
        }
        Ok(lhs)
    }

    #[function(name = "-=")]
    pub fn difference_assign(
        lhs: ndc_vm::value::MapValue,
        rhs: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        {
            let ndc_vm::value::Value::Object(ref obj) = lhs else {
                anyhow::bail!("-= requires a map on the left side");
            };
            let ndc_vm::value::Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("-= requires a map on the left side");
            };
            entries.borrow_mut().difference(rhs);
        }
        Ok(lhs)
    }

    #[function(name = "~=")]
    pub fn symmetric_difference_assign(
        lhs: ndc_vm::value::MapValue,
        rhs: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        {
            let ndc_vm::value::Value::Object(ref obj) = lhs else {
                anyhow::bail!("~= requires a map on the left side");
            };
            let ndc_vm::value::Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("~= requires a map on the left side");
            };
            let diff = hash_map::symmetric_difference(&*entries.borrow(), rhs);
            *entries.borrow_mut() = diff;
        }
        Ok(lhs)
    }

    /// Returns the union (elements that are in either `left` or `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left | right`
    #[function(alias = "|")]
    pub fn union(
        left: ndc_vm::value::MapValue,
        right: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        let ndc_vm::value::Value::Object(ref obj) = left else {
            anyhow::bail!("| requires a map on the left side");
        };
        let ndc_vm::value::Object::Map {
            ref entries,
            ref default,
        } = *obj.as_ref()
        else {
            anyhow::bail!("| requires a map on the left side");
        };
        let new_entries = hash_map::union(&*entries.borrow(), right);
        Ok(ndc_vm::value::Value::Object(Rc::new(
            ndc_vm::value::Object::map(new_entries, default.clone()),
        )))
    }

    /// Returns the intersection (elements that are in both `left` and `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left & right`.
    #[function(alias = "&")]
    pub fn intersection(
        left: ndc_vm::value::MapValue,
        right: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        let ndc_vm::value::Value::Object(ref obj) = left else {
            anyhow::bail!("& requires a map on the left side");
        };
        let ndc_vm::value::Object::Map {
            ref entries,
            ref default,
        } = *obj.as_ref()
        else {
            anyhow::bail!("& requires a map on the left side");
        };
        let new_entries = hash_map::intersection(&*entries.borrow(), right);
        Ok(ndc_vm::value::Value::Object(Rc::new(
            ndc_vm::value::Object::map(new_entries, default.clone()),
        )))
    }

    /// Returns the symmetric difference (elements that are either in `left` or `right` but not both) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left ~ right`.
    #[function(alias = "~")]
    pub fn symmetric_difference(
        left: ndc_vm::value::MapValue,
        right: &hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value>,
    ) -> anyhow::Result<ndc_vm::value::MapValue> {
        let ndc_vm::value::Value::Object(ref obj) = left else {
            anyhow::bail!("~ requires a map on the left side");
        };
        let ndc_vm::value::Object::Map {
            ref entries,
            ref default,
        } = *obj.as_ref()
        else {
            anyhow::bail!("~ requires a map on the left side");
        };
        let new_entries = hash_map::symmetric_difference(&*entries.borrow(), right);
        Ok(ndc_vm::value::Value::Object(Rc::new(
            ndc_vm::value::Object::map(new_entries, default.clone()),
        )))
    }

    /// Converts the given sequence to set.
    #[function(return_type = Map<_, ()>)]
    pub fn set(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        use ndc_core::hash_map::HashMap;
        use ndc_vm::value::{Object, Value};

        let entries: HashMap<Value, Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow::anyhow!("set requires a sequence"))?
            .map(|v| (v, Value::unit()))
            .collect();

        Ok(Value::Object(Rc::new(Object::map(entries, None))))
    }
}
