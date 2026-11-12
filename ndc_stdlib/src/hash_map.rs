use ndc_vm::hash_map::HashMap;
use ndc_vm::value::{MapValue, Object, SeqValue, Value};
use std::hash::Hash;
use std::rc::Rc;

#[ndc_macros::export_module]
mod inner {

    /// Returns a list of all the keys in the map or set.
    ///
    /// Note that for a set this will return the values in the set.
    #[function(signature = "<K, V>(Map<K, V>) -> List<K>")]
    pub fn keys(map: &mut HashMap<Value, Value>) -> Value {
        Value::Object(Rc::new(Object::list(
            map.keys().cloned().collect::<Vec<_>>(),
        )))
    }

    /// Returns a list of all the values in the map.
    ///
    /// Note that for sets this will return a list of unit types, you should use keys if you want the values in the set.
    #[function(signature = "<K, V>(Map<K, V>) -> List<V>")]
    pub fn values(map: &mut HashMap<Value, Value>) -> Value {
        Value::Object(Rc::new(Object::list(
            map.values().cloned().collect::<Vec<_>>(),
        )))
    }

    /// Removes a key from the map or a value from a set.
    #[function(signature = "<K, V>(Map<K, V>, K)")]
    pub fn remove(map: &mut HashMap<Value, Value>, key: Value) {
        map.remove(&key);
    }

    /// Removes all keys from the `left` map/set that are present in the `right` map/set.
    #[function(name = "remove", signature = "<K, V, W>(Map<K, V>, Map<K, W>)")]
    pub fn remove_map(left: &mut HashMap<Value, Value>, right: &HashMap<Value, Value>) {
        for (key, _) in right {
            left.remove(key);
        }
    }

    /// Insert a value into a map.
    #[function(name = "insert", signature = "<K, V>(Map<K, V>, K, V)")]
    pub fn insert_map(map: &mut HashMap<Value, Value>, key: Value, value: Value) {
        map.insert(key, value);
    }

    /// Inserts a value into a set.
    #[function(name = "insert", signature = "<K>(Map<K, ()>, K)")]
    pub fn insert_set(map: &mut HashMap<Value, Value>, key: Value) {
        map.insert(key, Value::unit());
    }

    /// Returns true if the map or set contains no elements.
    #[function(signature = "<K, V>(Map<K, V>) -> Bool")]
    pub fn is_empty(map: &HashMap<Value, Value>) -> bool {
        map.is_empty()
    }

    /// Intersection-assign: retains only elements present in both maps or sets.
    #[function(
        name = "&=",
        signature = "<K, V, W>(Map<K, V>, Map<K, W>) -> Map<K, V>"
    )]
    pub fn intersect_assign(
        lhs: MapValue,
        rhs: &HashMap<Value, Value>,
    ) -> anyhow::Result<MapValue> {
        {
            let Value::Object(ref obj) = lhs else {
                anyhow::bail!("&= requires a map on the left side");
            };
            let Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("&= requires a map on the left side");
            };
            entries.borrow_mut().retain(|key, _| rhs.contains_key(key));
        }
        Ok(lhs)
    }

    /// Union-assign: adds all elements from the right map or set into the left.
    #[function(name = "|=", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn union_assign(lhs: MapValue, rhs: &HashMap<Value, Value>) -> anyhow::Result<MapValue> {
        {
            let Value::Object(ref obj) = lhs else {
                anyhow::bail!("|= requires a map on the left side");
            };
            let Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("|= requires a map on the left side");
            };
            let mut m = entries.borrow_mut();
            for (key, value) in rhs {
                m.insert(key.clone(), value.clone());
            }
        }
        Ok(lhs)
    }

    /// Difference-assign: removes all elements from the left map or set that are present in the right.
    #[function(
        name = "-=",
        signature = "<K, V, W>(Map<K, V>, Map<K, W>) -> Map<K, V>"
    )]
    pub fn difference_assign(
        lhs: MapValue,
        rhs: &HashMap<Value, Value>,
    ) -> anyhow::Result<MapValue> {
        {
            let Value::Object(ref obj) = lhs else {
                anyhow::bail!("-= requires a map on the left side");
            };
            let Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("-= requires a map on the left side");
            };
            entries.borrow_mut().retain(|key, _| !rhs.contains_key(key));
        }
        Ok(lhs)
    }

    /// Symmetric-difference-assign: retains only elements present in exactly one of the two maps or sets.
    #[function(name = "~=", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn symmetric_difference_assign(
        lhs: MapValue,
        rhs: &HashMap<Value, Value>,
    ) -> anyhow::Result<MapValue> {
        {
            let Value::Object(ref obj) = lhs else {
                anyhow::bail!("~= requires a map on the left side");
            };
            let Object::Map { ref entries, .. } = *obj.as_ref() else {
                anyhow::bail!("~= requires a map on the left side");
            };
            let diff = symmetric_difference(&*entries.borrow(), rhs);
            *entries.borrow_mut() = diff;
        }
        Ok(lhs)
    }

    /// Returns the union (elements that are in either `left` or `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left | right`.
    #[function(alias = "|", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn union(left: MapValue, right: &HashMap<Value, Value>) -> anyhow::Result<MapValue> {
        let Value::Object(ref obj) = left else {
            anyhow::bail!("| requires a map on the left side");
        };
        let Object::Map {
            ref entries,
            ref default,
        } = *obj.as_ref()
        else {
            anyhow::bail!("| requires a map on the left side");
        };
        let new_entries = union(&*entries.borrow(), right);
        Ok(Value::Object(Rc::new(Object::map(
            new_entries,
            default.clone(),
        ))))
    }

    /// Returns the intersection (elements that are in both `left` and `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left & right`.
    #[function(
        alias = "&",
        signature = "<K, V, W>(Map<K, V>, Map<K, W>) -> Map<K, V>"
    )]
    pub fn intersection(left: MapValue, right: &HashMap<Value, Value>) -> anyhow::Result<MapValue> {
        let Value::Object(ref obj) = left else {
            anyhow::bail!("& requires a map on the left side");
        };
        let Object::Map {
            ref entries,
            ref default,
        } = *obj.as_ref()
        else {
            anyhow::bail!("& requires a map on the left side");
        };
        let new_entries = intersection(&*entries.borrow(), right);
        Ok(Value::Object(Rc::new(Object::map(
            new_entries,
            default.clone(),
        ))))
    }

    /// Returns the symmetric difference (elements that are either in `left` or `right` but not both) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left ~ right`.
    #[function(alias = "~", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn symmetric_difference(
        left: MapValue,
        right: &HashMap<Value, Value>,
    ) -> anyhow::Result<MapValue> {
        let Value::Object(ref obj) = left else {
            anyhow::bail!("~ requires a map on the left side");
        };
        let Object::Map {
            ref entries,
            ref default,
        } = *obj.as_ref()
        else {
            anyhow::bail!("~ requires a map on the left side");
        };
        let new_entries = symmetric_difference(&*entries.borrow(), right);
        Ok(Value::Object(Rc::new(Object::map(
            new_entries,
            default.clone(),
        ))))
    }

    /// Converts the given sequence to set.
    #[function(signature = "<T>(Sequence<T>) -> Map<T, ()>")]
    pub fn set(seq: SeqValue) -> anyhow::Result<Value> {
        use ndc_vm::hash_map::HashMap;

        let entries: HashMap<Value, Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow::anyhow!("set requires a sequence"))?
            .map(|v| (v, Value::unit()))
            .collect();

        Ok(Value::Object(Rc::new(Object::map(entries, None))))
    }
}

fn union<K, V>(left: &HashMap<K, V>, right: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    left.iter()
        .chain(right.iter())
        .map(|(key, value)| (key.to_owned(), value.to_owned()))
        .collect()
}

fn intersection<K, V>(left: &HashMap<K, V>, right: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    left.iter()
        .filter_map(|(key, value)| {
            if right.contains_key(key) {
                Some((key.clone(), value.clone()))
            } else {
                None
            }
        })
        .collect()
}

fn symmetric_difference<K, V>(left: &HashMap<K, V>, right: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    left.iter()
        .chain(right.iter())
        .filter_map(|(key, value)| {
            if !right.contains_key(key) || !left.contains_key(key) {
                Some((key.clone(), value.clone()))
            } else {
                None
            }
        })
        .collect()
}
