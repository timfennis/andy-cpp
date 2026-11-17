use ndc_vm::hash_map::HashMap;
use ndc_vm::value::{Object, Value};
use std::cell::RefCell;
use std::hash::Hash;
use std::rc::Rc;

/// What a map holds.
type Entries = HashMap<Value, Value>;

/// The entries and the default of a map.
fn map_parts(value: &Value) -> Option<(&RefCell<Entries>, &Option<Value>)> {
    let Value::Object(object) = value else {
        return None;
    };
    match object.as_ref() {
        Object::Map { entries, default } => Some((entries, default)),
        _ => None,
    }
}

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
    pub fn remove_map(left: Value, right: Value) -> anyhow::Result<()> {
        change_with(&left, &right, "remove", |entries, other| {
            entries.retain(|key, _| !other.contains_key(key));
        })
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
    pub fn intersect_assign(lhs: Value, rhs: Value) -> anyhow::Result<Value> {
        change_with(&lhs, &rhs, "&=", |entries, other| {
            entries.retain(|key, _| other.contains_key(key));
        })?;
        Ok(lhs)
    }

    /// Union-assign: adds all elements from the right map or set into the left.
    #[function(name = "|=", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn union_assign(lhs: Value, rhs: Value) -> anyhow::Result<Value> {
        change_with(&lhs, &rhs, "|=", |entries, other| {
            for (key, value) in other {
                entries.insert(key.clone(), value.clone());
            }
        })?;
        Ok(lhs)
    }

    /// Difference-assign: removes all elements from the left map or set that are present in the right.
    #[function(
        name = "-=",
        signature = "<K, V, W>(Map<K, V>, Map<K, W>) -> Map<K, V>"
    )]
    pub fn difference_assign(lhs: Value, rhs: Value) -> anyhow::Result<Value> {
        change_with(&lhs, &rhs, "-=", |entries, other| {
            entries.retain(|key, _| !other.contains_key(key));
        })?;
        Ok(lhs)
    }

    /// Symmetric-difference-assign: retains only elements present in exactly one of the two maps or sets.
    #[function(name = "~=", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn symmetric_difference_assign(lhs: Value, rhs: Value) -> anyhow::Result<Value> {
        change_with(&lhs, &rhs, "~=", |entries, other| {
            *entries = symmetric_difference(entries, other);
        })?;
        Ok(lhs)
    }

    /// Changes the entries of the map `target` by `change` with those of the
    /// map `other`, which may be the same map, as in `m &= m`: `change` then
    /// gets a copy of them, as a map can't be read while it's changed.
    fn change_with(
        target: &Value,
        other: &Value,
        operator: &str,
        change: impl FnOnce(&mut HashMap<Value, Value>, &HashMap<Value, Value>),
    ) -> anyhow::Result<()> {
        let (Some((entries, _)), Some((other, _))) = (map_parts(target), map_parts(other)) else {
            anyhow::bail!("{operator} requires two maps");
        };
        let copy;
        let borrowed;
        let other: &HashMap<Value, Value> = if std::ptr::eq(entries, other) {
            copy = other.borrow().clone();
            &copy
        } else {
            borrowed = other.borrow();
            &borrowed
        };
        change(&mut entries.borrow_mut(), other);
        Ok(())
    }

    /// Returns the union (elements that are in either `left` or `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left | right`.
    #[function(alias = "|", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn union(left: Value, right: &HashMap<Value, Value>) -> anyhow::Result<Value> {
        combine(&left, right, "|", union)
    }

    /// Returns the intersection (elements that are in both `left` and `right`) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left & right`.
    #[function(
        alias = "&",
        signature = "<K, V, W>(Map<K, V>, Map<K, W>) -> Map<K, V>"
    )]
    pub fn intersection(left: Value, right: &HashMap<Value, Value>) -> anyhow::Result<Value> {
        combine(&left, right, "&", intersection)
    }

    /// Returns the symmetric difference (elements that are either in `left` or `right` but not both) of two maps or sets.
    ///
    /// This is the same as evaluating the expression `left ~ right`.
    #[function(alias = "~", signature = "<K, V>(Map<K, V>, Map<K, V>) -> Map<K, V>")]
    pub fn symmetric_difference(
        left: Value,
        right: &HashMap<Value, Value>,
    ) -> anyhow::Result<Value> {
        combine(&left, right, "~", symmetric_difference)
    }

    /// A new map of what `combine` makes of the entries of `left` and `right`,
    /// with the default of `left`.
    fn combine(
        left: &Value,
        right: &HashMap<Value, Value>,
        operator: &str,
        combine: fn(&Entries, &Entries) -> Entries,
    ) -> anyhow::Result<Value> {
        let Some((entries, default)) = map_parts(left) else {
            anyhow::bail!("{operator} requires a map on the left side");
        };
        let entries = combine(&entries.borrow(), right);
        Ok(Value::Object(Rc::new(Object::map(
            entries,
            default.clone(),
        ))))
    }

    /// Converts the given sequence to set.
    #[function(signature = "<T>(Iterable<T>) -> Map<T, ()>")]
    pub fn set(seq: Value) -> anyhow::Result<Value> {
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
