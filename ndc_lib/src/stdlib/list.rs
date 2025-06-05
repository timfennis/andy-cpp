#[ndc_macros::export_module]
mod inner {
    use crate::interpreter::iterator::mut_seq_to_iterator;
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use itertools::Itertools;
    use std::rc::Rc;

    use anyhow::anyhow;

    /// Converts any sequence into a list
    pub fn list(seq: &mut Sequence) -> Vec<Value> {
        mut_seq_to_iterator(seq).collect::<Vec<_>>()
    }

    pub fn contains(list: &[Value], elem: &Value) -> bool {
        list.contains(elem)
    }

    pub fn contains_subsequence(list: &[Value], subsequence: &[Value]) -> bool {
        list.windows(subsequence.len()).contains(&subsequence)
    }

    pub fn find_subsequence(list: &[Value], subsequence: &[Value]) -> Value {
        let result = list
            .windows(subsequence.len())
            .enumerate()
            .find(|(_, seq)| *seq == subsequence)
            .map(|(idx, _)| idx);
        if let Some(result) = result {
            Value::from(result)
        } else {
            Value::unit()
        }
    }

    pub fn insert(list: &mut Vec<Value>, index: usize, elem: Value) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }
        list.insert(index, elem);
        Ok(Value::unit())
    }

    /// Removes and returns the element at position `index` within the list, shifting all elements after it to the left.
    pub fn remove(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(list.remove(index))
    }

    /// Removes all instances of `element` from `list`
    pub fn remove_element(list: &mut Vec<Value>, element: &Value) {
        list.retain(|cur| cur != element);
    }

    /// Appends `elem` to the back of `list`
    pub fn push(list: &mut Vec<Value>, elem: Value) {
        list.push(elem);
    }

    /// Moves elements from `other` to `list` leaving `other` empty
    pub fn append(list: &mut Vec<Value>, other: &mut Vec<Value>) {
        list.append(other);
    }

    // TODO: ISSUE: this implementation has pretty terrible performance compared to what we had before
    #[function(name = "++")]
    pub fn op_concat(left: &[Value], right: &[Value]) -> Vec<Value> {
        // TODO: ISSUE: This function always returns a list, even when it's concatenating two tuples
        left.iter().chain(right.iter()).cloned().collect()
    }

    /// Copies elements from `other` to `list` not touching `other`.
    pub fn extend(list: &mut Vec<Value>, other: &[Value]) {
        list.extend_from_slice(other);
    }

    /// Extends this `list` with elements from `iter` leaving the iterator empty
    #[function(name = "extend")]
    pub fn extend_from_iter(list: &mut Vec<Value>, iter: impl Iterator<Item = Value>) {
        list.extend(iter);
    }

    /// Removes the last element from a list and returns it, or `Unit` if it is empty
    #[function(name = "pop?")]
    pub fn maybe_pop(list: &mut Vec<Value>) -> Value {
        list.pop().map_or_else(Value::none, Value::some)
    }

    pub fn pop(list: &mut Vec<Value>) -> Value {
        list.pop().unwrap_or(Value::unit())
    }

    #[function(name = "pop_left?")]
    pub fn maybe_pop_left(list: &mut Vec<Value>) -> Value {
        if list.is_empty() {
            return Value::none();
        }

        Value::some(list.remove(0))
    }

    pub fn pop_left(list: &mut Vec<Value>) -> Value {
        if list.is_empty() {
            return Value::unit();
        }

        list.remove(0)
    }

    /// Creates a copy of the list with it's elements in reverse order
    pub fn reversed(list: &[Value]) -> Vec<Value> {
        list.iter().rev().cloned().collect::<Vec<Value>>()
    }

    /// Removes all values from the list
    pub fn clear(list: &mut Vec<Value>) {
        list.clear();
    }

    /// Returns `true` if the list contains no elements
    pub fn is_empty(list: &[Value]) -> bool {
        list.is_empty()
    }

    /// Splits the collection into two at the given index.
    pub fn split_off(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Vec<Value>> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(list.split_off(index))
    }

    /// Shortens the list, keeping the first `len` elements and dropping the rest.
    /// If `len` is greater or equal to the length of the list, nothing happens.
    pub fn truncate(list: &mut Vec<Value>, len: usize) {
        list.truncate(len);
    }

    /// Returns a copy of the first element of the list or results in an error if the list is empty.
    pub fn first(list: &[Value]) -> anyhow::Result<Value> {
        list.first()
            .cloned()
            .ok_or_else(|| anyhow!("collection is empty"))
    }

    /// Returns a copy of the first element or `unit` if the list is empty.
    #[function(name = "first?")]
    pub fn maybe_first(list: &[Value]) -> Value {
        list.first().cloned().map_or_else(Value::none, Value::some)
    }

    /// Returns a copy of the last element of the list or results in an error if the list is empty.
    pub fn last(list: &[Value]) -> anyhow::Result<Value> {
        list.last()
            .cloned()
            .ok_or_else(|| anyhow!("the list is empty"))
    }

    /// Returns a copy of the last element or `unit` if the list is empty.
    #[function(name = "last?")]
    pub fn maybe_last(list: &[Value]) -> Value {
        list.last().cloned().map_or_else(Value::none, Value::some)
    }

    pub fn cartesian_product(list_a: &[Value], list_b: &[Value]) -> Vec<Value> {
        list_a
            .iter()
            .cartesian_product(list_b)
            .map(|(a, b)| Value::Sequence(Sequence::Tuple(Rc::new(vec![a.clone(), b.clone()]))))
            .collect_vec()
    }
}
