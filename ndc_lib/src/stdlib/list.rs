#[ndc_macros::export_module]
mod inner {
    use crate::interpreter::iterator::mut_seq_to_iterator;
    use crate::interpreter::sequence::{ListRepr, Sequence};
    use crate::interpreter::value::Value;
    use itertools::Itertools;
    use std::rc::Rc;

    use anyhow::anyhow;

    /// Converts any sequence into a list
    #[function(return_type = Vec<Value>)]
    pub fn list(seq: &mut Sequence) -> Value {
        Value::list(mut_seq_to_iterator(seq).collect::<Vec<_>>())
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

    pub fn insert(list: &mut Vec<Value>, index: usize, elem: Value) -> anyhow::Result<()> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }
        list.insert(index, elem);
        Ok(())
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

    // A price we have to pay for the type system
    // #[function(name = "++")]
    // pub fn tup_concat(left: TupleRepr, mut right: TupleRepr) -> Value {
    //     match Rc::try_unwrap(left) {
    //         Ok(mut left) => {
    //             left.append(Rc::make_mut(&mut right));
    //             Value::tuple(left)
    //         }
    //         Err(left) => Value::tuple(
    //             left.iter()
    //                 .chain(right.iter())
    //                 .cloned()
    //                 .collect::<Vec<Value>>(),
    //         ),
    //     }
    // }

    #[function(name = "++")]
    pub fn list_concat(left: &mut ListRepr, right: &mut ListRepr) -> Value {
        if Rc::strong_count(left) == 1 {
            left.borrow_mut().extend_from_slice(&right.borrow());

            Value::Sequence(Sequence::List(left.clone()))
        } else {
            Value::list(
                left.borrow()
                    .iter()
                    .chain(right.borrow().iter())
                    .cloned()
                    .collect::<Vec<Value>>(),
            )
        }
    }

    #[function(name = "++=")]
    pub fn list_append_operator(left: &mut ListRepr, right: &mut ListRepr) {
        // The ++= operator has 3 implementation paths, this first one is the case where a list extends itself
        if Rc::ptr_eq(left, right) {
            left.borrow_mut().extend_from_within(..);
        } else if Rc::strong_count(right) == 1 {
            // The second path deals with a RHS that has an RC of one, in this case we can drain the RHS which should be faster than copying?
            left.borrow_mut().append(
                &mut right
                    .try_borrow_mut()
                    .expect("Failed to borrow_mut in `list_append_operator`"),
            );
            // The last path is if the RHS has an RC that's higher than 1, in this case we copy all the elements into the LHS
        } else {
            left.borrow_mut().extend_from_slice(
                &right
                    .try_borrow()
                    .expect("Failed to borrow in `list_append_operator`"),
            );
        }
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

    #[function(name = "pop_left?", return_type = Option<Value>)]
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

    /// Creates a copy of the list with its elements in reverse order
    #[function(return_type = Vec<Value>)]
    pub fn reversed(list: &[Value]) -> Value {
        Value::list(list.iter().rev().cloned().collect::<Vec<Value>>())
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
    pub fn split_off(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(Value::list(list.split_off(index)))
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

    #[function(return_type = Vec<(Value, Value)>)]
    pub fn cartesian_product(list_a: &[Value], list_b: &[Value]) -> Value {
        Value::list(
            list_a
                .iter()
                .cartesian_product(list_b)
                .map(|(a, b)| Value::tuple(vec![a.clone(), b.clone()]))
                .collect_vec(),
        )
    }
}
