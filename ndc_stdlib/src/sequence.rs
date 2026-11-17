//! Functions over sequences.
//!
//! A function that returns groups of elements returns each group as a tuple when its signature
//! alone fixes the group's size, and as a list when the size depends on a value only known at
//! runtime:
//!
//! - `[1, 2, 3].pairwise()` gives `[(1, 2), (2, 3)]`: always pairs, typed `List<(T, T)>`.
//! - `"ab".enumerate()` gives `(0, "a")`, `(1, "b")`: different element types need a tuple.
//! - `[1, 2, 3].windows(2)` gives `[[1, 2], [2, 3]]`: the size comes from the `Int` argument.
//! - `[1, 2, 3].prefixes()` gives `[[1], [1, 2], [1, 2, 3]]`: the sizes come from the input.
//!
//! A tuple type records its length and the type of each element, so the type checker can reject
//! `let (a, b, c) = xs.pairwise()[0]`. A `List<T>` records neither, which is all it can promise
//! when the size isn't fixed.

#![allow(clippy::ptr_arg)]

use anyhow::anyhow;
use itertools::Itertools;
use ndc_macros::export_module;
use ndc_vm::VmCallable;
use ndc_vm::value::{Object, Value};
use ndc_vm::{CombinationsIter, EnumerateIter, TakeIter};
use std::cmp::Ordering;

fn try_sort_by<E>(
    v: &mut [Value],
    cmp: impl Fn(&Value, &Value) -> Result<Ordering, E>,
) -> Result<(), E> {
    let mut ret = Ok(());
    v.sort_by(|left, right| {
        if ret.is_err() {
            return Ordering::Equal;
        }

        match cmp(left, right) {
            Ok(ordering) => ordering,
            Err(err) => {
                ret = Err(err);
                Ordering::Equal
            }
        }
    });
    ret?;
    Ok(())
}

/// The element of `iter` that compares `better` (`Greater` for the largest)
/// than the others, the first of equal ones.
fn vm_try_extreme(
    mut iter: impl Iterator<Item = Value>,
    better: Ordering,
) -> anyhow::Result<Value> {
    iter.try_fold(None::<Value>, |acc, b| match acc {
        None => Ok(Some(b)),
        Some(a) => a
            .try_cmp(&b)
            .map_err(|e: String| anyhow!(e))
            .map(|o| Some(if o == better.reverse() { b } else { a })),
    })
    .and_then(|x: Option<Value>| {
        x.ok_or_else(|| anyhow!("empty input to {}", extreme_name(better)))
    })
}

fn extreme_name(better: Ordering) -> &'static str {
    if better == Ordering::Greater {
        "max"
    } else {
        "min"
    }
}

#[export_module]
mod inner {
    use super::{extreme_name, try_sort_by, vm_try_extreme};
    use itertools::Itertools;
    use ndc_vm::VmCallable;
    use std::cmp::Ordering;
    use std::rc::Rc;

    /// Returns `true` if the element is one of the values the sequence
    /// iterates over, or for a string, a part of it.
    #[function(name = "in", signature = "<T>(T, Iterable<T>) -> Bool")]
    pub fn op_contains(elem: Value, seq: Value) -> bool {
        let elem = &elem;
        match &seq {
            Value::Object(obj) => match obj.as_ref() {
                Object::String(s) => match elem {
                    Value::Object(e) => match e.as_ref() {
                        Object::String(needle) => {
                            if Rc::ptr_eq(s, needle) {
                                return true;
                            }
                            s.borrow().contains(needle.borrow().as_str())
                        }
                        _ => false,
                    },
                    _ => false,
                },
                Object::List(l) => l.borrow().contains(elem),
                Object::Tuple(t) => t.contains(elem),
                // A map iterates its entries, as `(key, value)` tuples.
                Object::Map { entries, .. } => match elem {
                    Value::Object(e) => match e.as_ref() {
                        Object::Tuple(entry) if let [key, value] = entry.as_slice() => {
                            entries.borrow().get(key) == Some(value)
                        }
                        _ => false,
                    },
                    _ => false,
                },
                Object::Deque(d) => d.borrow().contains(elem),
                Object::MinHeap(h) => h.borrow().iter().any(|v| v.0.0 == *elem),
                Object::MaxHeap(h) => h.borrow().iter().any(|v| v.0 == *elem),
                Object::Iterator(i) => {
                    {
                        let iter = i.borrow();
                        if let Some((start, end, inclusive)) = iter.range_bounds() {
                            let &Value::Int(n) = elem else { return false };
                            return if inclusive {
                                n >= start && n <= end
                            } else {
                                n >= start && n < end
                            };
                        }
                        if let Some(start) = iter.unbounded_range_start() {
                            let &Value::Int(n) = elem else { return false };
                            return n >= start;
                        }
                    }
                    // Finite non-range iterator — linear scan
                    loop {
                        match i.borrow_mut().next() {
                            Some(v) if v == *elem => return true,
                            Some(_) => {}
                            None => return false,
                        }
                    }
                }
                _ => false,
            },
            _ => false,
        }
    }

    /// Returns `true` if the map contains the given key.
    #[function(name = "in", signature = "<K, V>(K, %{K: V}) -> Bool")]
    pub fn op_contains_key(key: Value, map: &ndc_vm::hash_map::HashMap<Value, Value>) -> bool {
        map.contains_key(&key)
    }

    /// Returns the highest key in the map.
    #[function(name = "max", signature = "<K, V>(%{K: V}) -> K")]
    pub fn max_key(map: Value) -> anyhow::Result<Value> {
        extreme(&map, Ordering::Greater)
    }

    /// Returns the lowest key in the map.
    #[function(name = "min", signature = "<K, V>(%{K: V}) -> K")]
    pub fn min_key(map: Value) -> anyhow::Result<Value> {
        extreme(&map, Ordering::Less)
    }

    /// Returns the highest element in the sequence.
    #[function(signature = "<T>(Iterable<T>) -> T")]
    pub fn max(seq: Value) -> anyhow::Result<Value> {
        extreme(&seq, Ordering::Greater)
    }

    /// The largest element of `seq` for `better` `Greater`, the smallest for
    /// `Less`: a map's key, a string's character, the top of a heap that
    /// keeps that end on top.
    fn extreme(seq: &Value, better: Ordering) -> anyhow::Result<Value> {
        let name = extreme_name(better);
        let Value::Object(obj) = seq else {
            return Err(anyhow!("cannot find {name} of non-sequence"));
        };
        let top = match obj.as_ref() {
            Object::String(s) => {
                let chars = s
                    .borrow()
                    .chars()
                    .map(|c| Value::string(c.to_string()))
                    .collect_vec();
                return vm_try_extreme(chars.into_iter(), better);
            }
            Object::Map { entries, .. } => {
                return vm_try_extreme(entries.borrow().keys().cloned(), better);
            }
            Object::MaxHeap(h) if better == Ordering::Greater => {
                h.borrow().peek().map(|v| v.0.clone())
            }
            Object::MinHeap(h) if better == Ordering::Less => {
                h.borrow().peek().map(|v| v.0.0.clone())
            }
            _ => {
                let iter = seq
                    .clone()
                    .try_into_iter()
                    .ok_or_else(|| anyhow!("cannot find {name} of non-sequence"))?;
                return vm_try_extreme(iter, better);
            }
        };
        top.ok_or_else(|| anyhow!("empty input to {name}"))
    }

    /// Returns the element for which the key function returns the highest value.
    #[function(signature = "<T, U>(Iterable<T>, fn(T) -> U) -> T")]
    pub fn max_by_key(seq: Value, func: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_key(seq, func, Ordering::Greater)
    }

    /// Returns the element for which the key function returns the lowest value.
    #[function(signature = "<T, U>(Iterable<T>, fn(T) -> U) -> T")]
    pub fn min_by_key(seq: Value, func: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_key(seq, func, Ordering::Less)
    }

    /// Returns the maximum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number, as `<=>` does. A positive
    /// result means the first argument is greater than the second, a negative result means the
    /// first argument is less than the second, and zero means they are equal.
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Int) -> T")]
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Float) -> T")]
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Number) -> T")]
    pub fn max_by(seq: Value, comp: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_comp(seq, comp, Ordering::Greater)
    }

    /// Returns the minimum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number, as `<=>` does. A positive
    /// result means the first argument is greater than the second, a negative result means the
    /// first argument is less than the second, and zero means they are equal.
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Int) -> T")]
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Float) -> T")]
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Number) -> T")]
    pub fn min_by(seq: Value, comp: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_comp(seq, comp, Ordering::Less)
    }

    /// Returns the lowest element in the sequence.
    #[function(signature = "<T>(Iterable<T>) -> T")]
    pub fn min(seq: Value) -> anyhow::Result<Value> {
        extreme(&seq, Ordering::Less)
    }

    /// Sorts the list in place.
    #[function(signature = "<T>([T])")]
    pub fn sort(list: &mut Vec<Value>) -> anyhow::Result<()> {
        try_sort_by(list, |a, b| a.try_cmp(b).map_err(|e| anyhow!(e)))
    }

    /// Sorts the given sequence using a comparing function in place.
    ///
    /// The comparing function returns an Int, a Float or a Number, which is compared to `0` to determine the relative ordering such that:
    /// - for values lower than `0` the first argument is smaller than the second argument
    /// - for values higher than `0` the first argument is greater than the second argument
    /// - for values equal to `0` the first argument is equal to the second argument
    ///
    /// This function only works for strings and lists and will throw errors otherwise.
    #[function(signature = "<T>([T], fn(T, T) -> Int)")]
    #[function(signature = "<T>([T], fn(T, T) -> Float)")]
    #[function(signature = "<T>([T], fn(T, T) -> Number)")]
    pub fn sort_by(list: Value, comp: &mut VmCallable<'_>) -> anyhow::Result<()> {
        sort_with(&list, comp)
    }

    /// Sorts the list `list` in place by what `comp` says of each pair of
    /// elements. The comparator is the program's code, which may use the
    /// list, so it sorts a copy of the elements, with no borrow of the list
    /// held while the comparator runs, and puts them back after. A
    /// comparator that changed the list meanwhile is an error, as the order
    /// it said was for elements the list doesn't hold any more.
    fn sort_with(list: &Value, comp: &mut VmCallable<'_>) -> anyhow::Result<()> {
        let Value::Object(object) = list else {
            return Err(anyhow::anyhow!("sort_by takes a list"));
        };
        let Object::List(cell) = object.as_ref() else {
            return Err(anyhow::anyhow!("sort_by takes a list"));
        };
        let original = cell
            .try_borrow()
            .map_err(|_in_use| anyhow::anyhow!("the list is being changed"))?
            .clone();
        let mut sorted = original.clone();
        sort_values(&mut sorted, comp)?;
        let mut current = cell
            .try_borrow_mut()
            .map_err(|_in_use| anyhow::anyhow!("the list is in use"))?;
        if current.len() != original.len()
            || current
                .iter()
                .zip(&original)
                .any(|(now, before)| !is_the_same(now, before))
        {
            return Err(anyhow::anyhow!(
                "the list changed while `sort_by` sorted it"
            ));
        }
        *current = sorted;
        Ok(())
    }

    /// Whether `now` is the value `before` was, not only an equal one: the
    /// same object for a container, which an equal copy isn't, and the same
    /// kind and bits of number, as `1` and `1.0` are equal.
    fn is_the_same(now: &Value, before: &Value) -> bool {
        match (now, before) {
            (Value::Object(now), Value::Object(before)) => Rc::ptr_eq(now, before),
            (Value::Number(now), Value::Number(before)) => Rc::ptr_eq(now, before),
            (Value::Float(now), Value::Float(before)) => now.to_bits() == before.to_bits(),
            (Value::Int(now), Value::Int(before)) => now == before,
            (Value::Bool(now), Value::Bool(before)) => now == before,
            (Value::None, Value::None) => true,
            _ => false,
        }
    }

    /// Sorts `values` by what `comp` says of each pair of them.
    fn sort_values(values: &mut [Value], comp: &mut VmCallable<'_>) -> anyhow::Result<()> {
        let mut err: Option<String> = None;
        values.sort_by(|left, right| {
            if err.is_some() {
                return Ordering::Equal;
            }
            match comp.call(&[left.clone(), right.clone()]) {
                Ok(ret) => match ret.cmp_to_zero() {
                    Ok(ord) => ord,
                    Err(e) => {
                        err = Some(e);
                        Ordering::Equal
                    }
                },
                Err(e) => {
                    err = Some(e.message);
                    Ordering::Equal
                }
            }
        });
        if let Some(e) = err {
            return Err(anyhow::anyhow!(e));
        }
        Ok(())
    }

    /// Returns a sorted copy of the input sequence as a list.
    #[function(signature = "<T>(Iterable<T>) -> [T]")]
    pub fn sorted(seq: Value) -> anyhow::Result<Value> {
        let mut list: Vec<Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("sorted requires a sequence"))?
            .collect();
        try_sort_by(&mut list, |a, b| {
            a.try_cmp(b).map_err(|e: String| anyhow!(e))
        })?;
        Ok(Value::list(list))
    }

    /// Sorts the given sequence using a comparing function, returning a new list with the elements in the sorted order.
    ///
    /// The comparing function returns an Int, a Float or a Number, which is compared to `0` to determine the relative ordering such that:
    /// - for values lower than `0` the first argument is smaller than the second argument
    /// - for values higher than `0` the first argument is greater than the second argument
    /// - for values equal to `0` the first argument is equal to the second argument
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Int) -> [T]")]
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Float) -> [T]")]
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> Number) -> [T]")]
    pub fn sorted_by(seq: Value, comp: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut list: Vec<Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("sorted_by requires a sequence"))?
            .collect();
        sort_values(&mut list, comp)?;
        Ok(Value::list(list))
    }

    /// Returns the length of a string in bytes.
    pub fn byte_len(str: &str) -> i64 {
        str.len() as i64
    }

    /// Returns the number of elements of a collection whose size is known: a list, deque, map or
    /// heap, or a string, for which it's the number of UTF-8 characters. A tuple's is declared
    /// with `first` and `last`. An iterator's length isn't known without reading it.
    #[function(signature = "<T>([T]) -> Int")]
    #[function(signature = "(String) -> Int")]
    #[function(signature = "<K, V>(%{K: V}) -> Int")]
    #[function(signature = "<T>(Deque<T>) -> Int")]
    #[function(signature = "<T>(MinHeap<T>) -> Int")]
    #[function(signature = "<T>(MaxHeap<T>) -> Int")]
    pub fn len(seq: Value) -> anyhow::Result<i64> {
        let Value::Object(obj) = &seq else {
            return Err(anyhow!(
                "cannot determine the length of {}",
                seq.static_type()
            ));
        };
        let length = match obj.as_ref() {
            Object::List(l) => l.borrow().len(),
            Object::Deque(d) => d.borrow().len(),
            Object::String(s) => s.char_count(),
            Object::Map { entries, .. } => entries.borrow().len(),
            Object::MinHeap(h) => h.borrow().len(),
            Object::MaxHeap(h) => h.borrow().len(),
            _ => {
                return Err(anyhow!(
                    "cannot determine the length of {}",
                    seq.static_type()
                ));
            }
        };
        Ok(length as i64)
    }

    /// Returns a lazy iterator yielding `(index, value)` tuples for each element of `seq`.
    #[function(signature = "<T>(Iterable<T>) -> Iterator<(Int, T)>")]
    pub fn enumerate(seq: Value) -> anyhow::Result<Value> {
        let iter =
            EnumerateIter::new(seq).ok_or_else(|| anyhow!("enumerate requires a sequence"))?;
        Ok(Value::iterator(iter.into_shared()))
    }

    /// Reduces/folds the given sequence using the given combining function and a custom initial value.
    #[function(signature = "<T, A>(Iterable<T>, A, fn(A, T) -> A) -> A")]
    pub fn fold(
        seq: Value,
        initial: Value,
        function: &mut VmCallable<'_>,
    ) -> anyhow::Result<Value> {
        fold_iterator(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("fold requires a sequence"))?,
            initial,
            function,
        )
    }

    /// Reduces/folds the given sequence using the given combining function.
    #[function(signature = "<T>(Iterable<T>, fn(T, T) -> T) -> T")]
    pub fn reduce(seq: Value, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut iterator = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("reduce requires a sequence"))?;
        let fst = iterator
            .next()
            .ok_or_else(|| anyhow!("first argument to reduce must not be empty"))?;
        fold_iterator(iterator, fst, function)
    }

    /// Filters the given sequence using the `predicate`.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> [T]")]
    pub fn filter(seq: Value, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut out = Vec::new();
        for element in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("filter requires a sequence"))?
        {
            match predicate
                .call(std::slice::from_ref(&element))
                .map_err(|e| anyhow!(e))?
            {
                Value::Bool(true) => out.push(element),
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Ok(Value::list(out))
    }

    /// Returns the number of elements in the input sequence for which the given `predicate` returns `true`.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> Int")]
    pub fn count(seq: Value, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut out = 0i64;
        for element in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("count requires a sequence"))?
        {
            match predicate.call(&[element]).map_err(|e| anyhow!(e))? {
                Value::Bool(true) => out += 1,
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Ok(Value::Int(out))
    }

    /// Returns the value of the first element for which the `predicate` is true for the given input sequence.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> T")]
    pub fn find(seq: Value, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        for element in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("find requires a sequence"))?
        {
            match predicate
                .call(std::slice::from_ref(&element))
                .map_err(|e| anyhow!(e))?
            {
                Value::Bool(true) => return Ok(element),
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Err(anyhow!("find did not find anything"))
    }

    /// Returns the first index of the element for which the `predicate` is true in the input sequence.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> Int")]
    pub fn locate(seq: Value, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        for (idx, element) in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("locate requires a sequence"))?
            .enumerate()
        {
            match predicate.call(&[element]).map_err(|e| anyhow!(e))? {
                Value::Bool(true) => return Ok(Value::Int(idx as i64)),
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Err(anyhow!("locate did not find anything"))
    }

    /// Returns the first index of the element or produces an error
    #[function(name = "locate", signature = "<T>(Iterable<T>, T) -> Int")]
    pub fn locate_element(seq: Value, element: Value) -> anyhow::Result<Value> {
        for (idx, el) in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("locate requires a sequence"))?
            .enumerate()
        {
            if el == element {
                return Ok(Value::Int(idx as i64));
            }
        }
        Err(anyhow!("locate did not find anything"))
    }

    /// Returns `true` if the `predicate` is true for none of the elements in `seq`.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> Bool")]
    pub fn none(seq: Value, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("none requires a sequence"))?
        {
            match function.call(&[item]).map_err(|e| anyhow!(e))? {
                Value::Bool(true) => return Ok(Value::Bool(false)),
                Value::Bool(false) => {}
                v => {
                    return Err(anyhow!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ));
                }
            }
        }
        Ok(Value::Bool(true))
    }

    /// Returns `true` if the `predicate` is true for all the elements in `seq`.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> Bool")]
    pub fn all(seq: Value, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("all requires a sequence"))?
        {
            match function.call(&[item]).map_err(|e| anyhow!(e))? {
                Value::Bool(true) => {}
                Value::Bool(false) => return Ok(Value::Bool(false)),
                v => {
                    return Err(anyhow!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ));
                }
            }
        }
        Ok(Value::Bool(true))
    }

    /// Returns `true` if the `predicate` is true for any of the elements in `seq`.
    #[function(signature = "<T>(Iterable<T>, fn(T) -> Bool) -> Bool")]
    pub fn any(seq: Value, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("any requires a sequence"))?
        {
            match predicate.call(&[item]).map_err(|e| anyhow!(e))? {
                Value::Bool(true) => return Ok(Value::Bool(true)),
                Value::Bool(false) => {}
                v => {
                    return Err(anyhow!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ));
                }
            }
        }
        Ok(Value::Bool(false))
    }

    /// Applies the function to each element in a sequence returning the result as a list.
    #[function(signature = "<T, U>(Iterable<T>, fn(T) -> U) -> [U]")]
    pub fn map(seq: Value, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut out = Vec::new();
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("map requires a sequence"))?
        {
            out.push(function.call(&[item]).map_err(|e| anyhow!(e))?);
        }
        Ok(Value::list(out))
    }

    /// Applies a function to each item in a sequence, flattens the resulting sequences, and returns a single combined sequence.
    #[function(signature = "<T, U>(Iterable<T>, fn(T) -> Iterable<U>) -> [U]")]
    pub fn flat_map(seq: Value, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut out = Vec::new();
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("flat_map requires a sequence"))?
        {
            let result = function.call(&[item]).map_err(|e| anyhow!(e))?;
            let inner = result
                .try_into_iter()
                .ok_or_else(|| anyhow!("callable argument to flat_map must return a sequence"))?;
            out.extend(inner);
        }
        Ok(Value::list(out))
    }

    /// Returns the first element of the sequence or the `default` value otherwise.
    #[function(signature = "<T>(Iterable<T>, T) -> T")]
    pub fn first_or(seq: Value, default: Value) -> Value {
        seq.try_into_iter()
            .and_then(|mut i| i.next())
            .unwrap_or(default)
    }

    /// Returns the first element of the sequence or the return value of the given function.
    #[function(signature = "<T>(Iterable<T>, fn() -> T) -> T")]
    pub fn first_or_else(seq: Value, default: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        if let Some(item) = seq.try_into_iter().and_then(|mut i| i.next()) {
            return Ok(item);
        }
        default.call(&[]).map_err(|e| anyhow!(e))
    }

    /// Returns the `k` sized combinations of the given sequence `seq` as a lazy iterator of lists.
    #[function(signature = "<T>(Iterable<T>, Int) -> Iterator<[T]>")]
    pub fn combinations(seq: Value, k: usize) -> anyhow::Result<Value> {
        let iter = CombinationsIter::new(seq, k)
            .ok_or_else(|| anyhow!("combinations requires a sequence"))?;
        Ok(Value::iterator(iter.into_shared()))
    }

    /// Returns a lazy iterator yielding the first `n` elements of `seq`.
    #[function(signature = "<T>(Iterable<T>, Int) -> Iterator<T>")]
    pub fn take(seq: Value, n: usize) -> anyhow::Result<Value> {
        let iter = TakeIter::new(seq, n).ok_or_else(|| anyhow!("take requires a sequence"))?;
        Ok(Value::iterator(iter.into_shared()))
    }

    /// Returns the `k` sized permutations of the given sequence `seq` as a list of lists.
    #[function(signature = "<T>(Iterable<T>, Int) -> [[T]]")]
    pub fn permutations(seq: Value, k: usize) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("permutations requires a sequence"))?
                .permutations(k)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        ))
    }

    /// Returns all prefixes of the string as a list of strings.
    #[function(name = "prefixes", return_type = Vec<String>)]
    pub fn prefixes_string(text: &str) -> Value {
        Value::list(
            text.chars()
                .scan(String::new(), |acc, c| {
                    acc.push(c);
                    Some(Value::string(acc.clone()))
                })
                .collect(),
        )
    }

    /// Returns all prefixes of a sequence, each as a list.
    #[function(signature = "<T>(Iterable<T>) -> [[T]]")]
    pub fn prefixes(seq: Value) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("prefixes requires a sequence"))?
                .scan(Vec::new(), |acc, item| {
                    acc.push(item);
                    Some(Value::list(acc.clone()))
                })
                .collect(),
        ))
    }

    /// Returns all suffixes of the string as a list of strings.
    #[function(name = "suffixes", return_type = Vec<String>)]
    pub fn suffixes_string(text: &str) -> Value {
        Value::list(
            text.char_indices()
                .map(|(i, _)| Value::string(text[i..].to_string()))
                .collect(),
        )
    }

    /// Returns all suffixes of a sequence, each as a list.
    #[function(signature = "<T>(Iterable<T>) -> [[T]]")]
    pub fn suffixes(seq: Value) -> anyhow::Result<Value> {
        let out: Vec<Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("suffixes requires a sequence"))?
            .collect();
        Ok(Value::list(
            (0..out.len())
                .map(|i| Value::list(out[i..].to_vec()))
                .collect(),
        ))
    }

    /// Transposes a sequence of lists, or of strings, turning rows into columns, and returns the result as a list of lists.
    #[function(signature = "<T>(Iterable<[T]>) -> [[T]]")]
    #[function(signature = "(Iterable<String>) -> [[String]]")]
    pub fn transposed(seq: Value) -> anyhow::Result<Value> {
        transpose(seq)
    }

    /// Returns each element paired with the next, wrapping around from the last element to the first: `[1, 2, 3]` gives `[(1, 2), (2, 3), (3, 1)]`.
    #[function(signature = "<T>(Iterable<T>) -> [(T, T)]")]
    pub fn circular_pairwise(seq: Value) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("circular_pairwise requires a sequence"))?
                .collect::<Vec<_>>()
                .iter()
                .circular_tuple_windows::<(_, _)>()
                .map(|(a, b)| Value::tuple(vec![a.clone(), b.clone()]))
                .collect::<Vec<_>>(),
        ))
    }

    /// Returns a list of all size-2 windows in `seq`.
    #[function(signature = "<T>(Iterable<T>) -> [(T, T)]")]
    pub fn pairwise(seq: Value) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("pairwise requires a sequence"))?
                .collect::<Vec<_>>()
                .windows(2)
                .map(|w| Value::tuple(w.to_vec()))
                .collect::<Vec<_>>(),
        ))
    }

    /// Applies a function to each pair of consecutive elements in a sequence and returns the results as a list.
    #[function(
        name = "pairwise",
        signature = "<T, U>(Iterable<T>, fn(T, T) -> U) -> [U]"
    )]
    pub fn pairwise_map(seq: Value, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let main: Vec<Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("pairwise requires a sequence"))?
            .collect();
        let mut out = Vec::with_capacity(main.len().saturating_sub(1));
        for (a, b) in main.into_iter().tuple_windows() {
            out.push(function.call(&[a, b]).map_err(|e| anyhow!(e))?);
        }
        Ok(Value::list(out))
    }

    /// Returns a list of all contiguous windows of `length` size. The windows overlap. If the `seq` is shorter than size, the iterator returns no values.
    #[function(signature = "<T>(Iterable<T>, Int) -> [[T]]")]
    pub fn windows(seq: Value, length: usize) -> anyhow::Result<Value> {
        if length == 0 {
            return Err(anyhow!("window size must be non-zero"));
        }
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("windows requires a sequence"))?
                .collect::<Vec<Value>>()
                .windows(length)
                .map(|w| Value::list(w.to_vec()))
                .collect::<Vec<Value>>(),
        ))
    }

    /// Return a list that represents the powerset of the elements of `seq`.
    ///
    /// The powerset of a set contains all subsets including the empty set and the full input set. A powerset has length `2^n` where `n` is the length of the input set.
    /// Each list produced by this function represents a subset of the elements in the source sequence.
    #[function(signature = "<T>(Iterable<T>) -> [[T]]")]
    pub fn subsequences(seq: Value) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("subsequences requires a sequence"))?
                .powerset()
                .map(Value::list)
                .collect::<Vec<Value>>(),
        ))
    }

    /// Return a list that represents the powerset of the elements of `seq` that are exactly `length` long.
    #[function(name = "subsequences", signature = "<T>(Iterable<T>, Int) -> [[T]]")]
    pub fn subsequences_len(seq: Value, length: usize) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("subsequences requires a sequence"))?
                .powerset()
                .filter(|x| x.len() == length)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        ))
    }

    /// Computes the Cartesian product of a sequence of lists, returning a list of all possible combinations where each combination contains one element from each list.
    ///
    /// Example:
    /// ```ndc
    /// multi_cartesian_product([[1, 2], [3, 4, 5]])
    /// [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
    /// ```
    #[function(signature = "<T>(Iterable<[T]>) -> [[T]]")]
    pub fn multi_cartesian_product(seq: Value) -> anyhow::Result<Value> {
        cartesian_product(seq)
    }

    /// Split the input sequence into evenly sized chunks. If the input length of the sequence
    /// is not dividable by the chunk_size the last chunk will contain fewer elements.
    #[function(signature = "<T>(Iterable<T>, Int) -> [[T]]")]
    pub fn chunks(seq: Value, chunk_size: usize) -> anyhow::Result<Value> {
        if chunk_size == 0 {
            return Err(anyhow!("chunk size must be non-zero"));
        }
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("chunks requires a sequence"))?
                .chunks(chunk_size)
                .into_iter()
                .map(|chunk| Value::list(chunk.collect_vec()))
                .collect_vec(),
        ))
    }

    /// Returns an infinite iterator that repeats the given value.
    #[function(signature = "<T>(T) -> Iterator<T>")]
    pub fn repeat(value: Value) -> Value {
        Value::iterator(Rc::new(std::cell::RefCell::new(ndc_vm::RepeatIter::new(
            value,
        ))))
    }

    /// Returns an iterator that repeats the given value `times` times.
    #[function(name = "repeat", signature = "<T>(T, Int) -> Iterator<T>")]
    pub fn repeat_times(value: Value, times: usize) -> Value {
        Value::iterator(Rc::new(std::cell::RefCell::new(
            ndc_vm::RepeatIter::new_limited(value, times),
        )))
    }
}

fn transpose(seq: Value) -> anyhow::Result<Value> {
    let main: Vec<Value> = seq
        .try_into_iter()
        .ok_or_else(|| anyhow!("transposed requires a sequence"))?
        .collect();
    let mut iterators: Vec<Box<dyn Iterator<Item = Value>>> = Vec::new();
    for elem in main {
        iterators.push(Box::new(elem.try_into_iter().ok_or_else(|| {
            anyhow!("elements of transposed sequence must be iterable")
        })?));
    }
    let mut out = Vec::new();
    loop {
        let row: Vec<Value> = iterators.iter_mut().filter_map(|i| i.next()).collect();
        if row.is_empty() {
            return Ok(Value::list(out));
        }
        out.push(Value::list(row));
    }
}

fn cartesian_product(seq: Value) -> anyhow::Result<Value> {
    let mut iterators = Vec::new();
    for elem in seq
        .try_into_iter()
        .ok_or_else(|| anyhow!("multi_cartesian_product requires a sequence"))?
    {
        let inner: Vec<Value> = elem
            .try_into_iter()
            .ok_or_else(|| anyhow!("elements of sequence must be iterable"))?
            .collect_vec();
        iterators.push(inner.into_iter());
    }
    Ok(Value::list(
        iterators
            .into_iter()
            .multi_cartesian_product()
            .map(Value::list)
            .collect_vec(),
    ))
}

fn by_key(seq: Value, func: &mut VmCallable<'_>, better: Ordering) -> anyhow::Result<Value> {
    let mut best_value: Option<Value> = None;
    let mut best_key: Option<Value> = None;
    for value in seq
        .try_into_iter()
        .ok_or_else(|| anyhow!("sequence is required"))?
    {
        let new_key = func
            .call(std::slice::from_ref(&value))
            .map_err(|e| anyhow!(e))?;
        let is_better = match &best_key {
            None => true,
            Some(current_best) => {
                new_key
                    .try_cmp(current_best)
                    .map_err(|e: String| anyhow!(e))?
                    == better
            }
        };
        if is_better {
            best_key = Some(new_key);
            best_value = Some(value);
        }
    }
    best_value.ok_or_else(|| anyhow!("sequence was empty"))
}

fn by_comp(seq: Value, comp: &mut VmCallable<'_>, better: Ordering) -> anyhow::Result<Value> {
    let mut best: Option<Value> = None;
    for value in seq
        .try_into_iter()
        .ok_or_else(|| anyhow!("sequence is required"))?
    {
        let is_better = match &best {
            None => true,
            Some(current) => {
                let result = comp
                    .call(&[value.clone(), current.clone()])
                    .map_err(|e| anyhow!(e))?;
                result.cmp_to_zero().map_err(|e| anyhow!(e))? == better
            }
        };
        if is_better {
            best = Some(value);
        }
    }
    best.ok_or_else(|| anyhow!("sequence was empty"))
}

fn fold_iterator(
    iter: impl Iterator<Item = Value>,
    initial: Value,
    function: &mut VmCallable<'_>,
) -> anyhow::Result<Value> {
    let mut acc = initial;
    for item in iter {
        acc = function.call(&[acc, item]).map_err(|e| anyhow!(e))?;
    }
    Ok(acc)
}

pub mod extra {
    use ndc_core::{FunctionRegistry, StaticType};
    use ndc_vm::error::VmError;
    use ndc_vm::value::{NativeFunc, NativeFunction, Object, Value};
    use std::rc::Rc;

    pub fn register(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
        // A tuple's elements have their own types, and a signature can't say
        // "a tuple of any length", so each length up to twelve, as far as
        // Rust's standard library goes for tuples, gets one that says which.
        const LONGEST: usize = 12;
        let names = |count: usize| {
            ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"][..count].to_vec()
        };
        // Every tuple, the empty one too, has a length.
        let tuple_len = |args: &[Value]| match args {
            [Value::Object(object)] => match object.as_ref() {
                Object::Tuple(values) => Ok(Value::Int(values.len() as i64)),
                _ => Err(VmError::native("len of a tuple takes a tuple")),
            },
            _ => Err(VmError::native("len of a tuple takes a tuple")),
        };
        env.declare_global_fn(Rc::new(NativeFunction {
            name: "len".to_string(),
            documentation: Some("Returns the number of elements of a tuple.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::unit()]),
                return_type: Box::new(StaticType::Int),
            },
            func: NativeFunc::Simple(Box::new(tuple_len)),
        }));
        for length in 1..=LONGEST {
            let names = names(length);
            let parameters = names.join(", ");
            let tuple = if length == 1 {
                "(A,)".to_string()
            } else {
                format!("({parameters})")
            };
            env.declare_global_fn(Rc::new(NativeFunction {
                name: "len".to_string(),
                documentation: Some("Returns the number of elements of a tuple.".to_string()),
                static_type: StaticType::signature(&format!("<{parameters}>({tuple}) -> Int"))
                    .expect("valid signature"),
                func: NativeFunc::Simple(Box::new(tuple_len)),
            }));
            for (name, returns, documentation) in [
                ("first", names[0], "Returns the first element of a tuple."),
                (
                    "last",
                    names[length - 1],
                    "Returns the last element of a tuple.",
                ),
            ] {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: name.to_string(),
                    documentation: Some(documentation.to_string()),
                    static_type: StaticType::signature(&format!(
                        "<{parameters}>({tuple}) -> {returns}"
                    ))
                    .expect("valid signature"),
                    func: NativeFunc::Simple(Box::new(move |args| {
                        tuple_end(args, name == "first")
                    })),
                }));
            }
        }
        // Two or more sequences zip into tuples of their elements.
        for length in 2..=LONGEST {
            let names = names(length);
            let sequences = names
                .iter()
                .map(|name| format!("Iterable<{name}>"))
                .collect::<Vec<_>>()
                .join(", ");
            env.declare_global_fn(Rc::new(NativeFunction {
                name: "zip".to_string(),
                documentation: Some(ZIP_DOCUMENTATION.to_string()),
                static_type: StaticType::signature(&format!(
                    "<{}>({sequences}) -> [({})]",
                    names.join(", "),
                    names.join(", ")
                ))
                .expect("valid signature"),
                func: NativeFunc::Simple(Box::new(zip)),
            }));
        }
    }

    const ZIP_DOCUMENTATION: &str = "Zips two or more sequences together into a list of tuples. The result length is the minimum length of the inputs.";

    /// The first or last element of the tuple in `args`.
    fn tuple_end(args: &[Value], first: bool) -> Result<Value, VmError> {
        let [Value::Object(object)] = args else {
            return Err(VmError::native("expected one tuple".to_string()));
        };
        let Object::Tuple(values) = object.as_ref() else {
            return Err(VmError::native("expected a tuple".to_string()));
        };
        let end = if first { values.first() } else { values.last() };
        end.cloned()
            .ok_or_else(|| VmError::native("the tuple is empty".to_string()))
    }

    fn zip(args: &[Value]) -> Result<Value, VmError> {
        if args.len() < 2 {
            return Err(VmError::native(
                "zip must be called with 2 or more arguments".to_string(),
            ));
        }
        // The iterators are stepped together, so an endless one like `1..`
        // stops with the others.
        let mut iters = args
            .iter()
            .map(|arg| arg.clone().try_into_iter())
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| VmError::native("zip requires sequences".to_string()))?;
        let mut result = Vec::new();
        'zipping: loop {
            let mut tuple = Vec::with_capacity(iters.len());
            for iter in &mut iters {
                let Some(value) = iter.next() else {
                    break 'zipping;
                };
                tuple.push(value);
            }
            result.push(Value::Object(Rc::new(Object::Tuple(tuple))));
        }
        Ok(Value::Object(Rc::new(Object::list(result))))
    }
}
