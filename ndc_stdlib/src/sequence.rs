#![allow(clippy::ptr_arg)]

use anyhow::anyhow;
use itertools::Itertools;
use ndc_macros::export_module;
use ndc_vm::VmCallable;
use ndc_vm::value::{Object, SeqValue, Value};
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

fn vm_try_max(mut iter: impl Iterator<Item = Value>) -> anyhow::Result<Value> {
    iter.try_fold(None::<Value>, |acc, b| match acc {
        None => Ok(Some(b)),
        Some(a) => a
            .try_cmp(&b)
            .map_err(|e: String| anyhow!(e))
            .map(|o| Some(if o == Ordering::Less { b } else { a })),
    })
    .and_then(|x: Option<Value>| x.ok_or_else(|| anyhow!("empty input to max")))
}

fn vm_try_min(mut iter: impl Iterator<Item = Value>) -> anyhow::Result<Value> {
    iter.try_fold(None::<Value>, |acc, b| match acc {
        None => Ok(Some(b)),
        Some(a) => a
            .try_cmp(&b)
            .map_err(|e: String| anyhow!(e))
            .map(|o| Some(if o == Ordering::Greater { b } else { a })),
    })
    .and_then(|x: Option<Value>| x.ok_or_else(|| anyhow!("empty input to min")))
}

#[export_module]
mod inner {
    use super::{try_sort_by, vm_try_max, vm_try_min};
    use itertools::Itertools;
    use ndc_vm::VmCallable;
    use std::cmp::Ordering;
    use std::rc::Rc;

    /// Returns `true` if the element is contained in the sequence.
    #[function(name = "in", signature = "<T>(T, Iterable<T>) -> Bool")]
    pub fn op_contains(elem: Value, seq: SeqValue) -> bool {
        contains(&elem, &seq)
    }

    fn contains(elem: &Value, seq: &Value) -> bool {
        match seq {
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
                Object::Map { entries, .. } => entries.borrow().contains_key(elem),
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
    #[function(name = "in", signature = "<K, V>(K, Map<K, V>) -> Bool")]
    pub fn op_contains_key(key: Value, map: SeqValue) -> bool {
        contains(&key, &map)
    }

    /// Returns the highest key in the map.
    #[function(name = "max", signature = "<K, V>(Map<K, V>) -> K")]
    pub fn max_key(map: SeqValue) -> anyhow::Result<Value> {
        highest(map)
    }

    /// Returns the lowest key in the map.
    #[function(name = "min", signature = "<K, V>(Map<K, V>) -> K")]
    pub fn min_key(map: SeqValue) -> anyhow::Result<Value> {
        lowest(map)
    }

    /// Returns the highest element in the sequence.
    #[function(signature = "<T>(Iterable<T>) -> T")]
    pub fn max(seq: SeqValue) -> anyhow::Result<Value> {
        highest(seq)
    }

    fn highest(seq: Value) -> anyhow::Result<Value> {
        match seq {
            Value::Object(obj) => match Rc::unwrap_or_clone(obj) {
                Object::String(s) => {
                    let chars: Vec<Value> = s
                        .borrow()
                        .chars()
                        .map(|c| Value::string(c.to_string()))
                        .collect();
                    vm_try_max(chars.into_iter())
                }
                Object::Map { entries, .. } => vm_try_max(entries.into_inner().into_keys()),
                Object::MaxHeap(h) => h
                    .borrow()
                    .peek()
                    .map(|v| v.0.clone())
                    .ok_or_else(|| anyhow!("empty input to max")),
                Object::MinHeap(_) => Err(anyhow!("max is not supported for MinHeap")),
                obj => vm_try_max(
                    Value::Object(Rc::new(obj))
                        .try_into_iter()
                        .ok_or_else(|| anyhow!("cannot find max of non-sequence"))?,
                ),
            },
            _ => Err(anyhow!("cannot find max of non-sequence")),
        }
    }

    /// Returns the element for which the key function returns the highest value.
    #[function(signature = "<T, U>(Iterable<T>, Fn(T) -> U) -> T")]
    pub fn max_by_key(seq: SeqValue, func: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_key(seq, func, Ordering::Greater)
    }

    /// Returns the element for which the key function returns the lowest value.
    #[function(signature = "<T, U>(Iterable<T>, Fn(T) -> U) -> T")]
    pub fn min_by_key(seq: SeqValue, func: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_key(seq, func, Ordering::Less)
    }

    /// Returns the maximum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number. A positive result means the
    /// first argument is greater than the second, a negative result means the first argument is
    /// less than the second, and zero means they are equal.
    #[function(signature = "<T, U>(Iterable<T>, Fn(T, T) -> U) -> T")]
    pub fn max_by(seq: SeqValue, comp: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_comp(seq, comp, Ordering::Greater)
    }

    /// Returns the minimum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number. A positive result means the
    /// first argument is greater than the second, a negative result means the first argument is
    /// less than the second, and zero means they are equal.
    #[function(signature = "<T, U>(Iterable<T>, Fn(T, T) -> U) -> T")]
    pub fn min_by(seq: SeqValue, comp: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        by_comp(seq, comp, Ordering::Less)
    }

    /// Returns the lowest element in the sequence.
    #[function(signature = "<T>(Iterable<T>) -> T")]
    pub fn min(seq: SeqValue) -> anyhow::Result<Value> {
        lowest(seq)
    }

    fn lowest(seq: Value) -> anyhow::Result<Value> {
        match seq {
            Value::Object(obj) => match Rc::unwrap_or_clone(obj) {
                Object::String(s) => {
                    let chars: Vec<Value> = s
                        .borrow()
                        .chars()
                        .map(|c| Value::string(c.to_string()))
                        .collect();
                    vm_try_min(chars.into_iter())
                }
                Object::Map { entries, .. } => vm_try_min(entries.into_inner().into_keys()),
                Object::MinHeap(h) => h
                    .borrow()
                    .peek()
                    .map(|v| v.0.0.clone())
                    .ok_or_else(|| anyhow!("empty input to min")),
                Object::MaxHeap(_) => Err(anyhow!("min is not supported for MaxHeap")),
                obj => vm_try_min(
                    Value::Object(Rc::new(obj))
                        .try_into_iter()
                        .ok_or_else(|| anyhow!("cannot find min of non-sequence"))?,
                ),
            },
            _ => Err(anyhow!("cannot find min of non-sequence")),
        }
    }

    /// Sorts the list in place.
    #[function(signature = "<T>(List<T>)")]
    pub fn sort(list: &mut Vec<Value>) -> anyhow::Result<()> {
        let mut err: Option<String> = None;
        list.sort_by(|a, b| {
            if let Some(ord) = a.partial_cmp(b) {
                ord
            } else {
                err = Some(format!(
                    "cannot compare {} and {}",
                    a.static_type(),
                    b.static_type()
                ));
                Ordering::Equal
            }
        });
        if let Some(e) = err {
            return Err(anyhow::anyhow!(e));
        }
        Ok(())
    }

    /// Sorts the given sequence using a comparing function in place.
    ///
    /// The result of the comparing function is compared to the number `0` to determine the relative ordering such that:
    /// - for values lower than `0` the first argument is smaller than the second argument
    /// - for values higher than `0` the first argument is greater than the second argument
    /// - for values equal to `0` the first argument is equal to the second argument
    ///
    /// This function only works for strings and lists and will throw errors otherwise.
    #[function(signature = "<T, U>(List<T>, Fn(T, T) -> U)")]
    pub fn sort_by(list: &mut Vec<Value>, comp: &mut VmCallable<'_>) -> anyhow::Result<()> {
        let mut err: Option<String> = None;
        list.sort_by(|left, right| {
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
    #[function(signature = "<T>(Iterable<T>) -> List<T>")]
    pub fn sorted(seq: SeqValue) -> anyhow::Result<Value> {
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
    /// The result of the comparing function is compared to the number `0` to determine the relative ordering such that:
    /// - for values lower than `0` the first argument is smaller than the second argument
    /// - for values higher than `0` the first argument is greater than the second argument
    /// - for values equal to `0` the first argument is equal to the second argument
    #[function(signature = "<T, U>(Iterable<T>, Fn(T, T) -> U) -> List<T>")]
    pub fn sorted_by(seq: SeqValue, comp: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut list: Vec<Value> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("sorted_by requires a sequence"))?
            .collect();
        let mut err: Option<String> = None;
        list.sort_by(|left, right| {
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
        Ok(Value::list(list))
    }

    /// Returns the length of a string in bytes.
    pub fn byte_len(str: &str) -> i64 {
        str.len() as i64
    }

    /// Returns the length of the sequence, for strings this returns the number of UTF-8 characters.
    #[function(signature = "<T>(Iterable<T>) -> Int")]
    pub fn len(seq: SeqValue) -> anyhow::Result<i64> {
        match &seq {
            Value::Object(obj) => match obj.as_ref() {
                Object::List(l) => Ok(l.borrow().len() as i64),
                Object::Tuple(t) => Ok(t.len() as i64),
                Object::Deque(d) => Ok(d.borrow().len() as i64),
                Object::String(s) => Ok(s.char_count() as i64),
                Object::Map { entries, .. } => Ok(entries.borrow().len() as i64),
                _ => Err(anyhow!(
                    "cannot determine the length of {}",
                    seq.static_type()
                )),
            },
            _ => Err(anyhow!(
                "cannot determine the length of {}",
                seq.static_type()
            )),
        }
    }

    /// Returns a lazy iterator yielding `(index, value)` tuples for each element of `seq`.
    #[function(signature = "<T>(Iterable<T>) -> Iterator<(Int, T)>")]
    pub fn enumerate(seq: SeqValue) -> anyhow::Result<Value> {
        let iter =
            EnumerateIter::new(seq).ok_or_else(|| anyhow!("enumerate requires a sequence"))?;
        Ok(Value::iterator(iter.into_shared()))
    }

    /// Reduces/folds the given sequence using the given combining function and a custom initial value.
    #[function(signature = "<T, A>(Iterable<T>, A, Fn(A, T) -> A) -> A")]
    pub fn fold(
        seq: SeqValue,
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
    #[function(signature = "<T>(Iterable<T>, Fn(T, T) -> T) -> T")]
    pub fn reduce(seq: SeqValue, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        let mut iterator = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("reduce requires a sequence"))?;
        let fst = iterator
            .next()
            .ok_or_else(|| anyhow!("first argument to reduce must not be empty"))?;
        fold_iterator(iterator, fst, function)
    }

    /// Filters the given sequence using the `predicate`.
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> List<T>")]
    pub fn filter(seq: SeqValue, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> Int")]
    pub fn count(seq: SeqValue, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> T")]
    pub fn find(seq: SeqValue, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> Int")]
    pub fn locate(seq: SeqValue, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    pub fn locate_element(seq: SeqValue, element: Value) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> Bool")]
    pub fn none(seq: SeqValue, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> Bool")]
    pub fn all(seq: SeqValue, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Fn(T) -> Bool) -> Bool")]
    pub fn any(seq: SeqValue, predicate: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T, U>(Iterable<T>, Fn(T) -> U) -> List<U>")]
    pub fn map(seq: SeqValue, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T, U>(Iterable<T>, Fn(T) -> Iterable<U>) -> List<U>")]
    pub fn flat_map(seq: SeqValue, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    pub fn first_or(seq: SeqValue, default: Value) -> Value {
        seq.try_into_iter()
            .and_then(|mut i| i.next())
            .unwrap_or(default)
    }

    /// Returns the first element of the sequence or the return value of the given function.
    #[function(signature = "<T>(Iterable<T>, Fn() -> T) -> T")]
    pub fn first_or_else(seq: SeqValue, default: &mut VmCallable<'_>) -> anyhow::Result<Value> {
        if let Some(item) = seq.try_into_iter().and_then(|mut i| i.next()) {
            return Ok(item);
        }
        default.call(&[]).map_err(|e| anyhow!(e))
    }

    /// Returns the `k` sized combinations of the given sequence `seq` as a lazy iterator of lists.
    #[function(signature = "<T>(Iterable<T>, Int) -> Iterator<List<T>>")]
    pub fn combinations(seq: SeqValue, k: usize) -> anyhow::Result<Value> {
        let iter = CombinationsIter::new(seq, k)
            .ok_or_else(|| anyhow!("combinations requires a sequence"))?;
        Ok(Value::iterator(iter.into_shared()))
    }

    /// Returns a lazy iterator yielding the first `n` elements of `seq`.
    #[function(signature = "<T>(Iterable<T>, Int) -> Iterator<T>")]
    pub fn take(seq: SeqValue, n: usize) -> anyhow::Result<Value> {
        let iter = TakeIter::new(seq, n).ok_or_else(|| anyhow!("take requires a sequence"))?;
        Ok(Value::iterator(iter.into_shared()))
    }

    /// Returns the `k` sized permutations of the given sequence `seq` as a list of lists.
    #[function(signature = "<T>(Iterable<T>, Int) -> List<List<T>>")]
    pub fn permutations(seq: SeqValue, k: usize) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("permutations requires a sequence"))?
                .permutations(k)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        ))
    }

    fn string_prefixes(text: &str) -> Value {
        Value::list(
            text.chars()
                .scan(String::new(), |acc, c| {
                    acc.push(c);
                    Some(Value::string(acc.clone()))
                })
                .collect(),
        )
    }

    /// Returns all prefixes of the string as a list of strings.
    #[function(name = "prefixes", return_type = Vec<String>)]
    pub fn prefixes_string(text: &str) -> Value {
        string_prefixes(text)
    }

    /// Returns all prefixes of a sequence, each as a list; for strings, returns string prefixes.
    #[function(signature = "<T>(Iterable<T>) -> List<List<T>>")]
    pub fn prefixes(seq: SeqValue) -> anyhow::Result<Value> {
        // A sequence-typed caller can still supply a string.
        if let Value::Object(ref obj) = seq
            && let Object::String(s) = obj.as_ref()
        {
            return Ok(string_prefixes(&s.borrow()));
        }
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

    fn string_suffixes(text: &str) -> Value {
        Value::list(
            text.char_indices()
                .map(|(i, _)| Value::string(text[i..].to_string()))
                .collect(),
        )
    }

    /// Returns all suffixes of the string as a list of strings.
    #[function(name = "suffixes", return_type = Vec<String>)]
    pub fn suffixes_string(text: &str) -> Value {
        string_suffixes(text)
    }

    /// Returns all suffixes of a sequence, each as a list; for strings, returns all trailing substrings.
    #[function(signature = "<T>(Iterable<T>) -> List<List<T>>")]
    pub fn suffixes(seq: SeqValue) -> anyhow::Result<Value> {
        // A sequence-typed caller can still supply a string.
        if let Value::Object(ref obj) = seq
            && let Object::String(s) = obj.as_ref()
        {
            return Ok(string_suffixes(&s.borrow()));
        }
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

    /// Transposes a sequence of sequences, turning rows into columns, and returns the result as a list of lists.
    #[function(signature = "<T>(Iterable<List<T>>) -> List<List<T>>")]
    pub fn transposed(seq: SeqValue) -> anyhow::Result<Value> {
        transpose(seq)
    }

    /// Transposes a sequence of strings, turning rows of characters into columns.
    #[function(
        name = "transposed",
        signature = "(Iterable<String>) -> List<List<String>>"
    )]
    pub fn transposed_strings(seq: SeqValue) -> anyhow::Result<Value> {
        transpose(seq)
    }

    /// Transposes a sequence of sequences whose elements have different types.
    #[function(name = "transposed", signature = "(Iterable<Any>) -> List<List<Any>>")]
    pub fn transposed_any(seq: SeqValue) -> anyhow::Result<Value> {
        transpose(seq)
    }

    /// Return a list of all windows, wrapping back to the first elements when the window would otherwise exceed the length of source list, producing tuples of size 2.
    #[function(signature = "<T>(Iterable<T>) -> List<(T, T)>")]
    pub fn circular_tuple_windows(seq: SeqValue) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("circular_tuple_windows requires a sequence"))?
                .collect::<Vec<_>>()
                .iter()
                .circular_tuple_windows::<(_, _)>()
                .map(|(a, b)| Value::tuple(vec![a.clone(), b.clone()]))
                .collect::<Vec<_>>(),
        ))
    }

    /// Returns a list of all size-2 windows in `seq`.
    #[function(signature = "<T>(Iterable<T>) -> List<List<T>>")]
    pub fn pairwise(seq: SeqValue) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("pairwise requires a sequence"))?
                .collect::<Vec<_>>()
                .windows(2)
                .map(|w| Value::list(w.to_vec()))
                .collect::<Vec<_>>(),
        ))
    }

    /// Applies a function to each pair of consecutive elements in a sequence and returns the results as a list.
    #[function(
        name = "pairwise",
        signature = "<T, U>(Iterable<T>, Fn(T, T) -> U) -> List<U>"
    )]
    pub fn pairwise_map(seq: SeqValue, function: &mut VmCallable<'_>) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>, Int) -> List<List<T>>")]
    pub fn windows(seq: SeqValue, length: usize) -> anyhow::Result<Value> {
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
    #[function(signature = "<T>(Iterable<T>) -> List<List<T>>")]
    pub fn subsequences(seq: SeqValue) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("subsequences requires a sequence"))?
                .powerset()
                .map(Value::list)
                .collect::<Vec<Value>>(),
        ))
    }

    /// Return a list that represents the powerset of the elements of `seq` that are exactly `length` long.
    #[function(
        name = "subsequences",
        signature = "<T>(Iterable<T>, Int) -> List<List<T>>"
    )]
    pub fn subsequences_len(seq: SeqValue, length: usize) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("subsequences requires a sequence"))?
                .powerset()
                .filter(|x| x.len() == length)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        ))
    }

    /// Computes the Cartesian product of multiple iterables, returning a list of all possible combinations where each combination contains one element from each iterable.
    ///
    /// Example:
    /// ```ndc
    /// multi_cartesian_product((1..=3, "abc", [true, false]))
    /// [
    ///   [1,"a",true],
    ///   [1,"a",false],
    ///   [1,"b",true],
    ///   [1,"b",false],
    ///   [1,"c",true],
    ///   [1,"c",false],
    ///   [2,"a",true],
    ///   [2,"a",false],
    ///   [2,"b",true],
    ///   [2,"b",false],
    ///   [2,"c",true],
    ///   [2,"c",false],
    ///   [3,"a",true],
    ///   [3,"a",false],
    ///   [3,"b",true],
    ///   [3,"b",false],
    ///   [3,"c",true],
    ///   [3,"c",false]
    /// ]
    /// ```
    #[function(signature = "<T>(Iterable<List<T>>) -> List<List<T>>")]
    pub fn multi_cartesian_product(seq: SeqValue) -> anyhow::Result<Value> {
        cartesian_product(seq)
    }

    /// The cartesian product of sequences whose elements have different types.
    #[function(
        name = "multi_cartesian_product",
        signature = "(Iterable<Any>) -> List<List<Any>>"
    )]
    pub fn multi_cartesian_product_any(seq: SeqValue) -> anyhow::Result<Value> {
        cartesian_product(seq)
    }

    /// Split the input sequence into evenly sized chunks. If the input length of the sequence
    /// is not dividable by the chunk_size the last chunk will contain fewer elements.
    #[function(signature = "<T>(Iterable<T>, Int) -> List<List<T>>")]
    pub fn chunks(seq: SeqValue, chunk_size: usize) -> anyhow::Result<Value> {
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

fn transpose(seq: SeqValue) -> anyhow::Result<Value> {
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

fn cartesian_product(seq: SeqValue) -> anyhow::Result<Value> {
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
        for length in 1..=LONGEST {
            let names = names(length);
            let parameters = names.join(", ");
            let tuple = if length == 1 {
                "(A,)".to_string()
            } else {
                format!("({parameters})")
            };
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
                    "<{}>({sequences}) -> List<({})>",
                    names.join(", "),
                    names.join(", ")
                ))
                .expect("valid signature"),
                func: NativeFunc::Simple(Box::new(zip)),
            }));
        }
        // More make tuples as long as the number of arguments.
        env.declare_global_fn(Rc::new(NativeFunction {
            name: "zip".to_string(),
            documentation: Some(ZIP_DOCUMENTATION.to_string()),
            static_type: StaticType::signature("(*) -> List<Any>").expect("valid signature"),
            func: NativeFunc::Simple(Box::new(zip)),
        }));
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
