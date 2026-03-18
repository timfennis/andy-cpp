#![allow(clippy::ptr_arg)]

use anyhow::anyhow;
use ndc_interpreter::compare::FallibleOrd;
use ndc_macros::export_module;
use ndc_vm::value::Value as VmValue;
use ndc_vm::vm::VmCallable;
use std::cmp::Ordering;

fn try_sort_by<E>(
    v: &mut [VmValue],
    cmp: impl Fn(&VmValue, &VmValue) -> Result<Ordering, E>,
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

fn vm_try_max(mut iter: impl Iterator<Item = VmValue>) -> anyhow::Result<VmValue> {
    iter.try_fold(None::<VmValue>, |acc, b| match acc {
        None => Ok(Some(b)),
        Some(a) => a
            .try_cmp(&b)
            .map_err(|e: String| anyhow!(e))
            .map(|o| Some(if o == Ordering::Less { b } else { a })),
    })
    .and_then(|x: Option<VmValue>| x.ok_or_else(|| anyhow!("empty input to max")))
}

fn vm_try_min(mut iter: impl Iterator<Item = VmValue>) -> anyhow::Result<VmValue> {
    iter.try_fold(None::<VmValue>, |acc, b| match acc {
        None => Ok(Some(b)),
        Some(a) => a
            .try_cmp(&b)
            .map_err(|e: String| anyhow!(e))
            .map(|o| Some(if o == Ordering::Greater { b } else { a })),
    })
    .and_then(|x: Option<VmValue>| x.ok_or_else(|| anyhow!("empty input to min")))
}

#[export_module]
mod inner {
    use super::{try_sort_by, vm_try_max, vm_try_min};
    use itertools::Itertools;
    use ndc_interpreter::compare::FallibleOrd;
    use ndc_vm::value::{Object, Value as VmValue};
    use ndc_vm::vm::VmCallable;
    use std::cmp::Ordering;
    use std::rc::Rc;

    #[function(name = "in")]
    pub fn op_contains(elem: ndc_vm::value::Value, seq: ndc_vm::value::SeqValue) -> bool {
        match &seq {
            VmValue::Object(obj) => match obj.as_ref() {
                Object::String(s) => match &elem {
                    VmValue::Object(e) => match e.as_ref() {
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
                Object::List(l) => l.borrow().contains(&elem),
                Object::Tuple(t) => t.contains(&elem),
                Object::Map { entries, .. } => entries.borrow().contains_key(&elem),
                Object::Deque(d) => d.borrow().contains(&elem),
                Object::MinHeap(h) => h.borrow().iter().any(|v| v.0.0 == elem),
                Object::MaxHeap(h) => h.borrow().iter().any(|v| v.0 == elem),
                Object::Iterator(i) => {
                    {
                        let iter = i.borrow();
                        if let Some((start, end, inclusive)) = iter.range_bounds() {
                            let VmValue::Int(n) = elem else { return false };
                            return if inclusive {
                                n >= start && n <= end
                            } else {
                                n >= start && n < end
                            };
                        }
                        if let Some(start) = iter.unbounded_range_start() {
                            let VmValue::Int(n) = elem else { return false };
                            return n >= start;
                        }
                    }
                    // Finite non-range iterator — linear scan
                    loop {
                        match i.borrow_mut().next() {
                            Some(v) if v == elem => return true,
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

    /// Returns the highest element in the sequence.
    pub fn max(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        match seq {
            VmValue::Object(obj) => match Rc::unwrap_or_clone(obj) {
                Object::String(s) => {
                    let chars: Vec<VmValue> = s
                        .borrow()
                        .chars()
                        .map(|c| VmValue::string(c.to_string()))
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
                    VmValue::Object(Rc::new(obj))
                        .try_into_iter()
                        .ok_or_else(|| anyhow!("cannot find max of non-sequence"))?,
                ),
            },
            _ => Err(anyhow!("cannot find max of non-sequence")),
        }
    }

    /// Returns the element for which the key function returns the highest value.
    pub fn max_by_key(
        seq: ndc_vm::value::SeqValue,
        func: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        by_key(seq, func, Ordering::Greater)
    }

    /// Returns the element for which the key function returns the lowest value.
    pub fn min_by_key(
        seq: ndc_vm::value::SeqValue,
        func: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        by_key(seq, func, Ordering::Less)
    }

    /// Returns the maximum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number. A positive result means the
    /// first argument is greater than the second, a negative result means the first argument is
    /// less than the second, and zero means they are equal.
    pub fn max_by(
        seq: ndc_vm::value::SeqValue,
        comp: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        by_comp(seq, comp, Ordering::Greater)
    }

    /// Returns the minimum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number. A positive result means the
    /// first argument is greater than the second, a negative result means the first argument is
    /// less than the second, and zero means they are equal.
    pub fn min_by(
        seq: ndc_vm::value::SeqValue,
        comp: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        by_comp(seq, comp, Ordering::Less)
    }

    /// Returns the lowest element in the sequence.
    pub fn min(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        match seq {
            VmValue::Object(obj) => match Rc::unwrap_or_clone(obj) {
                Object::String(s) => {
                    let chars: Vec<VmValue> = s
                        .borrow()
                        .chars()
                        .map(|c| VmValue::string(c.to_string()))
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
                    VmValue::Object(Rc::new(obj))
                        .try_into_iter()
                        .ok_or_else(|| anyhow!("cannot find min of non-sequence"))?,
                ),
            },
            _ => Err(anyhow!("cannot find min of non-sequence")),
        }
    }

    /// Sorts the list in place.
    #[function(return_type = ())]
    pub fn sort(list: &mut Vec<ndc_vm::value::Value>) -> anyhow::Result<()> {
        let mut err: Option<String> = None;
        list.sort_by(|a, b| match a.partial_cmp(b) {
            Some(ord) => ord,
            None => {
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
    #[function(return_type = ())]
    pub fn sort_by(
        list: &mut Vec<ndc_vm::value::Value>,
        comp: &VmCallable<'_>,
    ) -> anyhow::Result<()> {
        let mut err: Option<String> = None;
        list.sort_by(|left, right| {
            if err.is_some() {
                return Ordering::Equal;
            }
            match comp.call(vec![left.clone(), right.clone()]) {
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
    pub fn sorted(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        let mut list: Vec<VmValue> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("sorted requires a sequence"))?
            .collect();
        try_sort_by(&mut list, |a, b| {
            a.try_cmp(b).map_err(|e: String| anyhow!(e))
        })?;
        Ok(VmValue::list(list))
    }

    /// Sorts the given sequence using a comparing function, returning a new list with the elements in the sorted order.
    ///
    /// The result of the comparing function is compared to the number `0` to determine the relative ordering such that:
    /// - for values lower than `0` the first argument is smaller than the second argument
    /// - for values higher than `0` the first argument is greater than the second argument
    /// - for values equal to `0` the first argument is equal to the second argument
    pub fn sorted_by(
        seq: ndc_vm::value::SeqValue,
        comp: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut list: Vec<VmValue> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("sorted_by requires a sequence"))?
            .collect();
        let mut err: Option<String> = None;
        list.sort_by(|left, right| {
            if err.is_some() {
                return Ordering::Equal;
            }
            match comp.call(vec![left.clone(), right.clone()]) {
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
        Ok(VmValue::list(list))
    }

    /// Returns the length of a string in bytes.
    pub fn byte_len(str: &str) -> i64 {
        str.len() as i64
    }

    /// Returns the length of the sequence, for strings this returns the number of UTF-8 characters.
    pub fn len(seq: ndc_vm::value::SeqValue) -> anyhow::Result<i64> {
        match &seq {
            VmValue::Object(obj) => match obj.as_ref() {
                Object::List(l) => Ok(l.borrow().len() as i64),
                Object::Tuple(t) => Ok(t.len() as i64),
                Object::Deque(d) => Ok(d.borrow().len() as i64),
                Object::String(s) => Ok(s.borrow().chars().count() as i64),
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

    /// Enumerates the given sequence returning a list of tuples where the first element of the tuple is the index of the element in the input sequence.
    pub fn enumerate(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("enumerate requires a sequence"))?
                .enumerate()
                .map(|(i, v)| VmValue::tuple(vec![VmValue::Int(i as i64), v]))
                .collect(),
        ))
    }

    /// Reduces/folds the given sequence using the given combining function and a custom initial value.
    pub fn fold(
        seq: ndc_vm::value::SeqValue,
        initial: ndc_vm::value::Value,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        fold_iterator(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("fold requires a sequence"))?,
            initial,
            function,
        )
    }

    /// Reduces/folds the given sequence using the given combining function.
    pub fn reduce(
        seq: ndc_vm::value::SeqValue,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut iterator = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("reduce requires a sequence"))?;
        let fst = iterator
            .next()
            .ok_or_else(|| anyhow!("first argument to reduce must not be empty"))?;
        fold_iterator(iterator, fst, function)
    }

    /// Filters the given sequence using the `predicate`.
    pub fn filter(
        seq: ndc_vm::value::SeqValue,
        predicate: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut out = Vec::new();
        for element in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("filter requires a sequence"))?
        {
            match predicate
                .call(vec![element.clone()])
                .map_err(|e| anyhow!(e))?
            {
                VmValue::Bool(true) => out.push(element),
                VmValue::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Ok(VmValue::list(out))
    }

    /// Returns the number of elements in the input sequence for which the given `predicate` returns `true`.
    pub fn count(
        seq: ndc_vm::value::SeqValue,
        predicate: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut out = 0i64;
        for element in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("count requires a sequence"))?
        {
            match predicate.call(vec![element]).map_err(|e| anyhow!(e))? {
                VmValue::Bool(true) => out += 1,
                VmValue::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Ok(VmValue::Int(out))
    }

    /// Returns the value of the first element for which the `predicate` is true for the given input sequence.
    pub fn find(
        seq: ndc_vm::value::SeqValue,
        predicate: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        for element in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("find requires a sequence"))?
        {
            match predicate
                .call(vec![element.clone()])
                .map_err(|e| anyhow!(e))?
            {
                VmValue::Bool(true) => return Ok(element),
                VmValue::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Err(anyhow!("find did not find anything"))
    }

    /// Returns the first index of the element for which the `predicate` is true in the input sequence.
    pub fn locate(
        seq: ndc_vm::value::SeqValue,
        predicate: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        for (idx, element) in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("locate requires a sequence"))?
            .enumerate()
        {
            match predicate.call(vec![element]).map_err(|e| anyhow!(e))? {
                VmValue::Bool(true) => return Ok(VmValue::Int(idx as i64)),
                VmValue::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean")),
            }
        }
        Err(anyhow!("locate did not find anything"))
    }

    /// Returns the first index of the element or produces an error
    #[function(name = "locate")]
    pub fn locate_element(
        seq: ndc_vm::value::SeqValue,
        element: ndc_vm::value::Value,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        for (idx, el) in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("locate requires a sequence"))?
            .enumerate()
        {
            if el == element {
                return Ok(VmValue::Int(idx as i64));
            }
        }
        Err(anyhow!("locate did not find anything"))
    }

    /// Returns `true` if the `predicate` is true for none of the elements in `seq`.
    pub fn none(
        seq: ndc_vm::value::SeqValue,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("none requires a sequence"))?
        {
            match function.call(vec![item]).map_err(|e| anyhow!(e))? {
                VmValue::Bool(true) => return Ok(VmValue::Bool(false)),
                VmValue::Bool(false) => {}
                v => {
                    return Err(anyhow!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ));
                }
            }
        }
        Ok(VmValue::Bool(true))
    }

    /// Returns `true` if the `predicate` is true for all the elements in `seq`.
    pub fn all(
        seq: ndc_vm::value::SeqValue,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("all requires a sequence"))?
        {
            match function.call(vec![item]).map_err(|e| anyhow!(e))? {
                VmValue::Bool(true) => {}
                VmValue::Bool(false) => return Ok(VmValue::Bool(false)),
                v => {
                    return Err(anyhow!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ));
                }
            }
        }
        Ok(VmValue::Bool(true))
    }

    /// Returns `true` if the `predicate` is true for any of the elements in `seq`.
    pub fn any(
        seq: ndc_vm::value::SeqValue,
        predicate: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("any requires a sequence"))?
        {
            match predicate.call(vec![item]).map_err(|e| anyhow!(e))? {
                VmValue::Bool(true) => return Ok(VmValue::Bool(true)),
                VmValue::Bool(false) => {}
                v => {
                    return Err(anyhow!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ));
                }
            }
        }
        Ok(VmValue::Bool(false))
    }

    /// Applies the function to each element in a sequence returning the result as a list.
    pub fn map(
        seq: ndc_vm::value::SeqValue,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut out = Vec::new();
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("map requires a sequence"))?
        {
            out.push(function.call(vec![item]).map_err(|e| anyhow!(e))?);
        }
        Ok(VmValue::list(out))
    }

    /// Applies a function to each item in a sequence, flattens the resulting sequences, and returns a single combined sequence.
    pub fn flat_map(
        seq: ndc_vm::value::SeqValue,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut out = Vec::new();
        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("flat_map requires a sequence"))?
        {
            let result = function.call(vec![item]).map_err(|e| anyhow!(e))?;
            let inner = result
                .try_into_iter()
                .ok_or_else(|| anyhow!("callable argument to flat_map must return a sequence"))?;
            out.extend(inner);
        }
        Ok(VmValue::list(out))
    }

    /// Returns the first element of the sequence or the `default` value otherwise.
    pub fn first_or(
        seq: ndc_vm::value::SeqValue,
        default: ndc_vm::value::Value,
    ) -> ndc_vm::value::Value {
        seq.try_into_iter()
            .and_then(|mut i| i.next())
            .unwrap_or(default)
    }

    /// Returns the first element of the sequence or the return value of the given function.
    pub fn first_or_else(
        seq: ndc_vm::value::SeqValue,
        default: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        if let Some(item) = seq.try_into_iter().and_then(|mut i| i.next()) {
            return Ok(item);
        }
        default.call(vec![]).map_err(|e| anyhow!(e))
    }

    /// Returns the `k` sized combinations of the given sequence `seq` as a list of tuples.
    pub fn combinations(
        seq: ndc_vm::value::SeqValue,
        k: i64,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let k = k as usize;
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("combinations requires a sequence"))?
                .combinations(k)
                .map(VmValue::tuple)
                .collect::<Vec<VmValue>>(),
        ))
    }

    /// Returns the `k` sized permutations of the given sequence `seq` as a list of tuples.
    pub fn permutations(
        seq: ndc_vm::value::SeqValue,
        k: i64,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let k = k as usize;
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("permutations requires a sequence"))?
                .permutations(k)
                .map(VmValue::tuple)
                .collect::<Vec<VmValue>>(),
        ))
    }

    /// Returns all prefixes of a sequence, each as a list.
    pub fn prefixes(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        // Special case for String — produce string prefixes instead of lists of chars.
        if let VmValue::Object(ref obj) = seq {
            if let Object::String(s) = obj.as_ref() {
                return Ok(VmValue::list(
                    s.borrow()
                        .chars()
                        .scan(String::new(), |acc, c| {
                            acc.push(c);
                            Some(VmValue::string(acc.clone()))
                        })
                        .collect(),
                ));
            }
        }
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("prefixes requires a sequence"))?
                .scan(Vec::new(), |acc, item| {
                    acc.push(item);
                    Some(VmValue::list(acc.clone()))
                })
                .collect(),
        ))
    }

    /// Returns all suffixes of a sequence, each as a list; for strings, returns all trailing substrings.
    pub fn suffixes(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        // Special case for String — produce string suffixes instead of lists of chars.
        if let VmValue::Object(ref obj) = seq {
            if let Object::String(s) = obj.as_ref() {
                let borrowed = s.borrow();
                return Ok(VmValue::list(
                    borrowed
                        .char_indices()
                        .map(|(i, _)| VmValue::string(borrowed[i..].to_string()))
                        .collect(),
                ));
            }
        }
        let out: Vec<VmValue> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("suffixes requires a sequence"))?
            .collect();
        Ok(VmValue::list(
            (0..out.len())
                .map(|i| VmValue::list(out[i..].to_vec()))
                .collect(),
        ))
    }

    /// Transposes a sequence of sequences, turning rows into columns, and returns the result as a list of lists.
    pub fn transposed(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        let main: Vec<VmValue> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("transposed requires a sequence"))?
            .collect();
        let mut iterators: Vec<Box<dyn Iterator<Item = VmValue>>> = Vec::new();
        for elem in main {
            iterators.push(Box::new(elem.try_into_iter().ok_or_else(|| {
                anyhow!("elements of transposed sequence must be iterable")
            })?));
        }
        let mut out = Vec::new();
        loop {
            let row: Vec<VmValue> = iterators.iter_mut().filter_map(|i| i.next()).collect();
            if row.is_empty() {
                return Ok(VmValue::list(out));
            }
            out.push(VmValue::list(row));
        }
    }

    /// Return a list of all windows, wrapping back to the first elements when the window would otherwise exceed the length of source list, producing tuples of size 2.
    pub fn circular_tuple_windows(
        seq: ndc_vm::value::SeqValue,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("circular_tuple_windows requires a sequence"))?
                .collect::<Vec<_>>()
                .iter()
                .circular_tuple_windows::<(_, _)>()
                .map(|(a, b)| VmValue::tuple(vec![a.clone(), b.clone()]))
                .collect::<Vec<_>>(),
        ))
    }

    /// Returns a list of all size-2 windows in `seq`.
    pub fn pairwise(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("pairwise requires a sequence"))?
                .collect::<Vec<_>>()
                .windows(2)
                .map(|w| VmValue::list(w.to_vec()))
                .collect::<Vec<_>>(),
        ))
    }

    /// Applies a function to each pair of consecutive elements in a sequence and returns the results as a list.
    #[function(name = "pairwise")]
    pub fn pairwise_map(
        seq: ndc_vm::value::SeqValue,
        function: &VmCallable<'_>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let main: Vec<VmValue> = seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("pairwise requires a sequence"))?
            .collect();
        let mut out = Vec::with_capacity(main.len().saturating_sub(1));
        for (a, b) in main.into_iter().tuple_windows() {
            out.push(function.call(vec![a, b]).map_err(|e| anyhow!(e))?);
        }
        Ok(VmValue::list(out))
    }

    /// Returns a list of all contiguous windows of `length` size. The windows overlap. If the `seq` is shorter than size, the iterator returns no values.
    pub fn windows(
        seq: ndc_vm::value::SeqValue,
        length: i64,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let length = length as usize;
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("windows requires a sequence"))?
                .collect::<Vec<VmValue>>()
                .windows(length)
                .map(|w| VmValue::list(w.to_vec()))
                .collect::<Vec<VmValue>>(),
        ))
    }

    /// Return a list that represents the powerset of the elements of `seq`.
    ///
    /// The powerset of a set contains all subsets including the empty set and the full input set. A powerset has length `2^n` where `n` is the length of the input set.
    /// Each list produced by this function represents a subset of the elements in the source sequence.
    pub fn subsequences(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("subsequences requires a sequence"))?
                .powerset()
                .map(VmValue::list)
                .collect::<Vec<VmValue>>(),
        ))
    }

    /// Return a list that represents the powerset of the elements of `seq` that are exactly `length` long.
    #[function(name = "subsequences")]
    pub fn subsequences_len(
        seq: ndc_vm::value::SeqValue,
        length: i64,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let length = length as usize;
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("subsequences requires a sequence"))?
                .powerset()
                .filter(|x| x.len() == length)
                .map(VmValue::list)
                .collect::<Vec<VmValue>>(),
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
    pub fn multi_cartesian_product(
        seq: ndc_vm::value::SeqValue,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        let mut iterators = Vec::new();
        for elem in seq
            .try_into_iter()
            .ok_or_else(|| anyhow!("multi_cartesian_product requires a sequence"))?
        {
            let inner: Vec<VmValue> = elem
                .try_into_iter()
                .ok_or_else(|| anyhow!("elements of sequence must be iterable"))?
                .collect_vec();
            iterators.push(inner.into_iter());
        }
        Ok(VmValue::list(
            iterators
                .into_iter()
                .multi_cartesian_product()
                .map(VmValue::list)
                .collect_vec(),
        ))
    }

    /// Split the input sequence into evenly sized chunks. If the input length of the sequence
    /// is not dividable by the chunk_size the last chunk will contain fewer elements.
    pub fn chunks(
        seq: ndc_vm::value::SeqValue,
        chunk_size: usize,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        if chunk_size == 0 {
            return Err(anyhow!("chunk size must be non-zero"));
        }
        Ok(VmValue::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("chunks requires a sequence"))?
                .chunks(chunk_size)
                .into_iter()
                .map(|chunk| VmValue::list(chunk.collect_vec()))
                .collect_vec(),
        ))
    }

    #[function(return_type = Iterator<Value>)]
    pub fn repeat(value: ndc_vm::value::Value) -> ndc_vm::value::Value {
        ndc_vm::value::Value::iterator(Rc::new(std::cell::RefCell::new(ndc_vm::RepeatIter::new(
            value,
        ))))
    }

    #[function(name = "repeat", return_type = Iterator<Value>)]
    pub fn repeat_times(value: ndc_vm::value::Value, times: usize) -> ndc_vm::value::Value {
        ndc_vm::value::Value::iterator(Rc::new(std::cell::RefCell::new(
            ndc_vm::RepeatIter::new_limited(value, times),
        )))
    }
}

fn by_key(seq: VmValue, func: &VmCallable<'_>, better: Ordering) -> anyhow::Result<VmValue> {
    let mut best_value: Option<VmValue> = None;
    let mut best_key: Option<VmValue> = None;
    for value in seq
        .try_into_iter()
        .ok_or_else(|| anyhow!("sequence is required"))?
    {
        let new_key = func.call(vec![value.clone()]).map_err(|e| anyhow!(e))?;
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

fn by_comp(seq: VmValue, comp: &VmCallable<'_>, better: Ordering) -> anyhow::Result<VmValue> {
    let mut best: Option<VmValue> = None;
    for value in seq
        .try_into_iter()
        .ok_or_else(|| anyhow!("sequence is required"))?
    {
        let is_better = match &best {
            None => true,
            Some(current) => {
                let result = comp
                    .call(vec![value.clone(), current.clone()])
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
    iter: impl Iterator<Item = VmValue>,
    initial: VmValue,
    function: &VmCallable<'_>,
) -> anyhow::Result<VmValue> {
    let mut acc = initial;
    for item in iter {
        acc = function.call(vec![acc, item]).map_err(|e| anyhow!(e))?;
    }
    Ok(acc)
}

pub mod extra {
    use anyhow::anyhow;
    use itertools::izip;

    use ndc_interpreter::function::{FunctionBuilder, StaticType};
    use ndc_interpreter::{
        environment::Environment, function::FunctionBody, iterator::mut_value_to_iterator,
        value::Value,
    };

    pub fn register(env: &mut Environment) {
        env.declare_global_fn(
                FunctionBuilder::default()
                    .name("zip".to_string())
                    .documentation("Combines multiple sequences (or iterables) into a single sequence of tuples, where the ith tuple contains the ith element from each input sequence.\n\nIf the input sequences are of different lengths, the resulting sequence is truncated to the length of the shortest input.".to_string())
                    .body(FunctionBody::generic(
                        ndc_interpreter::function::TypeSignature::Variadic,
                        StaticType::List(Box::new(StaticType::Tuple(vec![StaticType::Any, StaticType::Any]))),
                        |args, _env| match args {
                            [_] => {
                                Err(anyhow!("zip must be called with 2 or more arguments").into())
                            }
                            [a, b] => {
                                let a = mut_value_to_iterator(a)?;
                                let b = mut_value_to_iterator(b)?;
                                let out = a
                                    .zip(b)
                                    .map(|(a, b)| Value::tuple(vec![a, b]))
                                    .collect::<Vec<Value>>();
                                Ok(Value::list(out))
                            }
                            [a, b, c] => {
                                let a = mut_value_to_iterator(a)?;
                                let b = mut_value_to_iterator(b)?;
                                let c = mut_value_to_iterator(c)?;
                                let mut out = Vec::new();
                                for (a, b, c) in izip!(a, b, c) {
                                    out.push(Value::tuple(vec![a, b, c]));
                                }
                                Ok(Value::list(out))
                            }
                            values => {
                                // HOLY HEAP ALLOCATION BATMAN!
                                // This branch can probably lose some heap allocations if I had 50 more IQ points
                                let mut lists = Vec::with_capacity(values.len());
                                for value in values.iter_mut() {
                                    lists.push(mut_value_to_iterator(value)?.collect::<Vec<_>>());
                                }
                                let out_len = lists.iter().map(Vec::len).min().unwrap();
                                let mut out = Vec::with_capacity(out_len);

                                for idx in 0..out_len {
                                    let mut tup = Vec::with_capacity(lists.len());
                                    for (list_idx, _) in lists.iter().enumerate() {
                                        tup.insert(list_idx, lists[list_idx][idx].clone());
                                    }
                                    out.insert(idx, Value::tuple(tup));
                                }

                                Ok(Value::list(out))
                            }
                        },
                    ))
                    .build()
                    .expect("function definitions must be valid"),
        );
    }
}
