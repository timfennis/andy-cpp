#![allow(clippy::ptr_arg)]

use crate::interpreter::iterator::{MutableValueIntoIterator, mut_seq_to_iterator};
use crate::interpreter::sequence::Sequence;
use crate::{
    compare::FallibleOrd,
    interpreter::{evaluate::EvaluationResult, function::Callable, value::Value},
};
use anyhow::anyhow;
use itertools::Itertools;
use ndc_macros::export_module;
use std::cmp::Ordering;
use std::rc::Rc;

trait TryCompare<R> {
    type Error;
    fn try_min(&mut self) -> Result<R, Self::Error>;
    fn try_max(&mut self) -> Result<R, Self::Error>;
}

impl<C, T, R> TryCompare<R> for C
where
    C: Iterator<Item = T>,
    T: FallibleOrd<Error = anyhow::Error>,
    T: Into<R>,
{
    type Error = anyhow::Error;

    fn try_min(&mut self) -> Result<R, Self::Error> {
        self.try_fold(None::<T>, |a, b| match a {
            None => Ok(Some(b)),
            Some(a) => a.try_cmp(&b).map(|o| match o {
                Ordering::Greater => Some(b),
                Ordering::Equal | Ordering::Less => Some(a),
            }),
        })
        .and_then(|x| {
            x.map(Into::into)
                .ok_or_else(|| anyhow::anyhow!("empty input to min"))
        })
    }

    fn try_max(&mut self) -> Result<R, Self::Error> {
        self.try_fold(None::<T>, |a, b| match a {
            None => Ok(Some(b)),
            Some(a) => a.try_cmp(&b).map(|o| match o {
                Ordering::Less => Some(b),
                Ordering::Equal | Ordering::Greater => Some(a),
            }),
        })
        .and_then(|x| {
            x.map(Into::into)
                .ok_or_else(|| anyhow::anyhow!("empty input to max"))
        })
    }
}

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

#[export_module]
mod inner {
    use crate::interpreter::iterator::{Repeat, ValueIterator};
    use crate::interpreter::{function::FunctionCarrier, iterator::mut_value_to_iterator};
    use std::cell::RefCell;

    #[function(name = "in")]
    pub fn op_contains(elem: &Value, seq: &Sequence) -> bool {
        seq.contains(elem)
    }

    /// Returns the highest element in the sequence.
    pub fn max(seq: &Sequence) -> anyhow::Result<Value> {
        match seq {
            Sequence::String(s) => s
                .try_borrow()?
                .chars()
                .max()
                .ok_or_else(|| anyhow::anyhow!("empty input to max"))
                .map(|v| Value::from(String::from(v))),
            Sequence::List(l) => l.try_borrow()?.iter().try_max(),
            Sequence::Tuple(l) => l.iter().try_max(),
            Sequence::Map(map, _) => map.borrow().keys().try_max(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_max(),
            Sequence::MaxHeap(h) => h
                .borrow()
                .peek()
                .map(|hv| hv.0.clone())
                .ok_or_else(|| anyhow::anyhow!("empty input to max")),
            // I think this is always going to be O(n)
            Sequence::MinHeap(_) => Err(anyhow::anyhow!("not supported for MinHeap")),
            Sequence::Deque(d) => d.try_borrow()?.iter().try_max(),
        }
    }
    /// Returns the element for which the key function returns the highest value.
    pub fn max_by_key(seq: &mut Sequence, func: &Callable<'_>) -> EvaluationResult {
        by_key(seq, func, Ordering::Greater)
    }

    /// Returns the element for which the key function returns the lowest value.
    pub fn min_by_key(seq: &mut Sequence, func: &Callable<'_>) -> EvaluationResult {
        by_key(seq, func, Ordering::Less)
    }

    /// Returns the maximum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number. A positive result means the
    /// first argument is greater than the second, a negative result means the first argument is
    /// less than the second, and zero means they are equal.
    pub fn max_by(seq: &mut Sequence, comp: &Callable<'_>) -> EvaluationResult {
        by_comp(seq, comp, Ordering::Greater)
    }

    /// Returns the minimum element using a comparator function.
    ///
    /// The comparator function takes two elements and returns a number. A positive result means the
    /// first argument is greater than the second, a negative result means the first argument is
    /// less than the second, and zero means they are equal.
    pub fn min_by(seq: &mut Sequence, comp: &Callable<'_>) -> EvaluationResult {
        by_comp(seq, comp, Ordering::Less)
    }

    /// Returns the lowest element in the sequence.
    pub fn min(seq: &Sequence) -> anyhow::Result<Value> {
        match seq {
            Sequence::String(s) => s
                .try_borrow()?
                .chars()
                .min()
                .ok_or_else(|| anyhow::anyhow!("empty input to min"))
                .map(|v| Value::from(String::from(v))),
            Sequence::List(l) => l.try_borrow()?.iter().try_min(),
            Sequence::Tuple(l) => l.iter().try_min(),
            Sequence::Map(map, _) => map.borrow().keys().try_min(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_min(),
            // I think this is always going to be O(n)
            Sequence::MaxHeap(_) => Err(anyhow::anyhow!("not supported for MaxHeap")),
            Sequence::MinHeap(h) => h
                .borrow()
                .peek()
                .map(|hv| hv.0.0.clone())
                .ok_or_else(|| anyhow::anyhow!("empty input to max")),
            Sequence::Deque(d) => d.try_borrow()?.iter().try_min(),
        }
    }

    /// Sorts the input sequence in place.
    ///
    /// This function only works for strings and lists and will throw errors otherwise.
    pub fn sort(seq: &mut Sequence) -> anyhow::Result<()> {
        match seq {
            Sequence::String(str) => {
                let r = &mut *str.borrow_mut();
                *r = r.chars().sorted().collect::<String>();
            }
            Sequence::List(list) => {
                let mut m = list.borrow_mut();
                try_sort_by(&mut m, Value::try_cmp)?;
            }
            Sequence::Tuple(_) => return Err(anyhow!("tuple cannot be sorted in place")),
            Sequence::Map(_, _) => return Err(anyhow!("map cannot be sorted in place")),
            Sequence::Iterator(_) => return Err(anyhow!("iterator cannot be sorted in place")),
            Sequence::MaxHeap(_) | Sequence::MinHeap(_) => {
                return Err(anyhow!("heap is already sorted"));
            }
            Sequence::Deque(_) => return Err(anyhow!("deque cannot be sorted in place")),
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
    pub fn sort_by(list: &mut Vec<Value>, comp: &Callable<'_>) -> EvaluationResult {
        try_sort_by::<FunctionCarrier>(list, |left, right| {
            let ret = comp.call(&mut [left.clone(), right.clone()])?;

            Ok(ret.try_cmp(&Value::from(0))?)
        })?;
        Ok(Value::unit())
    }

    /// Returns a sorted copy of the input sequence as a list.
    #[function(return_type = Vec<Value>)]
    pub fn sorted(seq: &mut Sequence) -> anyhow::Result<Value> {
        let mut list = mut_seq_to_iterator(seq).collect::<Vec<Value>>();
        try_sort_by(&mut list, Value::try_cmp)?;
        Ok(Value::list(list))
    }

    /// Sorts the given sequence using a comparing function, returning a new list with the elements in the sorted order.
    ///
    /// The result of the comparing function is compared to the number `0` to determine the relative ordering such that:
    /// - for values lower than `0` the first argument is smaller than the second argument
    /// - for values higher than `0` the first argument is greater than the second argument
    /// - for values equal to `0` the first argument is equal to the second argument
    #[function(return_type = Vec<Value>)]
    pub fn sorted_by(seq: &mut Sequence, comp: &Callable<'_>) -> EvaluationResult {
        let mut list = mut_seq_to_iterator(seq).collect::<Vec<Value>>();
        try_sort_by::<FunctionCarrier>(&mut list, |left, right| {
            let ret = comp.call(&mut [left.clone(), right.clone()])?;

            Ok(ret.try_cmp(&Value::from(0))?)
        })?;
        Ok(Value::list(list))
    }

    /// Returns the length of a string in bytes.
    pub fn byte_len(str: &str) -> usize {
        str.len()
    }

    /// Returns the length of the sequence, for strings this returns the number of UTF-8 characters.
    pub fn len(seq: &Sequence) -> anyhow::Result<usize> {
        match seq.length() {
            Some(n) => Ok(n),
            None => Err(anyhow!(
                "cannot determine the length of {}",
                seq.static_type()
            )),
        }
    }

    /// Enumerates the given sequence returning a list of tuples where the first element of the tuple is the index of the element in the input sequence.

    #[function(return_type = Vec<(i64, Value)>)]
    pub fn enumerate(seq: &mut Sequence) -> Value {
        match seq {
            Sequence::String(s) => Value::list(
                s.borrow()
                    .chars()
                    .enumerate()
                    .map(|(index, char)| Value::tuple(vec![Value::from(index), Value::from(char)]))
                    .collect::<Vec<Value>>(),
            ),
            Sequence::Map(map, _) => Value::list(
                map.borrow()
                    .iter()
                    .enumerate()
                    .map(|(index, (key, value))| {
                        Value::Sequence(Sequence::Tuple(Rc::new(vec![
                            Value::from(index),
                            Value::Sequence(Sequence::Tuple(Rc::new(vec![
                                Value::clone(key),
                                Value::clone(value),
                            ]))),
                        ])))
                    })
                    .collect::<Vec<Value>>(),
            ),
            // TODO: This entire branch is so cringe, why are we even trying to use iterators if we do shit like this
            Sequence::Iterator(rc) => {
                let mut iter = rc.borrow_mut();
                let mut out = Vec::new();
                for (idx, value) in iter.by_ref().enumerate() {
                    out.push(Value::Sequence(Sequence::Tuple(Rc::new(vec![
                        Value::from(idx),
                        value,
                    ]))));
                }

                Value::list(out)
            }
            seq => Value::list(
                mut_seq_to_iterator(seq)
                    .enumerate()
                    .map(|(idx, value)| Value::tuple(vec![Value::from(idx), value]))
                    .collect::<Vec<_>>(),
            ),
        }
    }

    /// Reduces/folds the given sequence using the given combining function and a custom initial value.
    #[function(return_type = Vec<_>)]
    pub fn fold(seq: &mut Sequence, initial: Value, function: &Callable<'_>) -> EvaluationResult {
        fold_iterator(mut_seq_to_iterator(seq), initial, function)
    }

    /// Reduces/folds the given sequence using the given combining function.
    #[function(return_type = Value)]
    pub fn reduce(seq: &mut Sequence, function: &Callable<'_>) -> EvaluationResult {
        let mut iterator = mut_seq_to_iterator(seq);
        let fst = iterator
            .next()
            .ok_or_else(|| anyhow!("first argument to reduce must not be empty"))?;

        fold_iterator(iterator, fst, function)
    }

    /// Filters the given sequence using the `predicate`.
    #[function(return_type = Vec<_>)]
    pub fn filter(seq: &mut Sequence, predicate: &Callable<'_>) -> EvaluationResult {
        let iterator = mut_seq_to_iterator(seq);
        let mut out = Vec::new();
        for element in iterator {
            out.push(element);
            let last_idx = out.len() - 1;
            let result = predicate.call(&mut out[last_idx..])?;
            match result {
                Value::Bool(true) => {}
                Value::Bool(false) => {
                    out.pop();
                }
                _ => return Err(anyhow!("return value of predicate must be a boolean").into()),
            }
        }

        Ok(Value::list(out))
    }

    /// Returns the number of elements in the input sequence for which the given `predicate` returns `true`.
    #[function(return_type = i64)]
    pub fn count(seq: &mut Sequence, predicate: &Callable<'_>) -> EvaluationResult {
        let iterator = mut_seq_to_iterator(seq);
        let mut out = 0;
        for element in iterator {
            let result = predicate.call(&mut [element])?;
            match result {
                Value::Bool(true) => {
                    out += 1;
                }
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean").into()),
            }
        }

        Ok(Value::number(out))
    }

    /// Returns the value of the first element for which the `predicate` is true for the given input sequence.
    #[function(return_type = Value)]
    pub fn find(seq: &mut Sequence, predicate: &Callable<'_>) -> EvaluationResult {
        let iterator = mut_seq_to_iterator(seq);
        for element in iterator {
            let result = predicate.call(&mut [element.clone()])?;
            match result {
                Value::Bool(true) => return Ok(element),
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean").into()),
            }
        }

        Err(anyhow!("find did not find anything").into())
    }

    /// Returns the first index of the element for which the `predicate` is true in the input sequence.
    #[function(return_type = usize)]
    pub fn locate(seq: &mut Sequence, predicate: &Callable<'_>) -> EvaluationResult {
        let iterator = mut_seq_to_iterator(seq);
        for (idx, element) in iterator.enumerate() {
            let result = predicate.call(&mut [element])?;
            match result {
                Value::Bool(true) => return Ok(Value::from(idx)),
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean").into()),
            }
        }

        Err(anyhow!("locate did not find anything").into())
    }

    /// Returns the first index of the element or produces an error
    #[function(name = "locate", return_type = usize)]
    pub fn locate_element(seq: &mut Sequence, element: &Value) -> EvaluationResult {
        let iterator = mut_seq_to_iterator(seq);
        for (idx, el) in iterator.enumerate() {
            if &el == element {
                return Ok(Value::from(idx));
            }
        }

        Err(anyhow!("locate did not find anything").into())
    }

    /// Returns `true` if the `predicate` is true for none of the elements in `seq`.
    #[function(return_type = bool)]
    pub fn none(seq: &mut Sequence, function: &Callable<'_>) -> EvaluationResult {
        for item in mut_seq_to_iterator(seq) {
            match function.call(&mut [item])? {
                Value::Bool(true) => return Ok(Value::Bool(false)),
                Value::Bool(false) => {}
                v => {
                    return Err(anyhow!(format!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ))
                    .into());
                }
            }
        }

        Ok(Value::Bool(true))
    }
    /// Returns `true` if the `predicate` is true for all the elements in `seq`.
    #[function(return_type = bool)]
    pub fn all(seq: &mut Sequence, function: &Callable<'_>) -> EvaluationResult {
        for item in mut_seq_to_iterator(seq) {
            match function.call(&mut [item])? {
                Value::Bool(true) => {}
                Value::Bool(false) => return Ok(Value::Bool(false)),
                v => {
                    return Err(anyhow!(format!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ))
                    .into());
                }
            }
        }

        Ok(Value::Bool(true))
    }

    /// Returns `true` if the `predicate` is true for any of the elements in `seq`.
    #[function(return_type = bool)]
    pub fn any(seq: &mut Sequence, predicate: &Callable<'_>) -> EvaluationResult {
        for item in mut_seq_to_iterator(seq) {
            match predicate.call(&mut [item])? {
                Value::Bool(true) => return Ok(Value::Bool(true)),
                Value::Bool(false) => {}
                v => {
                    return Err(anyhow!(format!(
                        "invalid return type, predicate returned {}",
                        v.static_type()
                    ))
                    .into());
                }
            }
        }

        Ok(Value::Bool(false))
    }

    /// Applies the function to each element in a sequence returning the result as a list.
    #[function(return_type = Vec<_>)]
    pub fn map(seq: &mut Sequence, function: &Callable<'_>) -> EvaluationResult {
        let iterator = mut_seq_to_iterator(seq);
        let mut out = Vec::new();

        for item in iterator {
            out.push(function.call(&mut [item])?);
        }

        Ok(Value::list(out))
    }

    /// Applies a function to each item in a sequence, flattens the resulting sequences, and returns a single combined sequence.
    #[function(return_type = Vec<_>)]
    pub fn flat_map(seq: &mut Sequence, function: &Callable<'_>) -> EvaluationResult {
        // let iterator = ;
        let mut out = Vec::new();

        for item in mut_seq_to_iterator(seq) {
            let fnout = function.call(&mut [item])?;
            match fnout {
                Value::Sequence(mut inner_seq) => {
                    out.extend(mut_seq_to_iterator(&mut inner_seq));
                }
                _ => {
                    return Err(
                        anyhow!("callable argument to flat_map must return a sequence").into(),
                    );
                }
            }
        }

        Ok(Value::list(out))
    }

    /// Returns the first element of the sequence or the `default` value otherwise.
    pub fn first_or(seq: &mut Sequence, default: Value) -> Value {
        let mut iterator = mut_seq_to_iterator(seq);
        if let Some(item) = iterator.next() {
            item
        } else {
            default
        }
    }

    /// Returns the first element of the sequence or the return value of the given function.
    pub fn first_or_else(seq: &mut Sequence, default: &Callable<'_>) -> EvaluationResult {
        let mut iterator = mut_seq_to_iterator(seq);
        Ok(if let Some(item) = iterator.next() {
            item
        } else {
            default.call(&mut [])?
        })
    }

    /// Returns the `k` sized combinations of the given sequence `seq` as a list of tuples.
    #[function(return_type = Vec<_>)]
    pub fn combinations(seq: &mut Sequence, k: usize) -> Value {
        Value::list(
            mut_seq_to_iterator(seq)
                .combinations(k)
                .map(Value::tuple)
                .collect::<Vec<Value>>(),
        )
    }

    /// Returns the `k` sized permutations of the given sequence `seq` as a list of tuples.
    #[function(return_type = Vec<_>)]
    pub fn permutations(seq: &mut Sequence, k: usize) -> Value {
        Value::list(
            mut_seq_to_iterator(seq)
                .permutations(k)
                .map(Value::tuple)
                .collect::<Vec<Value>>(),
        )
    }

    /// Returns al prefixes of a sequence, each as a list.
    #[function(return_type = Vec<_>)]
    pub fn prefixes(seq: &mut Sequence) -> Value {
        // Special case for string which is more efficient and doesn't produce lists of characters
        if let Sequence::String(string) = &seq {
            return Value::list(
                string
                    .borrow()
                    .chars()
                    .scan(String::new(), |acc, item| {
                        // Item must be string!!
                        acc.push(item);
                        Some(Value::string(acc.clone()))
                    })
                    .collect::<Vec<Value>>(),
            );
        }

        let iterator = mut_seq_to_iterator(seq);

        Value::list(
            iterator
                .scan(Vec::new(), |acc, item| {
                    acc.push(item);
                    Some(Value::list(acc.clone()))
                })
                .collect::<Vec<Value>>(),
        )
    }

    /// Returns all suffixes of a sequence, each as a list; for strings, returns all trailing substrings.
    #[function(return_type = Vec<_>)]
    pub fn suffixes(seq: &mut Sequence) -> Value {
        // Special case for string which is more efficient and doesn't produce lists of characters
        if let Sequence::String(string) = &seq {
            return Value::list(
                string
                    .borrow()
                    .char_indices()
                    .map(|(idx, _)| Value::string(&string.borrow()[idx..]))
                    .collect::<Vec<Value>>(),
            );
        }

        let iterator = mut_seq_to_iterator(seq);
        let out = iterator.collect::<Vec<_>>();

        Value::list(
            (0..out.len())
                .map(|i| Value::list(out[i..].to_vec()))
                .collect::<Vec<Value>>(),
        )
    }

    /// Transposes a sequence of sequences, turning rows into columns, and returns the result as a list of lists.
    // TODO: right now transposed always produces a list, it probably should produce whatever the input type was (if possible)
    // TODO: this might not be the expected result for sets (since iterators over sets yield tuples)
    #[function(return_type = Vec<_>)]
    pub fn transposed(seq: &mut Sequence) -> EvaluationResult {
        let mut main = mut_seq_to_iterator(seq).collect::<Vec<_>>();
        let mut iterators = Vec::new();
        for iter in &mut main {
            iterators.push(mut_value_to_iterator(iter)?);
        }
        let mut out = Vec::new();
        loop {
            let row = iterators
                .iter_mut()
                .filter_map(Iterator::next)
                .collect::<Vec<_>>();
            if row.is_empty() {
                return Ok(Value::list(out));
            }
            out.push(Value::list(row));
        }
    }

    /// Return a list of all windows, wrapping back to the first elements when the window would otherwise exceed the length of source list, producing tuples of size 2.
    #[function(return_type = Vec<(Value, Value)>)]
    pub fn circular_tuple_windows(seq: &mut Sequence) -> Value {
        // TODO: this implementation probably clones a bit more than it needs to, but it's better tol
        //       have something than nothing
        Value::list(
            mut_seq_to_iterator(seq)
                .collect::<Vec<_>>()
                .iter()
                .circular_tuple_windows::<(_, _)>()
                .map(|(a, b)| Value::tuple(vec![a.clone(), b.clone()]))
                .collect::<Vec<_>>(),
        )
    }

    /// Returns a list of all size-2 windows in `seq`.
    #[function(return_type = Vec<(Value, Value)>)]
    pub fn pairwise(seq: &mut Sequence) -> Value {
        Value::list(
            mut_seq_to_iterator(seq)
                .collect::<Vec<_>>()
                .windows(2)
                .map(Value::tuple)
                .collect::<Vec<_>>(),
        )
    }

    /// Applies a function to each pair of consecutive elements in a sequence and returns the results as a list.
    #[function(name = "pairwise")]
    #[function(return_type = Vec<(Value, Value)>)]
    pub fn pairwise_map(seq: &mut Sequence, function: &Callable<'_>) -> EvaluationResult {
        let main = mut_seq_to_iterator(seq).collect::<Vec<_>>();

        let mut out = Vec::with_capacity(main.len() - 1);
        for (a, b) in main.into_iter().tuple_windows() {
            out.push(function.call(&mut [a, b])?);
        }

        Ok(Value::list(out))
    }

    /// Returns a list of all contiguous windows of `length` size. The windows overlap. If the `seq` is shorter than size, the iterator returns no values.
    #[function(return_type = Vec<Vec<Value>>)]
    pub fn windows(seq: &mut Sequence, length: usize) -> Value {
        Value::list(
            mut_seq_to_iterator(seq)
                .collect::<Vec<Value>>()
                .windows(length)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        )
    }

    /// Return a list that represents the powerset of the elements of `seq`.
    ///
    /// The powerset of a set contains all subsets including the empty set and the full input set. A powerset has length `2^n` where `n` is the length of the input set.
    /// Each list produced by this function represents a subset of the elements in the source sequence.
    #[function(return_type = Vec<Vec<Value>>)]
    pub fn subsequences(seq: &mut Sequence) -> Value {
        Value::list(
            mut_seq_to_iterator(seq)
                .powerset()
                .map(Value::list)
                .collect::<Vec<Value>>(),
        )
    }

    /// Return a list that represents the powerset of the elements of `seq` that are exactly `length` long.
    #[function(name = "subsequences")]
    #[function(return_type = Vec<Vec<Value>>)]
    pub fn subsequences_len(seq: &mut Sequence, length: usize) -> Value {
        Value::list(
            mut_seq_to_iterator(seq)
                .powerset()
                .filter(|x| x.len() == length)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        )
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
    #[function(return_type = Vec<Vec<Value>>)]
    pub fn multi_cartesian_product(seq: &mut Sequence) -> anyhow::Result<Value> {
        let mut iterators = Vec::new();

        for mut value in mut_seq_to_iterator(seq) {
            let iter = mut_value_to_iterator(&mut value)?.collect_vec().into_iter();
            iterators.push(iter);
        }

        let out = iterators
            .into_iter()
            .multi_cartesian_product()
            .map(Value::list)
            .collect_vec();

        Ok(Value::list(out))
    }

    /// Split the input sequence into evenly sized chunks. If the input length of the sequence
    /// is not dividable by the chunk_size the last chunk will contain fewer elements.
    #[function(return_type = Vec<Vec<Value>>)]
    pub fn chunks(seq: &mut Sequence, chunk_size: usize) -> anyhow::Result<Value> {
        if chunk_size == 0 {
            return Err(anyhow!("chunk size must be non-zero"));
        }

        let iter = mut_seq_to_iterator(seq);

        Ok(Value::list(
            iter.chunks(chunk_size)
                .into_iter()
                .map(|chunk| Value::list(chunk.collect_vec()))
                .collect_vec(),
        ))
    }

    #[function(return_type = Iterator<Value>)]
    pub fn repeat(value: Value) -> Value {
        Value::Sequence(Sequence::Iterator(Rc::new(RefCell::new(
            ValueIterator::Repeat(Repeat {
                value,
                cur: 0,
                limit: None,
            }),
        ))))
    }

    #[function(name = "repeat", return_type = Iterator<Value>)]
    pub fn repeat_times(value: Value, times: usize) -> Value {
        Value::Sequence(Sequence::Iterator(Rc::new(RefCell::new(
            ValueIterator::Repeat(Repeat {
                value,
                cur: 0,
                limit: Some(times),
            }),
        ))))
    }
}

fn by_key(seq: &mut Sequence, func: &Callable<'_>, better: Ordering) -> EvaluationResult {
    let mut best_value = None;
    let mut best_key: Option<Value> = None;

    for value in mut_seq_to_iterator(seq) {
        let new_key = func.call(&mut [value.clone()])?;
        let is_better = match &best_key {
            None => true,
            Some(current_best) => new_key.try_cmp(current_best)? == better,
        };
        if is_better {
            best_key = Some(new_key);
            best_value = Some(value);
        }
    }

    best_value.ok_or_else(|| anyhow::anyhow!("sequence was empty").into())
}

fn by_comp(seq: &mut Sequence, comp: &Callable<'_>, better: Ordering) -> EvaluationResult {
    let mut best: Option<Value> = None;

    for value in mut_seq_to_iterator(seq) {
        let is_better = match &best {
            None => true,
            Some(current) => {
                let result = comp.call(&mut [value.clone(), current.clone()])?;
                result.try_cmp(&Value::from(0))? == better
            }
        };
        if is_better {
            best = Some(value);
        }
    }

    best.ok_or_else(|| anyhow::anyhow!("sequence was empty").into())
}

fn fold_iterator(
    iterator: MutableValueIntoIterator<'_>,
    initial: Value,
    function: &Callable<'_>,
) -> EvaluationResult {
    let mut acc = initial;
    for item in iterator {
        acc = function.call(&mut [acc, item])?;
    }

    Ok(acc)
}

pub mod extra {
    use anyhow::anyhow;
    use itertools::izip;

    use crate::interpreter::function::{FunctionBuilder, StaticType};
    use crate::interpreter::{
        environment::Environment, function::FunctionBody, iterator::mut_value_to_iterator,
        value::Value,
    };

    pub fn register(env: &mut Environment) {
        env.declare_global_fn(
                FunctionBuilder::default()
                    .name("zip".to_string())
                    .documentation("Combines multiple sequences (or iterables) into a single sequence of tuples, where the ith tuple contains the ith element from each input sequence.\n\nIf the input sequences are of different lengths, the resulting sequence is truncated to the length of the shortest input.".to_string())
                    .body(FunctionBody::generic(
                        crate::interpreter::function::TypeSignature::Variadic,
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
