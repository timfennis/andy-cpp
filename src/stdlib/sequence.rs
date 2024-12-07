use crate::interpreter::iterator::{mut_seq_into_iterator, MutableValueIntoIterator};
use crate::interpreter::sequence::Sequence;
use crate::{
    compare::FallibleOrd,
    interpreter::{evaluate::EvaluationResult, function::Callable, value::Value},
};
use andy_cpp_macros::export_module;
use anyhow::anyhow;
use itertools::Itertools;
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
    use crate::interpreter::{function::FunctionCarrier, iterator::mut_value_to_iterator};

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
        }
    }
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
        }
    }

    pub fn sort(seq: &Sequence) -> anyhow::Result<Value> {
        match seq {
            Sequence::String(str) => {
                let r = &mut *str.borrow_mut();
                *r = r.chars().sorted().collect::<String>();
                Ok(Value::unit())
            }
            Sequence::List(list) => {
                let mut m = list.borrow_mut();
                try_sort_by(&mut m, Value::try_cmp)?;
                Ok(Value::unit())
            }
            Sequence::Tuple(_) => Err(anyhow!("tuple cannot be sorted in place")),
            Sequence::Map(_, _) => Err(anyhow!("map cannot be sorted in place")),
            Sequence::Iterator(_) => Err(anyhow!("iterator cannot be sorted in place")),
        }
    }

    pub fn sort_by(list: &mut Vec<Value>, comp: &Callable) -> EvaluationResult {
        try_sort_by::<FunctionCarrier>(list, |left, right| {
            let ret = comp.call(&mut [left.clone(), right.clone()])?;

            Ok(ret.try_cmp(&Value::from(0))?)
        })?;
        Ok(Value::unit())
    }

    pub fn sorted(seq: &mut Sequence) -> anyhow::Result<Value> {
        let mut list = mut_seq_into_iterator(seq).collect::<Vec<Value>>();
        try_sort_by(&mut list, Value::try_cmp)?;
        Ok(Value::list(list))
    }

    pub fn sorted_by(seq: &mut Sequence, comp: &Callable) -> EvaluationResult {
        let mut list = mut_seq_into_iterator(seq).collect::<Vec<Value>>();
        try_sort_by::<FunctionCarrier>(&mut list, |left, right| {
            let ret = comp.call(&mut [left.clone(), right.clone()])?;

            Ok(ret.try_cmp(&Value::from(0))?)
        })?;
        Ok(Value::list(list))
    }

    pub fn byte_len(str: &str) -> usize {
        str.len()
    }

    pub fn len(seq: &Sequence) -> anyhow::Result<usize> {
        match seq {
            Sequence::String(s) => Ok(s.borrow().chars().count()),
            Sequence::List(l) => Ok(l.borrow().len()),
            Sequence::Tuple(t) => Ok(t.len()),
            Sequence::Map(d, _) => Ok(d.borrow().len()),
            Sequence::Iterator(_) => Err(anyhow!("cannot determine the length of an iterator")),
        }
    }

    pub fn enumerate(seq: &Sequence) -> Value {
        match seq {
            Sequence::String(s) => s
                .borrow()
                .chars()
                .enumerate()
                .map(|(index, char)| {
                    Value::Sequence(Sequence::Tuple(Rc::new(vec![
                        Value::from(index),
                        Value::from(char),
                    ])))
                })
                .collect::<Vec<Value>>()
                .into(),
            Sequence::List(list) => list
                .borrow()
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    Value::Sequence(Sequence::Tuple(Rc::new(vec![
                        Value::from(index),
                        Value::clone(value),
                    ])))
                })
                .collect::<Vec<Value>>()
                .into(),
            Sequence::Tuple(tuple) => tuple
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    Value::Sequence(Sequence::Tuple(Rc::new(vec![
                        Value::from(index),
                        Value::clone(value),
                    ])))
                })
                .collect::<Vec<Value>>()
                .into(),
            Sequence::Map(map, _) => map
                .borrow()
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
                .collect::<Vec<Value>>()
                .into(),
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

                out.into()
            }
        }
    }

    pub fn fold(seq: &mut Sequence, initial: Value, function: &Callable) -> EvaluationResult {
        fold_iterator(mut_seq_into_iterator(seq), initial, function)
    }

    pub fn reduce(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        let mut iterator = mut_seq_into_iterator(seq);
        let fst = iterator
            .next()
            .ok_or_else(|| anyhow!("first argument to reduce must not be empty"))?;

        fold_iterator(iterator, fst, function)
    }

    pub fn filter(seq: &mut Sequence, predicate: &Callable) -> EvaluationResult {
        let iterator = mut_seq_into_iterator(seq);
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

        Ok(out.into())
    }

    pub fn count(seq: &mut Sequence, predicate: &Callable) -> EvaluationResult {
        let iterator = mut_seq_into_iterator(seq);
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

        Ok(out.into())
    }

    pub fn find(seq: &mut Sequence, predicate: &Callable) -> EvaluationResult {
        let iterator = mut_seq_into_iterator(seq);
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

    pub fn locate(seq: &mut Sequence, predicate: &Callable) -> EvaluationResult {
        let iterator = mut_seq_into_iterator(seq);
        for (idx, element) in iterator.enumerate() {
            let result = predicate.call(&mut [element])?;
            match result {
                Value::Bool(true) => return Ok(Value::from(idx)),
                Value::Bool(false) => {}
                _ => return Err(anyhow!("return value of predicate must be a boolean").into()),
            }
        }

        Err(anyhow!("find did not find anything").into())
    }

    pub fn none(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        for item in mut_seq_into_iterator(seq) {
            match function.call(&mut [item])? {
                Value::Bool(true) => return Ok(Value::Bool(false)),
                Value::Bool(false) => {}
                v => {
                    return Err(anyhow!(format!(
                        "invalid return type, predicate returned {}",
                        v.value_type()
                    ))
                    .into());
                }
            }
        }

        Ok(Value::Bool(true))
    }
    pub fn all(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        for item in mut_seq_into_iterator(seq) {
            match function.call(&mut [item])? {
                Value::Bool(true) => {}
                Value::Bool(false) => return Ok(Value::Bool(false)),
                v => {
                    return Err(anyhow!(format!(
                        "invalid return type, predicate returned {}",
                        v.value_type()
                    ))
                    .into());
                }
            }
        }

        Ok(Value::Bool(true))
    }

    pub fn any(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        for item in mut_seq_into_iterator(seq) {
            match function.call(&mut [item])? {
                Value::Bool(true) => return Ok(Value::Bool(true)),
                Value::Bool(false) => {}
                v => {
                    return Err(anyhow!(format!(
                        "invalid return type, predicate returned {}",
                        v.value_type()
                    ))
                    .into());
                }
            }
        }

        Ok(Value::Bool(false))
    }

    pub fn map(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        let iterator = mut_seq_into_iterator(seq);
        let mut out = Vec::new();

        for item in iterator {
            out.push(function.call(&mut [item])?);
        }

        Ok(Value::from(out))
    }

    pub fn flat_map(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        // let iterator = ;
        let mut out = Vec::new();

        for item in mut_seq_into_iterator(seq) {
            let fnout = function.call(&mut [item])?;
            match fnout {
                Value::Sequence(mut inner_seq) => {
                    // TODO: would it be (much?) faster if we iterate over the iterator and append the elements individually?
                    out.extend(mut_seq_into_iterator(&mut inner_seq));
                }
                _ => return Err(anyhow!("callable must return a sequence").into()),
            }
        }

        Ok(Value::from(out))
    }

    pub fn first_or(seq: &mut Sequence, default: Value) -> Value {
        let mut iterator = mut_seq_into_iterator(seq);
        if let Some(item) = iterator.next() {
            item
        } else {
            default
        }
    }

    pub fn first_or_else(seq: &mut Sequence, default: &Callable) -> EvaluationResult {
        let mut iterator = mut_seq_into_iterator(seq);
        Ok(if let Some(item) = iterator.next() {
            item
        } else {
            default.call(&mut [])?
        })
    }

    pub fn combinations(seq: &mut Sequence, k: usize) -> Value {
        Value::list(
            mut_seq_into_iterator(seq)
                .combinations(k)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        )
    }

    pub fn permutations(seq: &mut Sequence, k: usize) -> Value {
        Value::list(
            mut_seq_into_iterator(seq)
                .permutations(k)
                .map(Value::list)
                .collect::<Vec<Value>>(),
        )
    }

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

        let iterator = mut_seq_into_iterator(seq);

        Value::list(
            iterator
                .scan(Vec::new(), |acc, item| {
                    acc.push(item);
                    Some(Value::list(acc.clone()))
                })
                .collect::<Vec<Value>>(),
        )
    }

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

        let iterator = mut_seq_into_iterator(seq);
        let out = iterator.collect::<Vec<_>>();

        Value::list(
            (0..out.len())
                .map(|i| Value::list(out[i..].to_vec()))
                .collect::<Vec<Value>>(),
        )
    }

    pub fn transposed(seq: &mut Sequence) -> EvaluationResult {
        let mut main = mut_seq_into_iterator(seq).collect::<Vec<_>>();
        let mut iterators = Vec::new();
        for iter in &mut main {
            iterators.push(mut_value_to_iterator(iter)?);
        }
        let mut out = Vec::new();
        loop {
            let row = iterators
                .iter_mut()
                .filter_map(std::iter::Iterator::next)
                .collect::<Vec<_>>();
            if row.is_empty() {
                return Ok(Value::list(out));
            }
            out.push(Value::list(row));
        }
    }

    pub fn pairwise(seq: &mut Sequence) -> Vec<Value> {
        mut_seq_into_iterator(seq)
            .collect::<Vec<_>>()
            .windows(2)
            .map(Value::tuple)
            .collect::<Vec<_>>()
    }

    // TODO: this implementation probably clones a bit more than it needs to, but it's better tol
    //       have something than nothing
    pub fn circular_tuple_windows(seq: &mut Sequence) -> Vec<Value> {
        mut_seq_into_iterator(seq)
            .collect::<Vec<_>>()
            .iter()
            .circular_tuple_windows::<(_, _)>()
            .map(|(a, b)| Value::tuple(vec![a.clone(), b.clone()]))
            .collect::<Vec<_>>()
    }

    #[function(name = "pairwise")]
    pub fn pairwise_map(seq: &mut Sequence, function: &Callable) -> EvaluationResult {
        let main = mut_seq_into_iterator(seq).collect::<Vec<_>>();

        let mut out = Vec::with_capacity(main.len() - 1);
        for (a, b) in main.into_iter().tuple_windows() {
            out.push(function.call(&mut [a, b])?);
        }

        Ok(Value::list(out))
    }

    pub fn windows(seq: &mut Sequence, size: usize) -> Vec<Value> {
        mut_seq_into_iterator(seq)
            .collect::<Vec<Value>>()
            .windows(size)
            .map(Value::list)
            .collect()
    }

    pub fn subsequences(seq: &mut Sequence) -> Vec<Value> {
        mut_seq_into_iterator(seq)
            .powerset()
            .map(Value::list)
            .collect::<Vec<Value>>()
    }

    #[function(name = "subsequences")]
    pub fn subsequences_len(seq: &mut Sequence, size: usize) -> Vec<Value> {
        mut_seq_into_iterator(seq)
            .powerset()
            .filter(|x| x.len() == size)
            .map(Value::list)
            .collect::<Vec<Value>>()
    }

    pub fn multi_cartesian_product(seq: &mut Sequence) -> anyhow::Result<Value> {
        let mut iterators = Vec::new();

        for mut value in mut_seq_into_iterator(seq) {
            let iter = mut_value_to_iterator(&mut value)?.collect_vec().into_iter();
            iterators.push(iter);
        }

        let out = iterators
            .into_iter()
            .multi_cartesian_product()
            .map(Value::list)
            .collect_vec();

        return Ok(Value::list(out));
    }
}

fn fold_iterator(
    iterator: MutableValueIntoIterator,
    initial: Value,
    function: &Callable,
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

    use crate::interpreter::{
        environment::Environment, function::Function, iterator::mut_value_to_iterator, value::Value,
    };

    pub fn register(env: &mut Environment) {
        env.declare(
            "zip",
            Value::from(Function::generic(
                crate::interpreter::function::TypeSignature::Variadic,
                |args, _env| match args {
                    [_] => Err(anyhow!("zip must be called with 2 or more arguments").into()),
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
            )),
        );
    }
}
