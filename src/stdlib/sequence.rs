use crate::{compare::FallibleOrd, interpreter::value::Value};
use andy_cpp_macros::export_module;
use std::cmp::Ordering;

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

fn try_sort(v: &mut [Value]) -> anyhow::Result<()> {
    let mut ret = Ok(());
    v.sort_by(|left, right| {
        if ret.is_err() {
            return Ordering::Equal;
        }

        match left.try_cmp(right) {
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
    use crate::interpreter::evaluate::EvaluationResult;
    use crate::interpreter::function::Callable;
    use crate::interpreter::iterator::mut_seq_into_iterator;
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use anyhow::anyhow;
    use itertools::Itertools;
    use std::cell::RefCell;
    use std::rc::Rc;

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
                Ok(Value::Unit)
            }
            Sequence::List(list) => {
                let mut m = list.borrow_mut();
                try_sort(&mut m)?;
                Ok(Value::Unit)
            }
            Sequence::Tuple(_) => Err(anyhow!("tuple cannot be sorted in place")),
            Sequence::Map(_, _) => Err(anyhow!("map cannot be sorted in place")),
            Sequence::Iterator(_) => Err(anyhow!("iterator cannot be sorted in place")),
        }
    }

    pub fn sorted(seq: &Sequence) -> anyhow::Result<Sequence> {
        match seq {
            Sequence::String(str) => {
                let sorted = str.borrow().chars().sorted().collect::<String>();
                Ok(Sequence::String(Rc::new(RefCell::new(sorted))))
            }
            Sequence::List(list) => {
                let mut out = Vec::clone(&*list.borrow());
                try_sort(&mut out)?;
                Ok(Sequence::List(Rc::new(RefCell::new(out))))
            }
            Sequence::Tuple(list) => {
                let mut out = list.iter().cloned().collect::<Vec<Value>>();
                try_sort(&mut out)?;
                Ok(Sequence::Tuple(Rc::new(out)))
            }
            Sequence::Map(map, _) => {
                let mut out = map.borrow().keys().cloned().collect::<Vec<Value>>();
                try_sort(&mut out)?;
                Ok(Sequence::List(Rc::new(RefCell::new(out))))
            }
            Sequence::Iterator(iter) => {
                let mut iter = iter.borrow_mut();
                let mut out: Vec<Value> = iter.by_ref().collect();
                try_sort(&mut out)?;
                Ok(Sequence::List(Rc::new(RefCell::new(out))))
            }
        }
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

    pub fn fold(seq: &mut Sequence, initial: Value, function: Callable) -> EvaluationResult {
        let mut iterator = mut_seq_into_iterator(seq);
        let mut acc = initial;
        while let Some(item) = iterator.next() {
            acc = function.call(&mut [acc, item?])?;
        }

        Ok(acc)
    }

    pub fn reduce(seq: &mut Sequence, function: Callable) -> EvaluationResult {
        let mut iterator = mut_seq_into_iterator(seq);
        let mut acc = iterator
            .next()
            .ok_or_else(|| anyhow!("first argument to reduce must not be empty"))??;

        while let Some(item) = iterator.next() {
            acc = function.call(&mut [acc, item?])?;
        }

        Ok(acc)
    }

    pub fn map(seq: &mut Sequence, function: Callable) -> EvaluationResult {
        let mut iterator = mut_seq_into_iterator(seq);
        let mut out = Vec::new();

        while let Some(item) = iterator.next() {
            let item = item?;
            out.push(function.call(&mut [item])?);
        }

        Ok(Value::from(out))
    }
}
