use crate::interpreter::value::Value;
use andy_cpp_macros::export_module;
use std::cmp::Ordering;

trait TryCompare {
    type Error;
    fn try_min(&mut self) -> Result<Value, Self::Error>;
    fn try_max(&mut self) -> Result<Value, Self::Error>;
}

impl<'a, T> TryCompare for T
where
    T: Iterator<Item = &'a Value>,
{
    type Error = anyhow::Error;

    fn try_min(&mut self) -> Result<Value, Self::Error> {
        self.try_fold(None, |a: Option<&Value>, b| match a {
            None => Ok(Some(b)),
            Some(a) => a.try_cmp(b).map(|o| match o {
                Ordering::Greater => Some(b),
                Ordering::Equal | Ordering::Less => Some(a),
            }),
        })
        .and_then(|x| x.ok_or_else(|| anyhow::anyhow!("empty input to min")))
        .cloned()
    }

    fn try_max(&mut self) -> Result<Value, Self::Error> {
        self.try_fold(None, |a: Option<&Value>, b| match a {
            None => Ok(Some(b)),
            Some(a) => a.try_cmp(b).map(|o| match o {
                Ordering::Less => Some(b),
                Ordering::Equal | Ordering::Greater => Some(a),
            }),
        })
        .and_then(|x| x.ok_or_else(|| anyhow::anyhow!("empty input to max")))
        .cloned()
    }
}

#[export_module]
mod inner {
    use crate::interpreter::value::{Sequence, Value};
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
        }
    }

    pub fn sorted(seq: &Sequence) -> Sequence {
        let cmp = |a: &&Value, b: &&Value| {
            a.partial_cmp(b).expect(
                "TODO: implement proper error handling in native functions (type cannot be sorted)",
            )
        };
        match seq {
            Sequence::String(str) => {
                let sorted = str.borrow().chars().sorted().collect::<String>();
                Sequence::String(Rc::new(RefCell::new(sorted)))
            }
            Sequence::List(list) => {
                let sorted = list
                    .borrow()
                    .iter()
                    .sorted_by(cmp)
                    .cloned()
                    .collect::<Vec<Value>>();
                Sequence::List(Rc::new(RefCell::new(sorted)))
            }
            Sequence::Tuple(list) => {
                let sorted = list
                    .iter()
                    .sorted_unstable_by(cmp)
                    .cloned()
                    .collect::<Vec<Value>>();
                Sequence::Tuple(Rc::new(sorted))
            }
            Sequence::Map(map, _) => {
                let sorted = map
                    .borrow()
                    .keys()
                    .sorted_unstable_by(cmp)
                    .cloned()
                    .collect::<Vec<Value>>();
                Sequence::List(Rc::new(RefCell::new(sorted)))
            }
        }
    }
    pub fn len(seq: &Sequence) -> usize {
        match seq {
            Sequence::String(s) => s.borrow().len(),
            Sequence::List(l) => l.borrow().len(),
            Sequence::Tuple(t) => t.len(),
            Sequence::Map(d, _) => d.borrow().len(),
        }
    }
}
