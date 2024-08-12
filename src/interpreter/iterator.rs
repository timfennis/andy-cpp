//! The implementation of the various iterators in this module were heavily inspired by the ones in
//! noulith which can be found [here](https://github.com/betaveros/noulith/blob/441d52ea433527b7ada5bc6cabd952f9ae8fb791/src/streams.rs)
//!
use crate::interpreter::evaluate::EvaluationResult;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::{Value, ValueType};
use itertools::Itertools;
use self_cell::self_cell;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

pub trait ValueIterator: Iterator<Item = Value> {
    fn boxed_clone(&self) -> Box<dyn ValueIterator>;
}

pub enum MutableValueIntoIterator<'a> {
    Tuple(RcVecIterator<'a, Value>),
    List(MutableVecIterator<'a, Value>),
    String(MutableStringIterator),
    Map(MutableHashMapIterator<'a>),
    Iterator(&'a mut Rc<dyn ValueIterator>),
}

impl<'a> Iterator for MutableValueIntoIterator<'a> {
    type Item = EvaluationResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MutableValueIntoIterator::Tuple(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::List(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::String(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::Map(iter) => iter.next().map(Ok),

            // This beautiful piece of magic tries to get a mutable reference to the iterator which
            // is needed if we want to be able to call `next`. This is only possible if the
            // reference count in Rc == 1. If there are more references to the iterator we use
            // `boxed_clone` to obtain a copy of the iterator and we write that copy back into
            // `MutableVecIntoIterator`.
            MutableValueIntoIterator::Iterator(iter) => if let Some(iter) = Rc::get_mut(iter) {
                iter.next()
            } else {
                let mut clone = iter.boxed_clone();
                let value = clone.next();
                **iter = Rc::from(clone);
                value
            }
            .map(Ok),
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{} is not iterable", .value_type)]
pub struct NotIterableError {
    value_type: ValueType,
}

pub fn mut_value_to_iterator(
    value: &mut Value,
) -> Result<MutableValueIntoIterator, NotIterableError> {
    match value {
        Value::Sequence(sequence) => Ok(mut_seq_into_iterator(sequence)),
        value => Err(NotIterableError {
            value_type: value.value_type(),
        }),
    }
}

fn mut_seq_into_iterator(sequence: &mut Sequence) -> MutableValueIntoIterator {
    match sequence {
        Sequence::String(string) => {
            MutableValueIntoIterator::String(MutableStringIterator::new(string))
        }
        Sequence::List(list) => {
            MutableValueIntoIterator::List(MutableVecIterator::from_rc_ref_cell_vec(list))
        }
        Sequence::Tuple(tup) => MutableValueIntoIterator::Tuple(RcVecIterator::from_rc_vec(tup)),
        Sequence::Map(map, _) => {
            MutableValueIntoIterator::Map(MutableHashMapIterator::from_mut_ref(map))
        }
        Sequence::Iterator(iter) => MutableValueIntoIterator::Iterator(iter),
    }
}

pub enum RcVecIterator<'a, T> {
    Draining(std::vec::Drain<'a, T>),
    Cloning(std::slice::Iter<'a, T>),
}

impl<'a, T> RcVecIterator<'a, T> {
    pub fn from_rc_vec(value: &mut Rc<Vec<T>>) -> RcVecIterator<T> {
        if Rc::get_mut(value).is_some() {
            let vec = Rc::get_mut(value).expect("must be some at this point");
            RcVecIterator::Draining(vec.drain(..))
        } else {
            RcVecIterator::Cloning(value.iter())
        }
    }
}

impl<'a, T> Iterator for RcVecIterator<'a, T>
where
    T: Clone,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            RcVecIterator::Draining(i) => i.next(),
            RcVecIterator::Cloning(i) => i.next().cloned(),
        }
    }
}

pub enum MutableVecIterator<'a, T> {
    IntoIter(std::vec::IntoIter<T>),
    RefCellIterator(RefCellIterator<'a, T>),
}

impl<'a, T> MutableVecIterator<'a, T> {
    pub fn from_rc_ref_cell_vec(value: &mut Rc<RefCell<Vec<T>>>) -> MutableVecIterator<T> {
        match Rc::get_mut(value) {
            Some(vec) => MutableVecIterator::IntoIter(vec.take().into_iter()),
            None => MutableVecIterator::RefCellIterator(RefCellIterator {
                inner: Some(Ref::map(value.borrow(), |it| &it[..])),
            }),
        }
    }
}

impl<'a, T> Iterator for MutableVecIterator<'a, T>
where
    T: Clone,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MutableVecIterator::RefCellIterator(i) => i.next().map(|it| it.to_owned()),
            MutableVecIterator::IntoIter(i) => i.next(),
        }
    }
}

/// The mutable string iterator effectively takes a reference to the string and keeps track of the
/// current offset in order to implement character by character iteration (instead of iterating over
/// u8's)
pub struct MutableStringIterator {
    inner: Rc<RefCell<String>>,
    offset: usize,
}

impl MutableStringIterator {
    pub fn new(value: &Rc<RefCell<String>>) -> Self {
        Self {
            inner: Rc::clone(value),
            offset: 0,
        }
    }
}

impl Iterator for MutableStringIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let current_char = self.inner.borrow()[self.offset..]
            .chars()
            .take(1)
            .collect::<String>();
        self.offset += current_char.len();
        if current_char.is_empty() {
            None
        } else {
            Some(Value::from(current_char))
        }
    }
}

/// This `RefCellIterator` is adapted from this stack-overflow answer:
/// <https://stackoverflow.com/questions/33541492/returning-iterator-of-a-vec-in-a-refcell>
/// It returns a `Ref` to a slice of the vector it's iterating over using `Ref::map_split` making it
/// so that we only need a `Ref` to the original list rather than cloning the `Rc`
pub struct RefCellIterator<'a, T> {
    inner: Option<Ref<'a, [T]>>,
}

impl<'a, T> Iterator for RefCellIterator<'a, T>
where
    T: Clone,
{
    type Item = Ref<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.take() {
            Some(borrow) => match *borrow {
                [] => None,
                [_, ..] => {
                    let (head, tail) = Ref::map_split(borrow, |slice| (&slice[0], &slice[1..]));
                    self.inner.replace(tail);
                    Some(head)
                }
            },
            None => None,
        }
    }
}

/// Hashmaps
/// Hashmaps
/// Hashmaps
use crate::hash_map::HashMap;

struct HashMapIter<'a>(pub std::collections::hash_map::Iter<'a, Value, Value>);

self_cell! {
    pub struct MutableHashMapIterator<'a> {
        owner: Ref<'a, HashMap<Value, Value>>,

        #[covariant]
        dependent: HashMapIter,
    }
}

impl MutableHashMapIterator<'_> {
    pub fn from_mut_ref(value: &mut Rc<RefCell<HashMap<Value, Value>>>) -> MutableHashMapIterator {
        let borrow = value.borrow();
        MutableHashMapIterator::new(borrow, |map| HashMapIter(map.iter()))
    }
}

impl<'a> Iterator for MutableHashMapIterator<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let cur = self.with_dependent_mut(|_map, iter| iter.next());
        // Creates copies of the values inside the map
        cur.map(|cur| Value::Sequence(Sequence::Tuple(Rc::new(vec![cur.0.clone(), cur.1.clone()]))))
    }
}

impl<'a> Iterator for HashMapIter<'a> {
    type Item = (&'a Value, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

/// Ranges are a whole thing
#[derive(Clone)]
pub struct ValueRange(pub std::ops::Range<i64>);
impl Iterator for ValueRange {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::from)
    }
}

impl ValueIterator for ValueRange {
    fn boxed_clone(&self) -> Box<dyn ValueIterator> {
        Box::new(self.clone())
    }
}

#[derive(Clone)]
pub struct ValueRangeInclusive(pub std::ops::RangeInclusive<i64>);

impl Iterator for ValueRangeInclusive {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::from)
    }
}

impl ValueIterator for ValueRangeInclusive {
    fn boxed_clone(&self) -> Box<dyn ValueIterator> {
        Box::new(self.clone())
    }
}

#[derive(Clone)]
pub struct ValueRangeFrom(pub std::ops::RangeFrom<i64>);

impl Iterator for ValueRangeFrom {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::from)
    }
}

impl ValueIterator for ValueRangeFrom {
    fn boxed_clone(&self) -> Box<dyn ValueIterator> {
        Box::new(self.clone())
    }
}
