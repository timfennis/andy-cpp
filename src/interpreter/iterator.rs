//! The implementation of the various iterators in this module were heavily inspired by the ones in
//! noulith which can be found [here](https://github.com/betaveros/noulith/blob/441d52ea433527b7ada5bc6cabd952f9ae8fb791/src/streams.rs)
//!
use crate::interpreter::evaluate::EvaluationResult;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::{Value, ValueType};
use self_cell::self_cell;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

#[derive(Clone)]
pub enum ValueIterator {
    ValueRange(ValueRange),
    ValueRangeFrom(ValueRangeFrom),
    ValueRangeInclusive(ValueRangeInclusive),
}

impl Iterator for ValueIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ValueIterator::ValueRange(inner) => inner.next(),
            ValueIterator::ValueRangeFrom(inner) => inner.next(),
            ValueIterator::ValueRangeInclusive(inner) => inner.next(),
        }
    }
}

pub enum MutableValueIntoIterator<'a> {
    Tuple(RcVecIterator<'a, Value>),
    List(SharedVecIterator<'a, Value>),
    String(SharedStringIterator),
    Map(SharedHashMapIterator<'a>),
    Iterator(Rc<RefCell<ValueIterator>>),
}

impl<'a> Iterator for MutableValueIntoIterator<'a> {
    type Item = EvaluationResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MutableValueIntoIterator::Tuple(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::List(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::String(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::Map(iter) => iter.next().map(Ok),
            MutableValueIntoIterator::Iterator(iter) => iter.borrow_mut().next().map(Ok),
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{} is not iterable", .value_type)]
pub struct NotIterableError {
    value_type: ValueType,
}

impl From<NotIterableError> for FunctionCarrier {
    fn from(value: NotIterableError) -> Self {
        FunctionCarrier::IntoEvaluationError(Box::new(value))
    }
}

pub fn mut_value_to_iterator(
    value: &mut Value,
) -> Result<MutableValueIntoIterator<'_>, NotIterableError> {
    match value {
        Value::Sequence(sequence) => Ok(mut_seq_into_iterator(sequence)),
        value => Err(NotIterableError {
            value_type: value.value_type(),
        }),
    }
}

pub fn mut_seq_into_iterator(sequence: &mut Sequence) -> MutableValueIntoIterator {
    match sequence {
        Sequence::String(string) => {
            MutableValueIntoIterator::String(SharedStringIterator::new(string))
        }
        Sequence::List(list) => {
            MutableValueIntoIterator::List(SharedVecIterator::from_shared_vec(list))
        }
        Sequence::Tuple(tup) => MutableValueIntoIterator::Tuple(RcVecIterator::from_rc_vec(tup)),
        Sequence::Map(map, _) => {
            MutableValueIntoIterator::Map(SharedHashMapIterator::from_ref(map))
        }
        Sequence::Iterator(iter) => MutableValueIntoIterator::Iterator(Rc::clone(iter)),
    }
}

pub enum RcVecIterator<'a, T> {
    Draining(std::vec::Drain<'a, T>),
    Cloning(std::slice::Iter<'a, T>),
}

impl<'a, T> RcVecIterator<'a, T> {
    pub fn from_rc_vec(value: &mut Rc<Vec<T>>) -> RcVecIterator<T> {
        if Rc::get_mut(value).is_some() {
            let vec =
                Rc::get_mut(value).expect("guaranteed to be some by previous call to is_some");
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

pub enum SharedVecIterator<'a, T> {
    IntoIter(std::vec::IntoIter<T>),
    RefCellIterator(RefCellIterator<'a, T>),
}

impl<'a, T> SharedVecIterator<'a, T> {
    pub fn from_shared_vec(value: &mut Rc<RefCell<Vec<T>>>) -> SharedVecIterator<T> {
        match Rc::get_mut(value) {
            Some(vec) => SharedVecIterator::IntoIter(vec.take().into_iter()),
            None => SharedVecIterator::RefCellIterator(RefCellIterator {
                inner: Some(Ref::map(value.borrow(), |it| &it[..])),
            }),
        }
    }
}

impl<'a, T> Iterator for SharedVecIterator<'a, T>
where
    T: Clone,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SharedVecIterator::RefCellIterator(i) => i.next().map(|it| it.to_owned()),
            SharedVecIterator::IntoIter(i) => i.next(),
        }
    }
}

/// The mutable string iterator effectively takes a reference to the string and keeps track of the
/// current offset in order to implement character by character iteration (instead of iterating over
/// u8's)
pub struct SharedStringIterator {
    inner: Rc<RefCell<String>>,
    offset: usize,
}

impl SharedStringIterator {
    pub fn new(value: &Rc<RefCell<String>>) -> Self {
        Self {
            inner: Rc::clone(value),
            offset: 0,
        }
    }
}

impl Iterator for SharedStringIterator {
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
            Some(borrow) => {
                if borrow.is_empty() {
                    None
                } else {
                    let (head, tail) = Ref::map_split(borrow, |slice| (&slice[0], &slice[1..]));
                    self.inner.replace(tail);
                    Some(head)
                }
            }
            None => None,
        }
    }
}

/// Hashmaps
/// Hashmaps
/// Hashmaps
use crate::hash_map::HashMap;

use super::function::FunctionCarrier;
use super::int::Int::Int64;
use super::num::Number;

struct HashMapIter<'a>(pub std::collections::hash_map::Iter<'a, Value, Value>);

self_cell! {
    pub struct SharedHashMapIterator<'a> {
        owner: Ref<'a, HashMap<Value, Value>>,

        #[covariant]
        dependent: HashMapIter,
    }
}

impl SharedHashMapIterator<'_> {
    pub fn from_ref(value: &Rc<RefCell<HashMap<Value, Value>>>) -> SharedHashMapIterator {
        let borrow = value.borrow();
        SharedHashMapIterator::new(borrow, |map| HashMapIter(map.iter()))
    }
}

impl<'a> Iterator for SharedHashMapIterator<'a> {
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

impl ValueRange {
    #[must_use] pub fn contains(&self, v: &Value) -> bool {
        match v {
            Value::Number(Number::Int(Int64(v))) => self.0.contains(v),
            _ => false,
        }
    }
}

impl Iterator for ValueRange {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::from)
    }
}

#[derive(Clone)]
pub struct ValueRangeInclusive(pub std::ops::RangeInclusive<i64>);

impl ValueRangeInclusive {
    #[must_use] pub fn contains(&self, v: &Value) -> bool {
        match v {
            Value::Number(Number::Int(Int64(v))) => self.0.contains(v),
            _ => false,
        }
    }
}

impl Iterator for ValueRangeInclusive {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::from)
    }
}

#[derive(Clone)]
pub struct ValueRangeFrom(pub std::ops::RangeFrom<i64>);
impl ValueRangeFrom {
    #[must_use] pub fn contains(&self, v: &Value) -> bool {
        match v {
            Value::Number(Number::Int(Int64(v))) => self.0.contains(v),
            _ => false,
        }
    }
}

impl Iterator for ValueRangeFrom {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::from)
    }
}

/// Cursed experiment
pub struct RcIter {
    iter: Rc<RefCell<ValueIterator>>,
}

impl RcIter {
    pub fn new(iter: Rc<RefCell<ValueIterator>>) -> Self {
        Self { iter }
    }
}

impl Iterator for RcIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.borrow_mut().next()
    }
}
