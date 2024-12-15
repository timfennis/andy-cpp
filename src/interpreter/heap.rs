use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::value::{Value, ValueType};
use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;
use std::ops::Deref;

pub type MinHeap = CheckedHeap<Reverse<HeapValue>>;
pub type MaxHeap = CheckedHeap<HeapValue>;
pub struct CheckedHeap<T> {
    heap: BinaryHeap<T>,
}

impl<T> Deref for CheckedHeap<T> {
    type Target = BinaryHeap<T>;

    fn deref(&self) -> &Self::Target {
        &self.heap
    }
}

impl<T> Default for CheckedHeap<T>
where
    T: Ord,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> CheckedHeap<T>
where
    T: Ord,
{
    pub fn new() -> Self {
        Self {
            heap: BinaryHeap::default(),
        }
    }
}

impl MinHeap {
    pub fn push(&mut self, value: Value) {
        self.heap.push(Reverse(HeapValue(value)));
    }

    pub fn pop(&mut self) -> Value {
        self.heap
            .pop()
            .map_or_else(Value::none, |hv| Value::some(hv.0 .0))
    }
}

impl MaxHeap {
    pub fn push(&mut self, value: Value) {
        self.heap.push(HeapValue(value));
    }

    pub fn pop(&mut self) -> Value {
        self.heap
            .pop()
            .map_or_else(Value::none, |hv| Value::some(hv.0))
    }
}

pub struct HeapValue(pub Value);

impl PartialEq for HeapValue {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for HeapValue {}
impl PartialOrd for HeapValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HeapValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).unwrap_or(Ordering::Equal)
        // .expect("checked heap must guarantee this never happens")
    }
}

#[derive(thiserror::Error, Debug)]
pub enum HeapError {
    // TODO: improve these error messages
    #[error("This heap can only contain {expected} but you tried to insert {actual}")]
    InvalidValueType {
        expected: ValueType,
        actual: ValueType,
    },
    #[error("Values of type {typ} are not supported in this datastructure")]
    UnsupportedValueType { typ: ValueType },
}

impl From<HeapError> for FunctionCarrier {
    fn from(value: HeapError) -> Self {
        FunctionCarrier::IntoEvaluationError(Box::new(value))
    }
}
