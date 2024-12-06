use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::value::{Value, ValueType};
use std::cmp::{Ordering, Reverse};
use std::collections::binary_heap::Iter;
use std::collections::BinaryHeap;

pub type MinHeap = CheckedHeap<Reverse<HeapValue>>;
pub type MaxHeap = CheckedHeap<HeapValue>;
pub struct CheckedHeap<T> {
    heap: BinaryHeap<T>,
    typ: Option<ValueType>,
}

impl<T> CheckedHeap<T>
where
    T: Ord,
{
    pub fn new() -> Self {
        Self {
            heap: BinaryHeap::default(),
            typ: None,
        }
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.heap.iter()
    }

    pub fn peek(&self) -> Option<&T> {
        self.heap.peek()
    }

    fn check_push(&mut self, value: &Value) -> Result<(), HeapError> {
        if value.value_type() == ValueType::Function {
            return Err(HeapError::UnsupportedValueType {
                typ: ValueType::Function,
            });
        }

        if let Some(typ) = self.typ {
            if value.value_type() != typ {
                return Err(HeapError::InvalidValueType {
                    expected: typ,
                    actual: value.value_type(),
                });
            }
        } else {
            self.typ = Some(value.value_type());
        }

        Ok(())
    }
}

impl MinHeap {
    pub fn push(&mut self, value: Value) -> Result<(), HeapError> {
        self.check_push(&value)?;
        self.heap.push(Reverse(HeapValue(value)));
        Ok(())
    }

    pub fn pop(&mut self) -> Value {
        let pop = self
            .heap
            .pop()
            .map_or_else(Value::none, |hv| Value::some(hv.0 .0));
        if self.heap.is_empty() {
            self.typ = None;
        }
        pop
    }
}

impl MaxHeap {
    pub fn push(&mut self, value: Value) -> Result<(), HeapError> {
        self.check_push(&value)?;
        self.heap.push(HeapValue(value));
        Ok(())
    }

    pub fn pop(&mut self) -> Value {
        let pop = self
            .heap
            .pop()
            .map_or_else(Value::none, |hv| Value::some(hv.0));
        if self.heap.is_empty() {
            self.typ = None;
        }
        pop
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
        self.0
            .partial_cmp(&other.0)
            .expect("checked heap must guarantee this never happens")
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
