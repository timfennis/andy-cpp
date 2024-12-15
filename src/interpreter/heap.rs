use crate::interpreter::value::Value;
use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;
use std::ops::{Deref, DerefMut};

pub type MinHeap = ManagedHeap<Reverse<HeapValue>>;
pub type MaxHeap = ManagedHeap<HeapValue>;

pub struct ManagedHeap<T> {
    heap: BinaryHeap<T>,
}

impl<T> Deref for ManagedHeap<T> {
    type Target = BinaryHeap<T>;

    fn deref(&self) -> &Self::Target {
        &self.heap
    }
}
impl<T> DerefMut for ManagedHeap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.heap
    }
}

impl<T> Default for ManagedHeap<T>
where
    T: Ord,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> ManagedHeap<T>
where
    T: Ord,
{
    pub fn new() -> Self {
        Self {
            heap: BinaryHeap::default(),
        }
    }

    fn from_heap(heap: BinaryHeap<T>) -> Self {
        Self { heap }
    }

    pub fn into_inner(self) -> BinaryHeap<T> {
        self.heap
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

impl FromIterator<Value> for MinHeap {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        MinHeap::from_heap(
            iter.into_iter()
                .map(|v| Reverse(HeapValue(v)))
                .collect::<BinaryHeap<_>>(),
        )
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

impl FromIterator<Value> for MaxHeap {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        MaxHeap::from_heap(iter.into_iter().map(HeapValue).collect::<BinaryHeap<_>>())
    }
}

#[derive(Clone)]
pub struct HeapValue(pub Value);

impl From<HeapValue> for Value {
    fn from(value: HeapValue) -> Self {
        value.0
    }
}

impl Deref for HeapValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for HeapValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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
