use crate::{Object, OrdValue, Value};
use ndc_core::hash_map::HashMap;
use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::VecDeque;
use std::rc::Rc;

pub trait VmIterator {
    fn next(&mut self) -> Option<Value>;

    /// (lower_bound, upper_bound). None upper = unbounded.
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Exact length if known.
    fn len(&self) -> Option<usize> {
        let (lo, hi) = self.size_hint();
        hi.filter(|&hi| hi == lo)
    }

    /// If this iterator represents a range, returns `(start, end, inclusive)`.
    /// Used by the VM bridge to convert ranges back to interpreter range values.
    /// TODO: remove once the VM bridge (vm_bridge.rs) is gone.
    fn range_bounds(&self) -> Option<(i64, i64, bool)> {
        None
    }

    /// If this iterator represents an unbounded range (`start..`), returns the start.
    /// TODO: remove once the VM bridge (vm_bridge.rs) is gone.
    fn unbounded_range_start(&self) -> Option<i64> {
        None
    }
}

pub type SharedIterator = Rc<RefCell<dyn VmIterator>>;

/// Exclusive range: `start..end`
pub struct RangeIter {
    current: i64,
    end: i64,
}

impl RangeIter {
    pub fn new(start: i64, end: i64) -> Self {
        Self {
            current: start,
            end,
        }
    }
}

impl VmIterator for RangeIter {
    fn next(&mut self) -> Option<Value> {
        if self.current < self.end {
            let val = self.current;
            self.current += 1;
            Some(Value::Int(val))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.end - self.current).max(0) as usize;
        (remaining, Some(remaining))
    }

    fn range_bounds(&self) -> Option<(i64, i64, bool)> {
        Some((self.current, self.end, false))
    }
}

/// Inclusive range: start..=end
pub struct RangeInclusiveIter {
    current: i64,
    end: i64,
    done: bool,
}

impl RangeInclusiveIter {
    pub fn new(start: i64, end: i64) -> Self {
        Self {
            current: start,
            end,
            done: start > end,
        }
    }
}

impl VmIterator for RangeInclusiveIter {
    fn next(&mut self) -> Option<Value> {
        if self.done {
            return None;
        }
        let val = self.current;
        if self.current == self.end {
            self.done = true;
        } else {
            self.current += 1;
        }
        Some(Value::Int(val))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.done {
            return (0, Some(0));
        }
        let remaining = (self.end - self.current + 1) as usize;
        (remaining, Some(remaining))
    }

    fn range_bounds(&self) -> Option<(i64, i64, bool)> {
        Some((self.current, self.end, true))
    }
}

/// Unbounded range: `start..`
pub struct UnboundedRangeIter {
    current: i64,
}

impl UnboundedRangeIter {
    pub fn new(start: i64) -> Self {
        Self { current: start }
    }
}

impl VmIterator for UnboundedRangeIter {
    fn next(&mut self) -> Option<Value> {
        let val = self.current;
        self.current += 1;
        Some(Value::Int(val))
    }

    fn unbounded_range_start(&self) -> Option<i64> {
        Some(self.current)
    }
}

/// Iterates over a list by index
pub struct ListIter {
    list: Rc<RefCell<Vec<Value>>>,
    index: usize,
}

impl ListIter {
    pub fn new(list: Rc<RefCell<Vec<Value>>>) -> Self {
        Self { list, index: 0 }
    }
}

impl VmIterator for ListIter {
    fn next(&mut self) -> Option<Value> {
        let list = self.list.borrow();
        if self.index < list.len() {
            let val = list[self.index].clone();
            self.index += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.list.borrow().len().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

/// Iterates over a tuple
pub struct TupleIter {
    values: Vec<Value>,
    index: usize,
}

impl TupleIter {
    pub fn new(values: Vec<Value>) -> Self {
        Self { values, index: 0 }
    }
}

impl VmIterator for TupleIter {
    fn next(&mut self) -> Option<Value> {
        if self.index < self.values.len() {
            let val = self.values[self.index].clone();
            self.index += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.values.len() - self.index;
        (remaining, Some(remaining))
    }
}

/// Iterates over a map, yielding `(key, value)` tuples.
///
/// Note: entries are snapshotted at creation time, so mutations to the map
/// during iteration are not reflected.
pub struct MapIter {
    entries: Vec<(Value, Value)>,
    index: usize,
}

impl MapIter {
    pub fn new(map: Rc<RefCell<HashMap<Value, Value>>>) -> Self {
        let entries = map
            .borrow()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        Self { entries, index: 0 }
    }
}

impl VmIterator for MapIter {
    fn next(&mut self) -> Option<Value> {
        if self.index < self.entries.len() {
            let (k, v) = self.entries[self.index].clone();
            self.index += 1;
            Some(Value::Object(Box::new(Object::Tuple(vec![k, v]))))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.entries.len() - self.index;
        (remaining, Some(remaining))
    }
}

/// Iterates over a deque front-to-back
pub struct DequeIter {
    deque: Rc<RefCell<VecDeque<Value>>>,
    index: usize,
}

impl DequeIter {
    pub fn new(deque: Rc<RefCell<VecDeque<Value>>>) -> Self {
        Self { deque, index: 0 }
    }
}

impl VmIterator for DequeIter {
    fn next(&mut self) -> Option<Value> {
        let d = self.deque.borrow();
        if self.index < d.len() {
            let val = d[self.index].clone();
            drop(d);
            self.index += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.deque.borrow().len().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

/// Iterates over a min-heap, yielding elements in sorted (ascending) order.
/// Drains the heap — snapshot the entries at creation time.
pub struct MinHeapIter {
    entries: Vec<Value>,
    index: usize,
}

impl MinHeapIter {
    pub fn new(heap: Rc<RefCell<BinaryHeap<Reverse<OrdValue>>>>) -> Self {
        let mut entries: Vec<Value> = heap.borrow().iter().map(|Reverse(v)| v.0.clone()).collect();
        entries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        Self { entries, index: 0 }
    }
}

impl VmIterator for MinHeapIter {
    fn next(&mut self) -> Option<Value> {
        if self.index < self.entries.len() {
            let val = self.entries[self.index].clone();
            self.index += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.entries.len() - self.index;
        (remaining, Some(remaining))
    }
}

/// Iterates over a max-heap, yielding elements in arbitrary (heap) order.
/// Snapshot the entries at creation time.
pub struct MaxHeapIter {
    entries: Vec<Value>,
    index: usize,
}

impl MaxHeapIter {
    pub fn new(heap: Rc<RefCell<BinaryHeap<OrdValue>>>) -> Self {
        let entries: Vec<Value> = heap.borrow().iter().map(|v| v.0.clone()).collect();
        Self { entries, index: 0 }
    }
}

impl VmIterator for MaxHeapIter {
    fn next(&mut self) -> Option<Value> {
        if self.index < self.entries.len() {
            let val = self.entries[self.index].clone();
            self.index += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.entries.len() - self.index;
        (remaining, Some(remaining))
    }
}

/// Iterates over string characters, yielding each as a single-char string
pub struct StringIter {
    string: Rc<RefCell<String>>,
    byte_offset: usize,
}

impl StringIter {
    pub fn new(string: Rc<RefCell<String>>) -> Self {
        Self {
            string,
            byte_offset: 0,
        }
    }
}

impl VmIterator for StringIter {
    fn next(&mut self) -> Option<Value> {
        let s = self.string.borrow();
        let remaining = &s[self.byte_offset..];
        let ch = remaining.chars().next()?;
        self.byte_offset += ch.len_utf8();
        Some(Value::string(ch.to_string()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let s = self.string.borrow();
        let remaining = &s[self.byte_offset..];
        let len = remaining.chars().count();
        (len, Some(len))
    }
}
