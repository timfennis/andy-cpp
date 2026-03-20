use crate::{Object, OrdValue, Value};
use ndc_core::hash_map::HashMap;
use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
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
    fn range_bounds(&self) -> Option<(i64, i64, bool)> {
        None
    }

    /// If this iterator represents an unbounded range (`start..`), returns the start.
    fn unbounded_range_start(&self) -> Option<i64> {
        None
    }

    /// Returns a new independent iterator with the same current state (for `deepcopy`).
    /// The default implementation returns `None`; override for clonable iterator types.
    fn deep_copy(&self) -> Option<SharedIterator> {
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

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            current: self.current,
            end: self.end,
        })))
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
        let remaining = (self.end - self.current).saturating_add(1) as usize;
        (remaining, Some(remaining))
    }

    fn range_bounds(&self) -> Option<(i64, i64, bool)> {
        Some((self.current, self.end, true))
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            current: self.current,
            end: self.end,
            done: self.done,
        })))
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

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            current: self.current,
        })))
    }
}

/// Iterates over a List, Tuple, or Deque by index.
/// Holds an `Rc<Object>` to keep the sequence alive without an extra allocation.
pub struct SeqIter {
    obj: Rc<Object>,
    index: usize,
}

impl SeqIter {
    pub fn new(obj: Rc<Object>) -> Self {
        Self { obj, index: 0 }
    }

    fn len_of(&self) -> usize {
        match self.obj.as_ref() {
            Object::List(v) => v.borrow().len(),
            Object::Tuple(v) => v.len(),
            Object::Deque(d) => d.borrow().len(),
            _ => unreachable!("SeqIter holds a non-sequence Object"),
        }
    }
}

impl VmIterator for SeqIter {
    fn next(&mut self) -> Option<Value> {
        let val = match self.obj.as_ref() {
            Object::List(v) => {
                let v = v.borrow();
                if self.index >= v.len() {
                    return None;
                }
                v[self.index].clone()
            }
            Object::Tuple(v) => {
                if self.index >= v.len() {
                    return None;
                }
                v[self.index].clone()
            }
            Object::Deque(d) => {
                let d = d.borrow();
                if self.index >= d.len() {
                    return None;
                }
                d[self.index].clone()
            }
            _ => unreachable!("SeqIter holds a non-sequence Object"),
        };
        self.index += 1;
        Some(val)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len_of().saturating_sub(self.index);
        (remaining, Some(remaining))
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            obj: Rc::clone(&self.obj),
            index: self.index,
        })))
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
    pub fn new(map: &RefCell<HashMap<Value, Value>>) -> Self {
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
            Some(Value::Object(Rc::new(Object::Tuple(vec![k, v]))))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.entries.len() - self.index;
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
    pub fn new(heap: &RefCell<BinaryHeap<Reverse<OrdValue>>>) -> Self {
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
    pub fn new(heap: &RefCell<BinaryHeap<OrdValue>>) -> Self {
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

/// Infinitely (or finitely) repeats a single value.
pub struct RepeatIter {
    value: Value,
    remaining: Option<usize>,
}

impl RepeatIter {
    pub fn new(value: Value) -> Self {
        Self {
            value,
            remaining: None,
        }
    }

    pub fn new_limited(value: Value, count: usize) -> Self {
        Self {
            value,
            remaining: Some(count),
        }
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn remaining(&self) -> Option<usize> {
        self.remaining
    }
}

impl VmIterator for RepeatIter {
    fn next(&mut self) -> Option<Value> {
        match self.remaining.as_mut() {
            Some(0) => None,
            Some(n) => {
                *n -= 1;
                Some(self.value.clone())
            }
            None => Some(self.value.clone()),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.remaining {
            Some(n) => (n, Some(n)),
            None => (usize::MAX, None),
        }
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            value: self.value.clone(),
            remaining: self.remaining,
        })))
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
        let remaining_bytes = s.len() - self.byte_offset;
        // Byte length is a valid upper bound (each char is 1–4 bytes).
        // Exact char count would require an O(n) scan.
        (0, Some(remaining_bytes))
    }
}
