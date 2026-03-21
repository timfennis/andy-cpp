use crate::{Object, OrdValue, Value, ValueIter};
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
/// Keys are collected at creation time for a stable iteration order; values are
/// cloned from the map on demand so the full entry set is never duplicated.
pub struct MapIter {
    obj: Rc<Object>,
    keys: Vec<Value>,
    index: usize,
}

impl MapIter {
    pub fn new(obj: Rc<Object>) -> Self {
        let keys = match obj.as_ref() {
            Object::Map { entries, .. } => entries.borrow().keys().cloned().collect(),
            _ => unreachable!("MapIter requires a Map object"),
        };
        Self {
            obj,
            keys,
            index: 0,
        }
    }
}

impl VmIterator for MapIter {
    fn next(&mut self) -> Option<Value> {
        if self.index >= self.keys.len() {
            return None;
        }
        let key = self.keys[self.index].clone();
        let value = match self.obj.as_ref() {
            Object::Map { entries, .. } => entries.borrow().get(&key).cloned()?,
            _ => unreachable!("MapIter holds a non-Map object"),
        };
        self.index += 1;
        Some(Value::Object(Rc::new(Object::Tuple(vec![key, value]))))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.keys.len() - self.index;
        (remaining, Some(remaining))
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            obj: Rc::clone(&self.obj),
            keys: self.keys.clone(),
            index: self.index,
        })))
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

/// Lazy k-combinations iterator.
///
/// Eager sources (List, Tuple, Deque, Map) are drained into an internal buffer
/// at construction using [`Value::try_into_iter`], which already applies
/// `Rc::unwrap_or_clone` so no copy occurs when this is the sole owner. This
/// gives O(1) direct-index access with no `RefCell` overhead in the hot path.
///
/// Iterator sources (`Object::Iterator`) are buffered lazily: elements are
/// pulled only as far as the current combination's maximum index requires, so
/// stopping early avoids materialising the full pool.
pub struct CombinationsIter {
    buffer: Vec<Value>,
    /// `Some` only for lazy (Shared) sources; set to `None` once exhausted.
    source: Option<ValueIter>,
    indices: Vec<usize>,
    first: bool,
}

impl CombinationsIter {
    /// Returns `None` if `value` is not iterable.
    pub fn new(value: Value, k: usize) -> Option<Self> {
        let iter = value.try_into_iter()?;
        let (buffer, source) = match iter {
            shared @ ValueIter::Shared(_) => (Vec::new(), Some(shared)),
            eager => (eager.collect(), None),
        };
        Some(Self {
            buffer,
            source,
            indices: (0..k).collect(),
            first: true,
        })
    }

    /// Pull from the lazy source until `buffer[i]` exists.
    /// Returns false if the source is exhausted before index `i` is available.
    fn ensure_index(&mut self, i: usize) -> bool {
        if i < self.buffer.len() {
            return true;
        }
        let Some(source) = &mut self.source else {
            return false;
        };
        while self.buffer.len() <= i {
            match source.next() {
                Some(v) => self.buffer.push(v),
                None => {
                    self.source = None;
                    return false;
                }
            }
        }
        true
    }

    pub fn into_shared(self) -> SharedIterator {
        Rc::new(RefCell::new(self))
    }
}

impl VmIterator for CombinationsIter {
    fn next(&mut self) -> Option<Value> {
        let k = self.indices.len();

        if k == 0 {
            return if self.first {
                self.first = false;
                Some(Value::tuple(vec![]))
            } else {
                None
            };
        }

        if self.first {
            self.first = false;
            if !self.ensure_index(k - 1) {
                return None;
            }
        } else {
            let pool_len = self.buffer.len();
            if pool_len < k {
                return None;
            }

            let pivot = match (0..k).rev().find(|&i| self.indices[i] < pool_len - k + i) {
                Some(p) => p,
                None if self.source.is_none() => return None,
                // For lazy sources: pulling one more element always unlocks index k-1.
                // When pivot = None, indices[j] = pool_len - k + j for all j, so
                // indices[k-1] = pool_len - 1. After one pull, pool_len grows by 1 and
                // indices[k-1] < new_pool_len - k + (k-1) holds.
                None => {
                    if !self.ensure_index(self.buffer.len()) {
                        return None;
                    }
                    k - 1
                }
            };

            self.indices[pivot] += 1;
            for j in (pivot + 1)..k {
                self.indices[j] = self.indices[j - 1] + 1;
            }
            // For lazy sources, buffer up to the new last index.
            if !self.ensure_index(self.indices[k - 1]) {
                return None;
            }
        }

        // All indices are in-bounds: direct indexing is safe.
        Some(Value::tuple(
            self.indices
                .iter()
                .map(|&i| self.buffer[i].clone())
                .collect(),
        ))
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        let source_copy = match &self.source {
            None => None,
            Some(ValueIter::Shared(shared)) => {
                Some(ValueIter::Shared(shared.borrow().deep_copy()?))
            }
            Some(_) => return None, // unreachable: eager sources drain to None at construction
        };
        Some(Rc::new(RefCell::new(Self {
            buffer: self.buffer.clone(),
            source: source_copy,
            indices: self.indices.clone(),
            first: self.first,
        })))
    }
}

/// Takes at most `n` elements from an upstream iterator.
///
/// `deep_copy` is supported when the source is a `Shared` iterator (e.g. a
/// `CombinationsIter`). It returns `None` for eager sources (list/deque/map
/// `IntoIter`s) since those lack a copy mechanism.
pub struct TakeIter {
    source: ValueIter,
    remaining: usize,
}

impl TakeIter {
    /// Returns `None` if `value` is not iterable.
    pub fn new(value: Value, n: usize) -> Option<Self> {
        Some(Self {
            source: value.try_into_iter()?,
            remaining: n,
        })
    }

    pub fn into_shared(self) -> SharedIterator {
        Rc::new(RefCell::new(self))
    }
}

impl VmIterator for TakeIter {
    fn next(&mut self) -> Option<Value> {
        if self.remaining == 0 {
            return None;
        }
        let val = self.source.next()?;
        self.remaining -= 1;
        Some(val)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lo, hi) = match &self.source {
            ValueIter::Shared(s) => s.borrow().size_hint(),
            _ => (0, None),
        };
        (
            lo.min(self.remaining),
            Some(hi.unwrap_or(usize::MAX).min(self.remaining)),
        )
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        let ValueIter::Shared(shared) = &self.source else {
            return None;
        };
        Some(Rc::new(RefCell::new(Self {
            source: ValueIter::Shared(shared.borrow().deep_copy()?),
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
