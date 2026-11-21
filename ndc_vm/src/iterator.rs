use crate::value::VmString;
use crate::{Object, OrdValue, Value};
use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::rc::Rc;

pub trait VmIterator {
    fn next(&mut self) -> Option<Value>;

    /// (`lower_bound`, `upper_bound`). None upper = unbounded.
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Exact length if known.
    fn len(&self) -> Option<usize> {
        let (lo, hi) = self.size_hint();
        hi.filter(|&hi| hi == lo)
    }

    fn is_empty(&self) -> bool {
        self.len() == Some(0)
    }

    /// Returns a new independent iterator with the same current state (for `deepcopy`).
    /// Must be implemented for all iterator types so that `deepcopy` works correctly.
    fn deep_copy(&self) -> Option<SharedIterator>;
}

pub type SharedIterator = Rc<RefCell<dyn VmIterator>>;

/// A cursor over the elements of a value a `for` loop iterates over. The VM
/// and the natives iterate the same way, through one of these: a list,
/// tuple, deque or string is read as it is when the cursor gets to each
/// element, by position, so an element added in a loop is visited; a map's
/// keys are fixed when the cursor starts, and a key removed before the
/// cursor gets to it is skipped; a heap is read in the order it gives its
/// elements; a range gives its `Int`s; and an iterator gives what it gives.
///
/// The cursor holds the value, so it's never copied, and it never holds a
/// borrow of it between elements: a callback may change it.
pub enum ValueIter {
    Seq(SeqIter),
    String(StringIter),
    Map(MapIter),
    Heap(HeapIter),
    /// The `Int`s of a range, counted without an iterator object.
    Ints(std::ops::RangeInclusive<i64>),
    Shared(SharedIterator),
}

impl ValueIter {
    /// A cursor over the elements of `value`, or `None` if it isn't something
    /// a `for` loop iterates over.
    pub fn new(value: &Value) -> Option<Self> {
        let Value::Object(object) = value else {
            return None;
        };
        Some(match object.as_ref() {
            Object::List(_) | Object::Tuple(_) | Object::Deque(_) => {
                Self::Seq(SeqIter::new(Rc::clone(object)))
            }
            Object::String(string) => Self::String(StringIter::new(Rc::clone(string))),
            Object::Map { .. } => Self::Map(MapIter::new(Rc::clone(object))),
            Object::MinHeap(heap) => Self::Heap(HeapIter::new_min(heap)),
            Object::MaxHeap(heap) => Self::Heap(HeapIter::new_max(heap)),
            #[expect(clippy::reversed_empty_ranges, reason = "an empty range gives no Ints")]
            Object::Range(range) => {
                Self::Ints(range.bounds().map_or(1..=0, |(first, last)| first..=last))
            }
            Object::RangeFrom(start) => Self::Ints(*start..=i64::MAX),
            Object::Iterator(iterator) => Self::Shared(Rc::clone(iterator)),
            // A lazy sequence is read with the VM, which may run functions.
            Object::Some(_)
            | Object::Function(_)
            | Object::Struct { .. }
            | Object::Lazy(_)
            | Object::LazyCursor(_) => return None,
        })
    }

    /// A cursor that gives what this one gives from here on, separately
    /// from it, if this one can be copied: all can but an iterator that
    /// doesn't support [`VmIterator::deep_copy`].
    pub fn try_clone(&self) -> Option<Self> {
        Some(match self {
            Self::Seq(seq) => Self::Seq(seq.clone()),
            Self::String(string) => Self::String(string.clone()),
            Self::Map(map) => Self::Map(map.clone()),
            Self::Heap(heap) => Self::Heap(heap.clone()),
            Self::Ints(ints) => Self::Ints(ints.clone()),
            Self::Shared(shared) => Self::Shared(shared.borrow().deep_copy()?),
        })
    }

    /// Whether this cursor makes its elements as it's read, so it may never
    /// end: a range's `Int`s or an iterator. The others go over elements a
    /// value holds.
    pub fn is_lazy(&self) -> bool {
        matches!(self, Self::Ints(_) | Self::Shared(_))
    }
}

impl Iterator for ValueIter {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        match self {
            Self::Seq(seq) => seq.next(),
            Self::String(string) => string.next(),
            Self::Map(map) => map.next(),
            Self::Heap(heap) => heap.next(),
            Self::Ints(ints) => ints.next().map(Value::Int),
            Self::Shared(shared) => shared.borrow_mut().next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Seq(seq) => seq.size_hint(),
            Self::String(string) => string.size_hint(),
            Self::Map(map) => map.size_hint(),
            Self::Heap(heap) => heap.size_hint(),
            Self::Ints(ints) => ints.size_hint(),
            Self::Shared(shared) => shared.borrow().size_hint(),
        }
    }
}

/// The VM keeps a cursor over a deque, string, map, heap or range as an
/// iterator in a register.
impl VmIterator for ValueIter {
    fn next(&mut self) -> Option<Value> {
        Iterator::next(self)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        Iterator::size_hint(self)
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(self.try_clone()?)))
    }
}

/// Iterates over a List, Tuple, or Deque by index.
/// Holds an `Rc<Object>` to keep the sequence alive without an extra allocation.
#[derive(Clone)]
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

    fn next(&mut self) -> Option<Value> {
        let val = match self.obj.as_ref() {
            Object::List(v) => v.borrow().get(self.index)?.clone(),
            Object::Tuple(v) => v.get(self.index)?.clone(),
            Object::Deque(d) => d.borrow().get(self.index)?.clone(),
            _ => unreachable!("SeqIter holds a non-sequence Object"),
        };
        self.index += 1;
        Some(val)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len_of().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

/// Iterates over a map, yielding `(key, value)` tuples.
///
/// Keys are collected at creation time for a stable iteration order; values are
/// cloned from the map on demand so the full entry set is never duplicated.
/// A key removed from the map before the iterator gets to it is skipped.
#[derive(Clone)]
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

    fn next(&mut self) -> Option<Value> {
        let Object::Map { entries, .. } = self.obj.as_ref() else {
            unreachable!("MapIter holds a non-Map object");
        };
        let entries = entries.borrow();
        while let Some(key) = self.keys.get(self.index) {
            self.index += 1;
            if let Some(value) = entries.get(key) {
                return Some(Value::tuple(vec![key.clone(), value.clone()]));
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Keys removed from the map since are skipped, so this is an upper
        // bound only.
        (0, Some(self.keys.len() - self.index))
    }
}

/// Iterates over a heap snapshot in the order the heap gives its elements:
/// a min-heap's smallest first, a max-heap's largest first.
#[derive(Clone)]
pub struct HeapIter {
    entries: Vec<Value>,
    index: usize,
}

impl HeapIter {
    /// Creates an iterator over a min-heap, yielding elements in sorted ascending order.
    pub fn new_min(heap: &RefCell<BinaryHeap<Reverse<OrdValue>>>) -> Self {
        let mut entries: Vec<Value> = heap.borrow().iter().map(|Reverse(v)| v.0.clone()).collect();
        entries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        Self { entries, index: 0 }
    }

    /// Creates an iterator over a max-heap, yielding elements in sorted descending order.
    pub fn new_max(heap: &RefCell<BinaryHeap<OrdValue>>) -> Self {
        let mut entries: Vec<Value> = heap.borrow().iter().map(|v| v.0.clone()).collect();
        entries.sort_by(|a, b| b.partial_cmp(a).unwrap_or(std::cmp::Ordering::Equal));
        Self { entries, index: 0 }
    }

    fn next(&mut self) -> Option<Value> {
        let val = self.entries.get(self.index)?.clone();
        self.index += 1;
        Some(val)
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
/// The elements of a value that holds them (a list, tuple, deque, string,
/// map or heap) are read into an internal buffer at construction, which
/// gives O(1) direct-index access with no `RefCell` overhead in the hot path.
///
/// A range or an iterator is buffered lazily: elements are pulled only as far
/// as the current combination's maximum index requires, so stopping early
/// avoids materialising the full pool, and a range without an end works.
pub struct CombinationsIter {
    buffer: Vec<Value>,
    /// `Some` only for lazy (Shared) sources; set to `None` once exhausted.
    source: Option<ValueIter>,
    /// Combination size. The `indices` vector is only allocated once the
    /// buffer is confirmed to hold at least `k` elements, so an absurdly
    /// large `k` (e.g. `i64::MAX`) over a short source yields nothing
    /// instead of overflowing the allocator with `(0..k).collect()`.
    k: usize,
    indices: Vec<usize>,
    first: bool,
}

impl CombinationsIter {
    /// Returns `None` if `value` is not iterable.
    pub fn new(value: &Value, k: usize) -> Option<Self> {
        let iter = value.iter()?;
        let (buffer, source) = match iter {
            lazy if lazy.is_lazy() => (Vec::new(), Some(lazy)),
            eager => (eager.collect(), None),
        };
        Some(Self {
            buffer,
            source,
            k,
            indices: Vec::new(),
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
            if let Some(v) = Iterator::next(source) {
                self.buffer.push(v)
            } else {
                self.source = None;
                return false;
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
        let k = self.k;

        if k == 0 {
            return if self.first {
                self.first = false;
                Some(Value::list(vec![]))
            } else {
                None
            };
        }

        if self.first {
            if !self.ensure_index(k - 1) {
                // Source can't supply `k` elements: stay exhausted. `first`
                // is left set so a re-poll returns `None` again instead of
                // falling into the else branch and indexing the still-empty
                // `indices`.
                return None;
            }
            // The buffer now holds at least `k` elements, so this allocation
            // is bounded by data we actually materialised — no capacity
            // overflow even when `k` is enormous. Only now is it safe to
            // leave the first-call branch.
            self.indices = (0..k).collect();
            self.first = false;
        } else {
            // For lazy sources: pull one more element before choosing the
            // pivot. Without this, when `indices[k-1]` reaches the end of
            // the current buffer the algorithm advances an earlier index
            // (because `indices[k-1] < pool_len - 1` is false against the
            // not-yet-extended pool), skipping combinations whose rightmost
            // index lives in the unrealised tail. Pulling first makes the
            // rightmost index pivot whenever the source still has elements,
            // matching the eager-source lex order.
            if self.source.is_some() {
                let _ = self.ensure_index(self.indices[k - 1] + 1);
            }

            let pool_len = self.buffer.len();
            if pool_len < k {
                return None;
            }

            let pivot = (0..k).rev().find(|&i| self.indices[i] < pool_len - k + i)?;

            self.indices[pivot] += 1;
            for j in (pivot + 1)..k {
                self.indices[j] = self.indices[j - 1] + 1;
            }
        }

        // All indices are in-bounds: direct indexing is safe.
        Some(Value::list(
            self.indices
                .iter()
                .map(|&i| self.buffer[i].clone())
                .collect(),
        ))
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        let source_copy = match &self.source {
            None => None,
            Some(source) => Some(source.try_clone()?),
        };
        Some(Rc::new(RefCell::new(Self {
            buffer: self.buffer.clone(),
            source: source_copy,
            k: self.k,
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
    pub fn new(value: &Value, n: usize) -> Option<Self> {
        Some(Self {
            source: value.iter()?,
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
        let val = Iterator::next(&mut self.source)?;
        self.remaining -= 1;
        Some(val)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lo, hi) = Iterator::size_hint(&self.source);
        (
            lo.min(self.remaining),
            Some(hi.unwrap_or(usize::MAX).min(self.remaining)),
        )
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            source: self.source.try_clone()?,
            remaining: self.remaining,
        })))
    }
}

/// Pairs each upstream element with its 0-based index, lazily.
///
/// `deep_copy` is supported when the source is a `Shared` iterator. It returns
/// `None` for eager sources (list/deque/map `IntoIter`s) since those lack a
/// copy mechanism — same constraint as [`TakeIter`].
pub struct EnumerateIter {
    source: ValueIter,
    index: usize,
}

impl EnumerateIter {
    /// Returns `None` if `value` is not iterable.
    pub fn new(value: &Value) -> Option<Self> {
        Some(Self {
            source: value.iter()?,
            index: 0,
        })
    }

    pub fn into_shared(self) -> SharedIterator {
        Rc::new(RefCell::new(self))
    }
}

impl VmIterator for EnumerateIter {
    fn next(&mut self) -> Option<Value> {
        let v = Iterator::next(&mut self.source)?;
        let i = self.index;
        self.index += 1;
        Some(Value::tuple(vec![Value::Int(i as i64), v]))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        Iterator::size_hint(&self.source)
    }

    fn deep_copy(&self) -> Option<SharedIterator> {
        Some(Rc::new(RefCell::new(Self {
            source: self.source.try_clone()?,
            index: self.index,
        })))
    }
}

/// Iterates over a string's characters, yielding each as a string of one
/// character. It reads the string as it is when it gets to each character,
/// by the character's position: a character added in a loop is visited, and
/// one removed before the position shifts the rest back, as with a list.
#[derive(Clone)]
pub struct StringIter {
    string: Rc<VmString>,
    index: usize,
}

impl StringIter {
    pub fn new(string: Rc<VmString>) -> Self {
        Self { string, index: 0 }
    }

    fn next(&mut self) -> Option<Value> {
        let ch = self.string.indexed().get_char(self.index)?;
        self.index += 1;
        Some(Value::string(ch.to_string()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.string.char_count().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}
