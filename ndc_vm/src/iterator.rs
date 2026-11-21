use crate::lazy::LazyCursor;
use crate::value::VmString;
use crate::{Object, OrdValue, Value};
use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::rc::Rc;

/// A cursor over the elements of a value a `for` loop iterates over. The VM
/// and the natives iterate the same way, through one of these: a list,
/// tuple, deque or string is read as it is when the cursor gets to each
/// element, by position, so an element added in a loop is visited; a map's
/// keys are fixed when the cursor starts, and a key removed before the
/// cursor gets to it is skipped; a heap is read in the order it gives its
/// elements; a range gives its `Int`s; and a lazy sequence whose steps call
/// no functions gives what its steps make. One whose steps call functions
/// is read with a [`crate::LazyCursor`] and the VM instead.
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
    /// What the steps of a lazy sequence that call no functions make, as
    /// `xs.enumerate()`.
    Steps(Box<LazyCursor>),
}

impl ValueIter {
    /// A cursor over the elements of `value`, or `None` if it isn't something
    /// a `for` loop iterates over without the VM.
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
            Object::Lazy(lazy) if !lazy.calls_functions() => Self::Steps(Box::new(lazy.cursor())),
            // A lazy sequence whose steps call functions is read with the
            // VM, which runs them.
            Object::Some(_)
            | Object::Function(_)
            | Object::Struct { .. }
            | Object::Lazy(_)
            | Object::Cursor(_)
            | Object::LazyCursor(_) => return None,
        })
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
            Self::Steps(steps) => steps.next_without_calls(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Seq(seq) => seq.size_hint(),
            Self::String(string) => string.size_hint(),
            Self::Map(map) => map.size_hint(),
            Self::Heap(heap) => heap.size_hint(),
            Self::Ints(ints) => ints.size_hint(),
            Self::Steps(_) => (0, None),
        }
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

/// Iterates over a string's characters, yielding each as a string of one
/// character. It reads the string as it is when it gets to each character,
/// by the character's position: a character added in a loop is visited, and
/// one removed before the position shifts the rest back, as with a list.
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
