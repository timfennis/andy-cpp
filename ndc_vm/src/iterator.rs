use crate::Value;
use std::cell::RefCell;
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
}

pub type SharedIterator = Rc<RefCell<dyn VmIterator>>;

/// Exclusive range: start..end
pub struct RangeIter {
    current: i64,
    end: i64,
}

impl RangeIter {
    pub fn new(start: i64, end: i64) -> Self {
        Self { current: start, end }
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
