use crate::hash_map::HashMap;
use crate::interpreter::heap::{MaxHeap, MinHeap};
use crate::interpreter::iterator::ValueIterator;
use crate::interpreter::value::{Value, ValueType};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;

pub type DefaultMapMut<'a> = (&'a mut HashMap<Value, Value>, Option<Box<Value>>);
pub type DefaultMap<'a> = (&'a HashMap<Value, Value>, Option<Box<Value>>);

#[derive(Clone)]
pub enum Sequence {
    String(Rc<RefCell<String>>),
    List(Rc<RefCell<Vec<Value>>>),
    Tuple(Rc<Vec<Value>>),
    Map(Rc<RefCell<HashMap<Value, Value>>>, Option<Box<Value>>),
    Iterator(Rc<RefCell<ValueIterator>>),
    MaxHeap(Rc<RefCell<MaxHeap>>),
    MinHeap(Rc<RefCell<MinHeap>>),
    Deque(Rc<RefCell<VecDeque<Value>>>),
}

impl Sequence {
    #[must_use]
    pub fn length(&self) -> Option<usize> {
        match self {
            Sequence::String(string) => Some(string.borrow().chars().count()),
            Sequence::List(list) => Some(list.borrow().len()),
            Sequence::Tuple(tup) => Some(tup.len()),
            Sequence::Map(map, _) => Some(map.borrow().len()),
            Sequence::Iterator(_iter) => None,
            Sequence::MaxHeap(heap) => Some(heap.borrow().len()),
            Sequence::MinHeap(heap) => Some(heap.borrow().len()),
            Sequence::Deque(deque) => Some(deque.borrow().len()),
        }
    }

    #[must_use]
    pub fn deepcopy(&self) -> Sequence {
        match self {
            Sequence::List(l) => Sequence::List(Rc::new(RefCell::new(
                l.borrow()
                    .iter()
                    .map(Value::deepcopy)
                    .collect::<Vec<Value>>(),
            ))),
            Sequence::Map(m, def) => Sequence::Map(
                Rc::new(RefCell::new(
                    m.borrow()
                        .iter()
                        .map(|(key, value)| (key.deepcopy(), value.deepcopy()))
                        .collect::<HashMap<Value, Value>>(),
                )),
                def.as_deref().map(|v| Box::new(v.deepcopy())),
            ),
            // Since tuple has copy on write semantics we just don't do a deepcopy and nobody will know
            Sequence::Tuple(t) => Sequence::Tuple(t.clone()),
            Sequence::MaxHeap(heap) => Sequence::MaxHeap(Rc::new(RefCell::new(
                heap.borrow()
                    .iter()
                    .map(|v| v.deepcopy())
                    .collect::<MaxHeap>(),
            ))),
            Sequence::MinHeap(heap) => Sequence::MinHeap(Rc::new(RefCell::new(
                heap.borrow()
                    .iter()
                    .map(|v| v.0.deepcopy())
                    .collect::<MinHeap>(),
            ))),
            Sequence::String(s) => Sequence::String(Rc::new(RefCell::new(s.borrow().to_string()))),
            _ => todo!("deepcopy is not yet implemented for this type"),
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Sequence::String(_) => ValueType::String,
            Sequence::List(_) => ValueType::List,
            Sequence::Tuple(t) => ValueType::Tuple(t.iter().map(Value::value_type).collect()),
            Sequence::Map(_, _) => ValueType::Map,
            Sequence::Iterator(_) => ValueType::Iterator,
            Sequence::MaxHeap(_) => ValueType::MaxHeap,
            Sequence::MinHeap(_) => ValueType::MinHeap,
            Sequence::Deque(_) => ValueType::Deque,
        }
    }

    #[must_use]
    pub fn contains(&self, needle: &Value) -> bool {
        match self {
            Sequence::String(string) => match needle {
                Value::Sequence(Sequence::String(needle)) => {
                    if Rc::ptr_eq(string, needle) {
                        return true;
                    }

                    string.borrow().contains(needle.borrow().as_str())
                }
                _ => false,
            },
            Sequence::List(list) => list.borrow().contains(needle),
            Sequence::Tuple(tuple) => tuple.contains(needle),
            Sequence::Map(map, _) => map.borrow().contains_key(needle),
            Sequence::Iterator(iterator) => {
                let iter = ValueIterator::clone(&*iterator.borrow());
                match iter {
                    ValueIterator::ValueRange(range) => range.contains(needle),
                    ValueIterator::ValueRangeFrom(range) => range.contains(needle),
                    ValueIterator::ValueRangeInclusive(range) => range.contains(needle),
                }
            }
            Sequence::MaxHeap(heap) => heap.borrow().iter().any(|v| &v.0 == needle),
            Sequence::MinHeap(heap) => heap.borrow().iter().any(|v| &v.0 .0 == needle),
            Sequence::Deque(deque) => deque.borrow().contains(needle),
        }
    }
}

impl PartialEq for Sequence {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Sequence::String(a), Sequence::String(b)) => a == b,
            (Sequence::List(a), Sequence::List(b)) => a == b,
            (Sequence::Tuple(a), Sequence::Tuple(b)) => a == b,
            (Sequence::Map(a, _), Sequence::Map(b, _)) => a == b,
            (Sequence::Deque(a), Sequence::Deque(b)) => a == b,

            // These types can't really be compared for equality so they will just return true if they point to the same memory addr
            (Sequence::MaxHeap(a), Sequence::MaxHeap(b)) => Rc::ptr_eq(a, b),
            (Sequence::MinHeap(a), Sequence::MinHeap(b)) => Rc::ptr_eq(a, b),
            (Sequence::Iterator(a), Sequence::Iterator(b)) => Rc::ptr_eq(a, b),

            _ => false,
        }
    }
}

impl Eq for Sequence {}

impl PartialOrd for Sequence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Sequence::String(left), Sequence::String(right)) => left.partial_cmp(right),
            (Sequence::List(left), Sequence::List(right)) => left.partial_cmp(right),
            (Sequence::Tuple(left), Sequence::Tuple(right)) => left.partial_cmp(right),
            _ => None,
        }
    }
}

impl fmt::Debug for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sequence::String(s) => write!(f, "\"{}\"", s.borrow()),
            Sequence::List(vs) => {
                write!(f, "[")?;
                let vs = vs.borrow();
                let mut vs = vs.iter().peekable();
                while let Some(v) = vs.next() {
                    if vs.peek().is_some() {
                        write!(f, "{v:?},")?;
                    } else {
                        write!(f, "{v:?}")?;
                    }
                }
                write!(f, "]")
            }
            Sequence::Tuple(vs) => {
                write!(f, "(")?;
                let mut iter = vs.iter().peekable();
                while let Some(v) = iter.next() {
                    write!(f, "{v:?}")?;
                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            }
            Sequence::Map(dict, default) => {
                let dict = dict.borrow();
                let mut iter = dict.iter().peekable();
                if let Some(default) = default {
                    write!(f, "{{default: {default:?}")?;
                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                } else {
                    write!(f, "{{")?;
                }
                while let Some((key, value)) = iter.next() {
                    match value {
                        Value::Option(opt) if opt.is_none() => write!(f, "{key:?}")?,
                        _ => write!(f, "{key:?}: {value:?}")?,
                    }

                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                }
                write!(f, "}}")
            }
            Sequence::Iterator(_) => {
                write!(f, "Iterator")
            }
            Sequence::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
            Sequence::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
            Sequence::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
        }
    }
}

impl fmt::Display for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sequence::String(s) => write!(f, "{}", s.borrow()),
            otherwise => write!(f, "{otherwise:?}"),
        }
    }
}
