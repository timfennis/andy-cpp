use crate::hash_map::HashMap;
use crate::interpreter::function::StaticType;
use crate::interpreter::heap::{MaxHeap, MinHeap};
use crate::interpreter::iterator::ValueIterator;
use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;

pub type DefaultMap<'a> = (&'a HashMap<Value, Value>, Option<Box<Value>>);
pub type DefaultMapMut<'a> = (&'a mut HashMap<Value, Value>, Option<Box<Value>>);
pub type ListRepr = Rc<RefCell<Vec<Value>>>;
pub type TupleRepr = Rc<Vec<Value>>;
pub type MapRepr = Rc<RefCell<HashMap<Value, Value>>>;
pub type StringRepr = Rc<RefCell<String>>;

#[derive(Clone)]
pub enum Sequence {
    String(Rc<RefCell<String>>),
    List(ListRepr),
    Tuple(TupleRepr),
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
            Self::String(string) => Some(string.borrow().chars().count()),
            Self::List(list) => Some(list.borrow().len()),
            Self::Tuple(tup) => Some(tup.len()),
            Self::Map(map, _) => Some(map.borrow().len()),
            Self::Iterator(_iter) => None,
            Self::MaxHeap(heap) => Some(heap.borrow().len()),
            Self::MinHeap(heap) => Some(heap.borrow().len()),
            Self::Deque(deque) => Some(deque.borrow().len()),
        }
    }

    #[must_use]
    pub fn deepcopy(&self) -> Self {
        match self {
            Self::List(l) => Self::List(Rc::new(RefCell::new(
                l.borrow()
                    .iter()
                    .map(Value::deepcopy)
                    .collect::<Vec<Value>>(),
            ))),
            Self::Map(m, def) => Self::Map(
                Rc::new(RefCell::new(
                    m.borrow()
                        .iter()
                        .map(|(key, value)| (key.deepcopy(), value.deepcopy()))
                        .collect::<HashMap<Value, Value>>(),
                )),
                def.as_deref().map(|v| Box::new(v.deepcopy())),
            ),
            // Since tuple has copy on write semantics we just don't do a deepcopy and nobody will know
            Self::Tuple(t) => Self::Tuple(t.clone()),
            Self::MaxHeap(heap) => Self::MaxHeap(Rc::new(RefCell::new(
                heap.borrow()
                    .iter()
                    .map(|v| v.deepcopy())
                    .collect::<MaxHeap>(),
            ))),
            Self::MinHeap(heap) => Self::MinHeap(Rc::new(RefCell::new(
                heap.borrow()
                    .iter()
                    .map(|v| v.0.deepcopy())
                    .collect::<MinHeap>(),
            ))),
            Self::Deque(deque) => Self::Deque(Rc::new(RefCell::new(
                deque
                    .borrow()
                    .iter()
                    .map(|v| v.deepcopy())
                    .collect::<VecDeque<Value>>(),
            ))),
            Self::String(s) => Self::String(Rc::new(RefCell::new(s.borrow().to_string()))),
            Self::Iterator(i) => {
                Self::Iterator(Rc::new(RefCell::new(ValueIterator::clone(&*i.borrow())))) // ???
            }
        }
    }

    pub fn value_type(&self) -> StaticType {
        match self {
            Self::String(_) => StaticType::String,
            Self::List(_) => StaticType::List,
            Self::Tuple(t) => StaticType::Tuple(t.iter().map(Value::static_type).collect()),
            Self::Map(_, _) => StaticType::Map,
            Self::Iterator(_) => StaticType::Iterator,
            Self::MaxHeap(_) => StaticType::MaxHeap,
            Self::MinHeap(_) => StaticType::MinHeap,
            Self::Deque(_) => StaticType::Deque,
        }
    }

    #[must_use]
    pub fn contains(&self, needle: &Value) -> bool {
        match self {
            Self::String(string) => match needle {
                Value::Sequence(Self::String(needle)) => {
                    if Rc::ptr_eq(string, needle) {
                        return true;
                    }

                    string.borrow().contains(needle.borrow().as_str())
                }
                _ => false,
            },
            Self::List(list) => list.borrow().contains(needle),
            Self::Tuple(tuple) => tuple.contains(needle),
            Self::Map(map, _) => map.borrow().contains_key(needle),
            Self::Iterator(iterator) => {
                let iter = ValueIterator::clone(&*iterator.borrow());
                match iter {
                    ValueIterator::ValueRange(range) => range.contains(needle),
                    ValueIterator::ValueRangeFrom(range) => range.contains(needle),
                    ValueIterator::ValueRangeInclusive(range) => range.contains(needle),
                    ValueIterator::Repeat(repeat) => &repeat.value == needle,
                }
            }
            Self::MaxHeap(heap) => heap.borrow().iter().any(|v| &v.0 == needle),
            Self::MinHeap(heap) => heap.borrow().iter().any(|v| &v.0.0 == needle),
            Self::Deque(deque) => deque.borrow().contains(needle),
        }
    }
}

impl PartialEq for Sequence {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::List(a), Self::List(b)) => a == b,
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            (Self::Map(a, _), Self::Map(b, _)) => a == b,
            (Self::Deque(a), Self::Deque(b)) => a == b,

            // These types can't really be compared for equality so they will just return true if they point to the same memory addr
            (Self::MaxHeap(a), Self::MaxHeap(b)) => Rc::ptr_eq(a, b),
            (Self::MinHeap(a), Self::MinHeap(b)) => Rc::ptr_eq(a, b),
            (Self::Iterator(a), Self::Iterator(b)) => Rc::ptr_eq(a, b),

            _ => false,
        }
    }
}

impl Eq for Sequence {}

impl PartialOrd for Sequence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::String(left), Self::String(right)) => left.partial_cmp(right),
            (Self::List(left), Self::List(right)) => left.partial_cmp(right),
            (Self::Tuple(left), Self::Tuple(right)) => left.partial_cmp(right),
            _ => None,
        }
    }
}

impl fmt::Debug for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "\"{}\"", s.borrow()),
            Self::List(vs) => {
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
            Self::Tuple(vs) => {
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
            Self::Map(dict, default) => {
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
            Self::Iterator(_) => {
                write!(f, "Iterator")
            }
            Self::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
            Self::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
            Self::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
        }
    }
}

impl fmt::Display for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s.borrow()),
            otherwise => write!(f, "{otherwise:?}"),
        }
    }
}
