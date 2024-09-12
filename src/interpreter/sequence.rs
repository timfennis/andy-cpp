use crate::hash_map::HashMap;
use crate::interpreter::iterator::ValueIterator;
use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::cmp::Ordering;
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
}

impl Sequence {
    pub fn length(&self) -> Option<usize> {
        match self {
            Sequence::String(string) => Some(string.borrow().chars().count()),
            Sequence::List(list) => Some(list.borrow().len()),
            Sequence::Tuple(tup) => Some(tup.len()),
            Sequence::Map(map, _) => Some(map.borrow().len()),
            Sequence::Iterator(iter) => None,
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
                    if value == &Value::Unit {
                        write!(f, "{key:?}")?;
                    } else {
                        write!(f, "{key:?}: {value:?}")?;
                    }

                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                }
                write!(f, "}}")
            }
            Sequence::Iterator(_) => {
                write!(f, "iterator")
            }
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
