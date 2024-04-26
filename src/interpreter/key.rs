use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value};
use std::fmt;
use std::fmt::Formatter;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Key {
    Unit,
    Bool(bool),
    Number(Number),
    Tuple(Vec<Key>),
    List(Vec<Key>),
    String(String),
}

impl Eq for Key {} // TODO: figure out what the deal is here

impl TryFrom<&Value> for Key {
    type Error = ();

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Unit => Ok(Key::Unit),
            Value::Number(n) => Ok(Key::Number(n.clone())),
            Value::Bool(b) => Ok(Key::Bool(*b)),
            Value::Sequence(Sequence::String(string)) => {
                Ok(Self::String(string.borrow().to_string()))
            }
            Value::Sequence(Sequence::List(list)) => {
                let key = list
                    .borrow()
                    .iter()
                    .map(Key::try_from)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::List(key))
            }
            Value::Sequence(Sequence::Tuple(tuple)) => {
                let key = tuple
                    .iter()
                    .map(Key::try_from)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::Tuple(key))
            }
            Value::Sequence(Sequence::Dictionary(_dict)) => Err(()),
            Value::Function(_) => Err(()),
        }
    }
}

impl TryFrom<Value> for Key {
    // TODO: create a neat error type
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Unit => Ok(Self::Unit),
            Value::Number(n) => Ok(Self::Number(n)),
            Value::Bool(b) => Ok(Self::Bool(b)),

            Value::Sequence(Sequence::String(string)) => {
                Ok(Self::String(string.borrow().to_string()))
            }
            Value::Sequence(Sequence::List(list)) => {
                let keys = list
                    .borrow()
                    .iter()
                    .map(Key::try_from)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::List(keys))
            }
            Value::Sequence(Sequence::Tuple(tuple)) => match Rc::try_unwrap(tuple) {
                Ok(tuple) => Ok(Self::Tuple(
                    tuple
                        .into_iter()
                        .map(Key::try_from)
                        .collect::<Result<_, _>>()?,
                )),
                Err(tuple) => Ok(Self::Tuple(
                    tuple.iter().map(Key::try_from).collect::<Result<_, _>>()?,
                )),
            },
            Value::Sequence(_s) => Err(()),
            Value::Function(_f) => Err(()),
        }
    }
}

impl From<&Key> for Value {
    fn from(value: &Key) -> Self {
        match value {
            Key::Unit => Value::Unit,
            Key::Bool(b) => Value::Bool(*b),
            Key::Number(n) => Value::Number(n.clone()),
            Key::Tuple(t) => Value::Sequence(Sequence::Tuple(Rc::new(
                t.iter().map(Value::from).collect(),
            ))),
            Key::String(s) => Value::from(s.clone()),
            Key::List(l) => Value::from(l.iter().map(Value::from).collect::<Vec<Value>>()),
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Key::Unit => write!(f, "()"),
            Key::Bool(b) => write!(f, "{b}"),
            Key::Number(n) => write!(f, "{n}"),
            Key::String(s) => write!(f, "{s}"),
            //TODO: implement proper
            Key::Tuple(t) => write!(f, "{t:?}"),
            //TODO: implement proper
            Key::List(l) => write!(f, "{l:?}"),
        }
    }
}
