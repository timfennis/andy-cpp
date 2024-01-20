use crate::interpreter::function::Function;
use crate::interpreter::int::Int::Int64;
use crate::interpreter::num::{Number, NumberType};
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(Number),
    Bool(bool),
    Sequence(Sequence),
    Function(Rc<dyn Function>),
    // TODO: add structs or classes
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Number(n1), Self::Number(n2)) => n1.eq(n2),
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            (Self::Sequence(s1), Self::Sequence(s2)) => s1.eq(s2),
            _ => false,
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Number(Number::Int(Int64(value)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sequence {
    String(Rc<String>),
    List(Rc<VecDeque<Value>>),
    //TODO: Dict comes later because we need hashing and comparison
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ValueType {
    Unit,
    Number(NumberType),
    Bool,
    String,
    List(Option<Box<ValueType>>),
    Function,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "unit"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::List(t) => {
                if let Some(tt) = t {
                    write!(f, "[{tt}]")
                } else {
                    write!(f, "[]")
                }
            }
            Self::Function => write!(f, "function"),
        }
    }
}

impl From<&Value> for ValueType {
    fn from(value: &Value) -> Self {
        match value {
            Value::Unit => Self::Unit,
            Value::Number(n) => Self::Number(n.into()),
            Value::Bool(_) => Self::Bool,
            Value::Sequence(Sequence::String(_)) => Self::String,
            Value::Sequence(Sequence::List(t)) => {
                let t = t.front().map(|it| Box::new(ValueType::from(it)));
                Self::List(t)
            }
            Value::Function(_) => Self::Function,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Function(_) => {
                //TODO: implement function printing
                write!(f, "function")
            }
            Self::Sequence(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Sequence {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Sequence::String(s) => write!(f, "{s}"),
            Sequence::List(vs) => {
                write!(f, "[")?;
                let mut vs = vs.iter().peekable();
                while let Some(v) = vs.next() {
                    if vs.peek().is_some() {
                        write!(f, "{v},")?;
                    } else {
                        write!(f, "{v}")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}
