use crate::interpreter::function::Function;
use crate::interpreter::int::Int;
use crate::interpreter::num::{Number, NumberToUsizeError, NumberType};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt;
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
}

impl Value {
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Unit => ValueType::Unit,
            Value::Number(n) => ValueType::Number(n.into()),
            Value::Bool(_) => ValueType::Bool,
            Value::Sequence(Sequence::String(_)) => ValueType::String,
            Value::Sequence(Sequence::List(t)) => {
                let t = t.borrow().front().map(|it| Box::new(ValueType::from(it)));
                ValueType::List(t)
            }
            Value::Function(_) => ValueType::Function,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sequence {
    String(Rc<RefCell<String>>),
    List(Rc<RefCell<VecDeque<Value>>>),
    //TODO: Dict comes later because we need hashing and comparison
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Number(n1), Self::Number(n2)) => n1.eq(n2),
            (Self::Bool(b1), Self::Bool(b2)) => b1.eq(b2),
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
        Self::Number(Number::Int(Int::Int64(value)))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ValueToIntError {
    #[error("Cannot convert {0} to int")]
    UnsupportedVariant(ValueType),
}

impl TryFrom<Value> for i64 {
    type Error = ValueToIntError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(Number::Int(Int::Int64(i))) => Ok(i),
            v => Err(Self::Error::UnsupportedVariant(v.value_type())),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ValueToUsizeError {
    #[error(transparent)]
    NumberToUsize(#[from] NumberToUsizeError),

    #[error("cannot convert from {0} to usize")]
    UnsupportedVariant(ValueType),
}

impl TryFrom<Value> for usize {
    type Error = ValueToUsizeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(usize::try_from(n)?),
            v => Err(Self::Error::UnsupportedVariant(v.value_type())),
        }
    }
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

impl From<&Value> for ValueType {
    fn from(value: &Value) -> Self {
        value.value_type()
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl fmt::Display for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sequence::String(s) => write!(f, "{}", s.borrow()),
            Sequence::List(vs) => {
                write!(f, "[")?;
                let vs = vs.borrow();
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
