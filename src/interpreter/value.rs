use crate::interpreter::int::Int::Int64;
use crate::interpreter::Number;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Number(Number),
    Bool(bool),
    Sequence(Sequence),
    // TODO: add structs or classes
    // TODO: add functions
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sequence {
    String(Rc<String>),
    List(Rc<VecDeque<Value>>),
    //TODO: Dict comes later because we need hashing and comparison
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Unit,
    Number,
    Bool,
    String,
    List,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValueType::Unit => "unit",
                ValueType::Number => "number",
                ValueType::Bool => "bool",
                ValueType::String => "string",
                ValueType::List => "list",
            }
        )
    }
}

impl From<Value> for ValueType {
    fn from(value: Value) -> Self {
        match value {
            Value::Unit => ValueType::Unit,
            Value::Number(_) => ValueType::Number,
            Value::Bool(_) => ValueType::Bool,
            Value::Sequence(Sequence::String(_)) => ValueType::String,
            Value::Sequence(Sequence::List(_)) => ValueType::List,
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Number(Number::Int(Int64(value)))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Sequence(Sequence::String(s)) => write!(f, "{s}"),
            d => write!(f, "{d:?}"),
        }
    }
}
