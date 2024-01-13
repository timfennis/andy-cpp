use crate::interpreter::function::Function;
use crate::interpreter::int::Int::Int64;
use crate::interpreter::Number;
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
    Function,
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
                ValueType::Function => "function",
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
            Value::Function(_) => ValueType::Function,
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
