use crate::interpreter::function::{Function, OverloadedFunction};
use crate::interpreter::int::Int;
use crate::interpreter::num::{Number, NumberToUsizeError, NumberType};
use num::BigInt;
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
    Function(Rc<RefCell<OverloadedFunction>>),
}

impl Value {
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Unit => ValueType::Unit,
            Value::Number(n) => ValueType::Number(n.into()),
            Value::Bool(_) => ValueType::Bool,
            Value::Sequence(Sequence::String(_)) => ValueType::String,
            Value::Sequence(Sequence::List(_)) => ValueType::List,
            Value::Function(_) => ValueType::Function,
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub enum Sequence {
    String(Rc<RefCell<String>>),
    List(Rc<RefCell<VecDeque<Value>>>),
    //TODO: Dict comes later because we need hashing and comparison
}

// -----------------------------------------------------
// Into value
// -----------------------------------------------------

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Self::Unit
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

impl From<BigInt> for Value {
    fn from(value: BigInt) -> Self {
        Self::Number(Number::Int(Int::BigInt(value)))
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Sequence(Sequence::String(Rc::new(RefCell::new(value))))
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Function(Rc::new(RefCell::new(OverloadedFunction::from(value))))
    }
}

// TODO: is this implementation useful, should it be over an Iterator of some kind instead
impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Self::Sequence(Sequence::List(Rc::new(RefCell::new(
            value.into_iter().map(Into::into).collect(),
        ))))
    }
}

// impl<T: Into<Value>> From<dyn Iterator<Item = T>> for Value {
//     fn from(value: dyn Iterator<Item = T>) -> Self {
//         Self::Sequence(Sequence::List(Rc::new(RefCell::new(
//             value.into_iter().map(Into::into).collect(),
//         ))))
//     }
// }

impl From<Number> for Value {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}

impl From<Sequence> for Value {
    fn from(value: Sequence) -> Self {
        Self::Sequence(value)
    }
}

// -----------------------------------------------------
// Out of value
// -----------------------------------------------------

#[derive(thiserror::Error, Debug)]
pub enum ConversionError {
    #[error("Cannot convert {0} into {1}")]
    UnsupportedVariant(ValueType, &'static str),

    #[error("{0}")]
    NumberToUsizeError(#[from] NumberToUsizeError),
}

impl TryFrom<Value> for i64 {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(Number::Int(Int::Int64(i))) => Ok(i),
            v => Err(Self::Error::UnsupportedVariant(
                v.value_type(),
                stringify!(i64),
            )),
        }
    }
}

impl TryFrom<&Value> for i64 {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(Number::Int(Int::Int64(i))) => Ok(*i),
            v => Err(Self::Error::UnsupportedVariant(
                v.value_type(),
                stringify!(i64),
            )),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(usize::try_from(n)?),
            v => Err(Self::Error::UnsupportedVariant(
                v.value_type(),
                stringify!(usize),
            )),
        }
    }
}

impl TryFrom<Value> for Number {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(n),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(Number),
            )),
        }
    }
}

impl TryFrom<Value> for BigInt {
    type Error = ConversionError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(Number::Int(Int::BigInt(b))) => Ok(b),
            Value::Number(Number::Int(Int::Int64(i))) => Ok(BigInt::from(i)),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(BigInt),
            )),
        }
    }
}

// FIXME: We should not need to convert `&Value` into `BigInt`, we should use `&BigInt` instead.
//        But we can't return references to BigInt in case the source type is i64. A possible
//        solution could be to create the BigInt instance from the i64 as part of the attribute
//        macro
impl TryFrom<&Value> for BigInt {
    type Error = ConversionError;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(Number::Int(Int::BigInt(b))) => Ok(b.clone()),
            Value::Number(Number::Int(Int::Int64(i))) => Ok(BigInt::from(*i)),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(BigInt),
            )),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Number {
    type Error = ConversionError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(n),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                "&Number",
            )),
        }
    }
}

// TODO: This implementation is trash
impl TryFrom<&Value> for String {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Sequence(Sequence::String(string)) => Ok(string.borrow().clone()), // TODO: no clonerino pls, just take ref
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                "String",
            )),
        }
    }
}

// ValueType

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ValueType {
    Unit,
    Number(NumberType),
    Bool,
    String,
    List,
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
            Self::List => write!(f, "list"),
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
