use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use num::BigInt;

use crate::hash_map::{DefaultHasher, HashMap};
use crate::interpreter::function::{Function, OverloadedFunction};
use crate::interpreter::int::Int;
use crate::interpreter::num::{Number, NumberToUsizeError, NumberType};

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Clone)]
pub enum Value {
    Unit,
    Number(Number),
    Bool(bool),
    Sequence(Sequence),
    Function(Rc<RefCell<OverloadedFunction>>),
}

impl Value {
    #[must_use]
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Unit => ValueType::Unit,
            Value::Number(n) => ValueType::Number(n.into()),
            Value::Bool(_) => ValueType::Bool,
            Value::Sequence(Sequence::String(_)) => ValueType::String,
            Value::Sequence(Sequence::List(_)) => ValueType::List,
            Value::Sequence(Sequence::Tuple(_)) => ValueType::Tuple,
            Value::Function(_) => ValueType::Function,
            Value::Sequence(Sequence::Map(_, _)) => ValueType::Map,
        }
    }

    #[must_use]
    pub fn empty_list() -> Value {
        Value::Sequence(Sequence::List(Rc::new(RefCell::new(vec![]))))
    }

    // TODO: don't use anyhow here
    pub fn try_cmp(&self, other: &Value) -> Result<Ordering, anyhow::Error> {
        self.partial_cmp(other).ok_or_else(|| {
            anyhow::anyhow!(
                "{} cannot be compared to {}",
                self.value_type(),
                other.value_type()
            )
        })
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Unit => state.write_u8(1),
            Value::Number(number) => {
                state.write_u8(2);
                number.hash(state);
            }
            Value::Bool(true) => state.write_u8(3),
            Value::Bool(false) => state.write_u8(4),
            Value::Sequence(seq) => match seq {
                Sequence::String(string) => {
                    state.write_u8(5);
                    string.borrow().hash(state);
                }
                Sequence::List(list) => {
                    state.write_u8(6);
                    for item in list.borrow().iter() {
                        item.hash(state);
                    }
                }
                Sequence::Tuple(list) => {
                    state.write_u8(7);
                    for item in list.iter() {
                        item.hash(state);
                    }
                }
                // NOTE: the default value is not party of the identity of the map so %{1,2,3} == {:0,1,2,3}
                Sequence::Map(dict, _) => {
                    state.write_u8(8);
                    // This is 1 to 1 ripped from Noulith, and it's meant to ensure that if sets
                    // are equal {1,2,3} == {3,2,1} they produce the same hash regardless of the
                    // order the element appear in

                    let mut acc = 0u64;
                    let mut cube_acc = 0u64;
                    for (key, value) in dict.borrow().iter() {
                        let mut hasher = DefaultHasher::default();
                        key.hash(&mut hasher);
                        value.hash(&mut hasher);

                        let f = hasher.finish();
                        acc = acc.wrapping_add(f);
                        cube_acc = cube_acc.wrapping_add(f.wrapping_mul(f));
                    }
                    state.write_u64(acc);
                    state.write_u64(cube_acc);
                }
            },
            Value::Function(f) => {
                state.write_u8(9);
                Rc::as_ptr(f).hash(state);
            }
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
            (Self::Function(f1), Self::Function(f2)) => Rc::as_ptr(f1) == Rc::as_ptr(f2),
            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Some(Ordering::Equal),
            (Value::Number(left), Value::Number(right)) => left.partial_cmp(right),
            (Value::Sequence(left), Value::Sequence(right)) => left.partial_cmp(right),
            (Value::Bool(left), Value::Bool(right)) => left.partial_cmp(right),
            // Functions definitely don't have an order
            // Things that are different don't have an order either
            _ => None,
        }
    }
}

pub type DefaultMapMut<'a> = (&'a mut HashMap<Value, Value>, Option<Box<Value>>);
pub type DefaultMap<'a> = (&'a HashMap<Value, Value>, Option<Box<Value>>);

#[derive(Clone, PartialEq)]
pub enum Sequence {
    String(Rc<RefCell<String>>),
    List(Rc<RefCell<Vec<Value>>>),
    Tuple(Rc<Vec<Value>>),
    Map(Rc<RefCell<HashMap<Value, Value>>>, Option<Box<Value>>),
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

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Number(Number::Int(Int::Int64(i64::from(value))))
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        i64::try_from(value).map_or_else(|_| Value::from(BigInt::from(value)), Value::from)
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

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::Sequence(Sequence::String(Rc::new(RefCell::new(value.to_string()))))
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

impl From<OverloadedFunction> for Value {
    fn from(value: OverloadedFunction) -> Self {
        Self::Function(Rc::new(RefCell::new(value)))
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

impl TryFrom<&Value> for bool {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(bool) => Ok(*bool),
            v => Err(Self::Error::UnsupportedVariant(
                v.value_type(),
                stringify!(bool),
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

impl TryFrom<&Value> for usize {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(number) => Ok(usize::try_from(number.clone())?),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(usize),
            )),
        }
    }
}

// TODO: Should we implement `Deref` or something?
impl<'a> TryFrom<&'a Value> for &'a Sequence {
    type Error = ConversionError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Sequence(seq) => Ok(seq),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(&Sequence),
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ValueType {
    Unit,
    Number(NumberType),
    Bool,
    String,
    List,
    Tuple,
    Function,
    Map,
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
            Self::Tuple => write!(f, "tuple"),
            Self::Function => write!(f, "function"),
            ValueType::Map => write!(f, "map"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Function(_) => {
                write!(f, "function")
            }
            Self::Sequence(s) => write!(f, "{s:?}"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, ""),
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
