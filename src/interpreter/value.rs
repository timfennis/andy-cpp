use std::any::Any;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Range, RangeFrom, RangeInclusive};
use std::rc::Rc;

use itertools::Itertools;
use num::BigInt;

use crate::compare::FallibleOrd;
use crate::hash_map::DefaultHasher;
use crate::interpreter::function::{Function, OverloadedFunction};
use crate::interpreter::int::Int;
use crate::interpreter::num::{Number, NumberToUsizeError, NumberType};
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::ConversionError::{IncorrectLength, UnsupportedVariant};

use super::iterator::{ValueIterator, ValueRange, ValueRangeFrom, ValueRangeInclusive};

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Clone)]
pub enum Value {
    Option(Option<Box<Value>>),
    Number(Number),
    Bool(bool),
    Sequence(Sequence),
    Function(Rc<RefCell<OverloadedFunction>>),
}

impl Value {
    pub(crate) fn string<S: Into<String>>(string: S) -> Self {
        Value::Sequence(Sequence::String(Rc::new(RefCell::new(string.into()))))
    }

    pub(crate) fn list<V: Into<Vec<Value>>>(data: V) -> Self {
        Value::Sequence(Sequence::List(Rc::new(RefCell::new(data.into()))))
    }

    pub(crate) fn tuple<V: Into<Vec<Value>>>(data: V) -> Self {
        Value::Sequence(Sequence::Tuple(Rc::new(data.into())))
    }

    pub(crate) fn unit() -> Self {
        Value::Sequence(Sequence::Tuple(Rc::new(vec![])))
    }

    pub(crate) fn none() -> Self {
        Self::Option(None)
    }

    pub(crate) fn some(value: Value) -> Self {
        Self::Option(Some(Box::new(value)))
    }

    /// If this value is a type of `Sequence` it returns the length of the sequence, otherwise it returns `None`
    #[must_use]
    pub fn sequence_length(&self) -> Option<usize> {
        match self {
            Value::Sequence(seq) => seq.length(),
            _ => None,
        }
    }

    // The alternate solution in this SO thread has a nice way to iterate over the contents of a
    // RefCell but the iterator would not be compatible with non refcell items
    // https://stackoverflow.com/questions/33541492/returning-iterator-of-a-vec-in-a-refcell
    //
    // Note: this method is called `try_into_iter` but it doesn't always create an iterator over
    //       the original value. In most cases you get an iterator over a copy of the data.
    #[must_use]
    pub fn try_into_iter(self) -> Option<impl Iterator<Item = Value>> {
        match self {
            Value::Sequence(Sequence::List(list)) => match Rc::try_unwrap(list) {
                // This short circuit is almost certainly wrong because take will panic if list is borrowed
                Ok(list) => Some(list.into_inner().into_iter()),
                Err(list) => Some(Vec::clone(&*list.borrow()).into_iter()),
            },
            Value::Sequence(Sequence::Tuple(list)) => match Rc::try_unwrap(list) {
                Ok(list) => Some(list.into_iter()),
                Err(list) => Some(Vec::clone(&list).into_iter()),
            },
            Value::Sequence(Sequence::Map(map, _)) => {
                let x = map.borrow().keys().cloned().collect_vec().into_iter();
                Some(x)
            }
            Value::Sequence(Sequence::String(string)) => match Rc::try_unwrap(string) {
                // This implementation is peak retard, we don't want collect_vec here
                // ^-- WTF: is this comment, we collect_vec here anyways?
                Ok(string) => Some(
                    string
                        .into_inner()
                        .chars()
                        .map(Value::from)
                        .collect_vec()
                        .into_iter(),
                ),
                Err(string) => Some(
                    string
                        .borrow()
                        .chars()
                        .map(Value::from)
                        .collect_vec()
                        .into_iter(),
                ),
            },
            _ => None,
        }
    }

    #[must_use]
    /// Returns the `ValueType` associated with this value
    /// ```
    /// # use ndc_lib::interpreter::value::Value;
    /// # use ndc_lib::interpreter::value::ValueType;
    /// # use ndc_lib::interpreter::num::NumberType;
    /// let val = Value::from(1);
    /// assert_eq!(val.value_type(), NumberType::Int.into());
    /// ```
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Option(_) => ValueType::Option,
            Value::Number(n) => ValueType::Number(n.into()),
            Value::Bool(_) => ValueType::Bool,
            Value::Sequence(Sequence::String(_)) => ValueType::String,
            Value::Sequence(Sequence::List(_)) => ValueType::List,
            Value::Sequence(Sequence::Tuple(_)) => ValueType::Tuple,
            Value::Function(_) => ValueType::Function,
            Value::Sequence(Sequence::Map(_, _)) => ValueType::Map,
            Value::Sequence(Sequence::Iterator(_)) => ValueType::Iterator,
        }
    }

    #[must_use]
    pub fn empty_list() -> Value {
        Value::Sequence(Sequence::List(Rc::new(RefCell::new(vec![]))))
    }
}

impl FallibleOrd for Value {
    type Error = anyhow::Error;

    // TODO: do we really want to use anyhow here?
    fn try_cmp(&self, other: &Self) -> anyhow::Result<Ordering> {
        self.partial_cmp(other).ok_or_else(|| {
            anyhow::anyhow!(
                "{} cannot be compared to {}",
                self.value_type(),
                other.value_type()
            )
        })
    }
}

// TODO: is there a way to get rid of this implementation?!??!
impl FallibleOrd for &Value {
    type Error = anyhow::Error;

    fn try_cmp(&self, other: &Self) -> Result<Ordering, Self::Error> {
        self.partial_cmp(other).ok_or_else(|| {
            anyhow::anyhow!(
                "{} cannot be compared to {}",
                self.value_type(),
                other.value_type()
            )
        })
    }
}

// TODO: probably get rid of this and do explicit clones
impl From<&Value> for Value {
    fn from(value: &Value) -> Self {
        value.clone()
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Option(o) => {
                state.write_u8(1);
                o.hash(state);
            }
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
                Sequence::Iterator(i) => {
                    state.write_u8(9);
                    Rc::as_ptr(i).hash(state);
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
            (Self::Option(o1), Self::Option(o2)) => o1 == o2,
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
            (Value::Option(left), Value::Option(right)) => left.partial_cmp(right),
            (Value::Number(left), Value::Number(right)) => left.partial_cmp(right),
            (Value::Sequence(left), Value::Sequence(right)) => left.partial_cmp(right),
            (Value::Bool(left), Value::Bool(right)) => left.partial_cmp(right),
            // Functions definitely don't have an order
            // Things that are different don't have an order either
            _ => None,
        }
    }
}

// -----------------------------------------------------
// Into value
// -----------------------------------------------------

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Self::tuple(vec![])
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(Number::Float(value))
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

impl From<char> for Value {
    fn from(value: char) -> Self {
        // Haha so many heap allocations and decorators for a single character
        Self::Sequence(Sequence::String(Rc::new(RefCell::new(String::from(value)))))
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

impl From<ValueIterator> for Value {
    fn from(value: ValueIterator) -> Self {
        Self::Sequence(Sequence::Iterator(Rc::new(RefCell::new(value))))
    }
}

impl<'a> TryFrom<&'a mut Value> for &'a mut Sequence {
    type Error = &'static str;
    fn try_from(value: &'a mut Value) -> Result<Self, Self::Error> {
        match value {
            Value::Sequence(seq) => Ok(seq),
            _ => Err("Kapot"),
        }
    }
}

impl<'a> From<&'a mut Value> for &'a Value {
    fn from(value: &'a mut Value) -> Self {
        &*value
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

impl From<RangeInclusive<i64>> for Value {
    fn from(value: RangeInclusive<i64>) -> Self {
        Value::from(ValueIterator::ValueRangeInclusive(ValueRangeInclusive(
            value,
        )))
    }
}

impl From<RangeFrom<i64>> for Value {
    fn from(value: RangeFrom<i64>) -> Self {
        Value::from(ValueIterator::ValueRangeFrom(ValueRangeFrom(value)))
    }
}

impl From<Range<i64>> for Value {
    fn from(value: Range<i64>) -> Self {
        Value::from(ValueIterator::ValueRange(ValueRange(value)))
    }
}

// -----------------------------------------------------
// Out of value
// -----------------------------------------------------

#[derive(thiserror::Error, Debug)]
pub enum ConversionError {
    #[error("Cannot convert {0} into {1}")]
    UnsupportedVariant(ValueType, &'static str),

    #[error("Cannot into {0} because the length is incorrect")]
    IncorrectLength(&'static str),

    #[error("{0}")]
    NumberToUsizeError(#[from] NumberToUsizeError),
}

impl TryFrom<Value> for (Value, Value) {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let Value::Sequence(Sequence::Tuple(tuple)) = value else {
            return Err(UnsupportedVariant(
                value.value_type(),
                stringify!((Value, Value)),
            ));
        };

        // If we can take ownership of the vector we use pop otherwise we use get + clone
        let (right, left) = match Rc::try_unwrap(tuple) {
            Ok(mut tuple) => (tuple.pop(), tuple.pop()),
            Err(tuple) => (tuple.get(1).cloned(), tuple.first().cloned()),
        };

        if let (Some(left), Some(right)) = (left, right) {
            Ok((left, right))
        } else {
            Err(IncorrectLength(stringify!((Value, Value))))
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let typ = value.value_type();
        if let Value::Number(Number::Int(Int::Int64(i))) = value {
            return Ok(i);
        }

        if let Value::Number(Number::Int(i)) = value {
            if let Int::Int64(i) = i.simplified() {
                return Ok(i);
            }
        }

        return Err(Self::Error::UnsupportedVariant(typ, stringify!(i64)));
    }
}

impl TryFrom<&mut Value> for i64 {
    type Error = ConversionError;

    fn try_from(value: &mut Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(Number::Int(Int::Int64(i))) => Ok(*i),
            v => Err(Self::Error::UnsupportedVariant(
                v.value_type(),
                stringify!(i64),
            )),
        }
    }
}

impl TryFrom<&mut Value> for bool {
    type Error = ConversionError;

    fn try_from(value: &mut Value) -> Result<Self, Self::Error> {
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

impl TryFrom<&mut Value> for usize {
    type Error = ConversionError;

    fn try_from(value: &mut Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(number) => Ok(usize::try_from(number.clone())?),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(usize),
            )),
        }
    }
}

impl<'a> TryFrom<&'a mut Value> for &'a Sequence {
    type Error = ConversionError;

    fn try_from(value: &'a mut Value) -> Result<Self, Self::Error> {
        match value {
            Value::Sequence(seq) => Ok(seq),
            v => Err(ConversionError::UnsupportedVariant(
                v.value_type(),
                stringify!(&Sequence),
            )),
        }
    }
}

impl<'a> TryFrom<&'a mut Value> for &'a Number {
    type Error = ConversionError;

    fn try_from(value: &'a mut Value) -> Result<Self, Self::Error> {
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
    Option, // TODO: add type param
    Number(NumberType),
    Bool,
    String,
    List,
    Tuple,
    Function,
    Map,
    Iterator,
}

impl From<&Value> for ValueType {
    fn from(value: &Value) -> Self {
        value.value_type()
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Option => write!(f, "option"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::List => write!(f, "list"),
            Self::Tuple => write!(f, "tuple"),
            Self::Function => write!(f, "function"),
            Self::Map => write!(f, "map"),
            Self::Iterator => write!(f, "iterator"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Option(o) => write!(f, "{o:?}"),
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
            Self::Option(Some(v)) => write!(f, "Some({v})"),
            Self::Option(None) => write!(f, ""),
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Function(_) => {
                //TODO: implement function printing
                write!(f, "function")
            }
            // Unit tuple does not print anything
            Self::Sequence(Sequence::Tuple(t)) if t.len() == 0 => write!(f, ""),
            Self::Sequence(s) => write!(f, "{s}"),
        }
    }
}
