use ndc_macros::export_module;
use std::rc::Rc;
use std::{cell::RefCell, str::FromStr};

use crate::hash_map::HashMap;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use anyhow::Context;
use num::BigInt;
use num::ToPrimitive;
use serde_json::{Map, Number, Value as JsonValue, json};

impl TryFrom<Value> for JsonValue {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Option(Some(value)) => Self::try_from(*value),
            Value::Option(None) => Ok(Self::Null),
            Value::Number(number) => match number {
                crate::interpreter::num::Number::Int(int) => match int {
                    crate::interpreter::int::Int::Int64(i) => Ok(json!(i)),
                    // Ehrmm..
                    crate::interpreter::int::Int::BigInt(big_int) => {
                        Number::from_str(&big_int.to_string())
                            .map(JsonValue::Number)
                            .context("Cannot convert bigint to string")
                    }
                },
                crate::interpreter::num::Number::Float(f) => Ok(json!(f)),
                crate::interpreter::num::Number::Rational(ratio) => Ok(json!(ratio.to_f64())),
                crate::interpreter::num::Number::Complex(complex) => {
                    Ok(json!(format!("{complex}")))
                }
            },
            Value::Bool(b) => Ok(json!(b)),
            Value::Sequence(s) => match s {
                Sequence::String(s) => Ok(json!(&*s.borrow())),
                Sequence::List(values) => Ok(Self::Array(
                    values
                        .borrow()
                        .iter()
                        .map(|v| v.clone().try_into())
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                Sequence::Tuple(values) => Ok(Self::Array(
                    values
                        .iter()
                        .map(|v| v.clone().try_into())
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                Sequence::Map(values, _) => Ok(Self::Object(
                    values
                        .borrow()
                        .iter()
                        .map(|(key, value)| {
                            Self::try_from(value.clone()).map(|value| (key.to_string(), value))
                        })
                        .collect::<Result<Map<String, Self>, _>>()?,
                )),
                Sequence::Iterator(i) => {
                    let mut i = i.borrow_mut();
                    let mut out = Vec::new();
                    for value in i.by_ref() {
                        out.push(Self::try_from(value)?);
                    }
                    Ok(Self::Array(out))
                }
                Sequence::MaxHeap(h) => Ok(Self::Array(
                    h.borrow()
                        .iter()
                        .map(|h| Self::try_from(h.0.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                Sequence::MinHeap(h) => Ok(Self::Array(
                    h.borrow()
                        .iter()
                        .map(|h| Self::try_from(h.0.0.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                Sequence::Deque(d) => Ok(Self::Array(
                    d.borrow()
                        .iter()
                        .map(|v| Self::try_from(v.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
            },
            Value::Function(_) => Err(anyhow::anyhow!("Unable to serialize function")),
        }
    }
}

impl TryFrom<JsonValue> for Value {
    type Error = anyhow::Error;

    fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
        Ok(match value {
            JsonValue::Null => Self::unit(),
            JsonValue::Bool(b) => Self::Bool(b),
            JsonValue::Number(n) => n.as_str().parse::<BigInt>().map(Self::from).or_else(|_| {
                n.as_f64()
                    .map(Self::from)
                    .context("Cannot parse number as int or float")
            })?,
            JsonValue::String(s) => Self::string(s),
            JsonValue::Array(a) => Self::list(
                a.into_iter()
                    .map(TryInto::try_into)
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            JsonValue::Object(o) => Self::Sequence(Sequence::Map(
                Rc::new(RefCell::new(
                    o.into_iter()
                        .map(|(key, value)| {
                            value.try_into().map(|value| (Self::string(key), value))
                        })
                        .collect::<Result<HashMap<Self, Self>, _>>()?,
                )),
                None,
            )),
        })
    }
}

#[export_module]
mod inner {
    use crate::interpreter::value::Value;

    pub fn json_decode(input: &str) -> anyhow::Result<Value> {
        serde_json::from_str::<JsonValue>(input)?.try_into()
    }

    pub fn json_encode(input: Value) -> anyhow::Result<Value> {
        let v: JsonValue = input.try_into()?;
        Ok(Value::string(v.to_string()))
    }
}
