use andy_cpp_macros::export_module;
use std::rc::Rc;
use std::{cell::RefCell, str::FromStr};

use crate::hash_map::HashMap;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use anyhow::Context;
use num::BigInt;
use num::ToPrimitive;
use serde_json::{json, Number, Value as JsonValue};

impl TryFrom<Value> for JsonValue {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Option(Some(value)) => JsonValue::try_from(*value),
            Value::Option(None) => Ok(JsonValue::Null),
            Value::Number(number) => match number {
                crate::interpreter::num::Number::Int(int) => match int {
                    crate::interpreter::int::Int::Int64(i) => Ok(json!(i)),
                    // Ehrmm..
                    crate::interpreter::int::Int::BigInt(big_int) => {
                        Number::from_str(&big_int.to_string())
                            .map(|n| JsonValue::Number(n))
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
                Sequence::List(values) => Ok(JsonValue::Array(
                    values
                        .borrow()
                        .iter()
                        .map(|v| v.clone().try_into())
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                Sequence::Tuple(values) => Ok(JsonValue::Array(
                    values
                        .iter()
                        .map(|v| v.clone().try_into())
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                Sequence::Map(values, value) => {
                    Ok(JsonValue::Object(values.borrow().iter().map(todo!(""))))
                }
                Sequence::Iterator(ref_cell) => todo!(),
                Sequence::MaxHeap(ref_cell) => todo!(),
                Sequence::MinHeap(ref_cell) => todo!(),
                Sequence::Deque(ref_cell) => todo!(),
            },
            Value::Function(_) => Err(anyhow::anyhow!("Unable to serialize function")),
        }
    }
}

impl TryFrom<JsonValue> for Value {
    type Error = anyhow::Error;

    fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
        Ok(match value {
            JsonValue::Null => Value::unit(),
            JsonValue::Bool(b) => Value::Bool(b),
            JsonValue::Number(n) => {
                n.as_str().parse::<BigInt>().map(Value::from).or_else(|_| {
                    n.as_f64()
                        .map(Value::from)
                        .context("Cannot parse number as int or float")
                })?
            }
            JsonValue::String(s) => Value::string(s),
            JsonValue::Array(a) => Value::list(
                a.into_iter()
                    .map(TryInto::try_into)
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            JsonValue::Object(o) => Value::Sequence(Sequence::Map(
                Rc::new(RefCell::new(
                    o.into_iter()
                        .map(|(key, value)| {
                            value.try_into().map(|value| (Value::string(key), value))
                        })
                        .collect::<Result<HashMap<Value, Value>, _>>()?,
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
