use ndc_macros::export_module;
use std::rc::Rc;
use std::{cell::RefCell, str::FromStr};

use anyhow::Context;
use ndc_lib::hash_map::HashMap;
use ndc_lib::interpreter::sequence::Sequence;
use ndc_lib::interpreter::value::Value;
use num::BigInt;
use num::ToPrimitive;
use serde_json::{Map, Number, Value as JsonValue, json};

fn value_to_json(value: Value) -> Result<JsonValue, anyhow::Error> {
    match value {
        Value::Option(Some(value)) => value_to_json(*value),
        Value::Option(None) => Ok(JsonValue::Null),
        Value::Number(number) => match number {
            ndc_lib::interpreter::num::Number::Int(int) => match int {
                ndc_lib::interpreter::int::Int::Int64(i) => Ok(json!(i)),
                ndc_lib::interpreter::int::Int::BigInt(big_int) => {
                    Number::from_str(&big_int.to_string())
                        .map(JsonValue::Number)
                        .context("Cannot convert bigint to string")
                }
            },
            ndc_lib::interpreter::num::Number::Float(f) => Ok(json!(f)),
            ndc_lib::interpreter::num::Number::Rational(ratio) => Ok(json!(ratio.to_f64())),
            ndc_lib::interpreter::num::Number::Complex(complex) => Ok(json!(format!("{complex}"))),
        },
        Value::Bool(b) => Ok(json!(b)),
        Value::Sequence(s) => match s {
            Sequence::String(s) => Ok(json!(&*s.borrow())),
            Sequence::List(values) => Ok(JsonValue::Array(
                values
                    .borrow()
                    .iter()
                    .map(|v| value_to_json(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Sequence::Tuple(values) => match values.len() {
                0 => Ok(JsonValue::Null),
                _ => Ok(JsonValue::Array(
                    values
                        .iter()
                        .map(|v| value_to_json(v.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
            },
            Sequence::Map(values, _) => Ok(JsonValue::Object(
                values
                    .borrow()
                    .iter()
                    .map(|(key, value)| {
                        value_to_json(value.clone()).map(|value| (key.to_string(), value))
                    })
                    .collect::<Result<Map<String, JsonValue>, _>>()?,
            )),
            Sequence::Iterator(i) => {
                let mut i = i.borrow_mut();
                let mut out = Vec::new();
                for value in i.by_ref() {
                    out.push(value_to_json(value)?);
                }
                Ok(JsonValue::Array(out))
            }
            Sequence::MaxHeap(h) => Ok(JsonValue::Array(
                h.borrow()
                    .iter()
                    .map(|h| value_to_json(h.0.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Sequence::MinHeap(h) => Ok(JsonValue::Array(
                h.borrow()
                    .iter()
                    .map(|h| value_to_json(h.0.0.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Sequence::Deque(d) => Ok(JsonValue::Array(
                d.borrow()
                    .iter()
                    .map(|v| value_to_json(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        },
        Value::Function(_) => Err(anyhow::anyhow!("Unable to serialize function")),
    }
}

fn json_to_value(value: JsonValue) -> Result<Value, anyhow::Error> {
    Ok(match value {
        JsonValue::Null => Value::unit(),
        JsonValue::Bool(b) => Value::Bool(b),
        JsonValue::Number(n) => n.as_str().parse::<BigInt>().map(Value::from).or_else(|_| {
            n.as_f64()
                .map(Value::from)
                .context("Cannot parse number as int or float")
        })?,
        JsonValue::String(s) => Value::string(s),
        JsonValue::Array(a) => Value::list(
            a.into_iter()
                .map(json_to_value)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        JsonValue::Object(o) => Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(
                o.into_iter()
                    .map(|(key, value)| {
                        json_to_value(value).map(|value| (Value::string(key), value))
                    })
                    .collect::<Result<HashMap<Value, Value>, _>>()?,
            )),
            None,
        )),
    })
}

#[export_module]
mod inner {
    use ndc_lib::interpreter::value::Value;

    /// Converts a JSON string to a value
    pub fn json_decode(input: &str) -> anyhow::Result<Value> {
        let json: JsonValue = serde_json::from_str(input)?;
        json_to_value(json)
    }

    /// Converts the input value to JSON
    pub fn json_encode(input: Value) -> anyhow::Result<String> {
        let v = value_to_json(input)?;
        Ok(v.to_string())
    }
}
