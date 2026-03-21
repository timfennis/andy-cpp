use anyhow::Context;
use ndc_core::hash_map::HashMap;
use ndc_macros::export_module;
use ndc_vm::value::{Object, Value};
use num::ToPrimitive;
use serde_json::{Map, Number, Value as JsonValue, json};
use std::rc::Rc;

fn value_to_json(value: Value) -> Result<JsonValue, anyhow::Error> {
    match value {
        Value::None => Ok(JsonValue::Null),
        Value::Bool(b) => Ok(json!(b)),
        Value::Int(i) => Ok(json!(i)),
        Value::Float(f) => Ok(json!(f)),
        Value::Object(obj) => match obj.as_ref() {
            Object::Some(inner) => value_to_json(inner.clone()),
            Object::BigInt(big_int) => {
                use std::str::FromStr;
                Number::from_str(&big_int.to_string())
                    .map(JsonValue::Number)
                    .context("Cannot convert bigint to JSON number")
            }
            Object::Rational(ratio) => Ok(json!(ratio.to_f64())),
            Object::Complex(complex) => Ok(json!(format!("{complex}"))),
            Object::String(s) => Ok(json!(&*s.borrow())),
            Object::List(v) => Ok(JsonValue::Array(
                v.borrow()
                    .iter()
                    .map(|v| value_to_json(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Object::Tuple(v) => match v.len() {
                0 => Ok(JsonValue::Null),
                _ => Ok(JsonValue::Array(
                    v.iter()
                        .map(|v| value_to_json(v.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
            },
            Object::Map { entries, .. } => Ok(JsonValue::Object(
                entries
                    .borrow()
                    .iter()
                    .map(|(key, value)| {
                        value_to_json(value.clone()).map(|value| (key.to_string(), value))
                    })
                    .collect::<Result<Map<String, JsonValue>, _>>()?,
            )),
            Object::Iterator(i) => {
                let mut out = Vec::new();
                let mut iter = i.borrow_mut();
                while let Some(v) = iter.next() {
                    out.push(value_to_json(v)?);
                }
                Ok(JsonValue::Array(out))
            }
            Object::MaxHeap(h) => Ok(JsonValue::Array(
                h.borrow()
                    .iter()
                    .map(|v| value_to_json(v.0.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Object::MinHeap(h) => Ok(JsonValue::Array(
                h.borrow()
                    .iter()
                    .map(|v| value_to_json(v.0.0.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Object::Deque(d) => Ok(JsonValue::Array(
                d.borrow()
                    .iter()
                    .map(|v| value_to_json(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Object::Function(_) | Object::OverloadSet(_) => {
                Err(anyhow::anyhow!("Unable to serialize function"))
            }
        },
    }
}

fn json_to_value(value: JsonValue) -> Result<Value, anyhow::Error> {
    Ok(match value {
        JsonValue::Null => Value::unit(),
        JsonValue::Bool(b) => Value::Bool(b),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                return Err(anyhow::anyhow!("Cannot parse JSON number"));
            }
        }
        JsonValue::String(s) => Value::string(s),
        JsonValue::Array(a) => Value::list(
            a.into_iter()
                .map(json_to_value)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        JsonValue::Object(o) => Value::Object(Rc::new(Object::map(
            o.into_iter()
                .map(|(key, value)| json_to_value(value).map(|value| (Value::string(key), value)))
                .collect::<Result<HashMap<Value, Value>, _>>()?,
            None,
        ))),
    })
}

#[export_module]
mod inner {
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
