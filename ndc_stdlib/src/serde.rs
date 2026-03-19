use anyhow::Context;
use ndc_core::StaticType;
use ndc_core::hash_map::HashMap;
use ndc_macros::export_module;
use ndc_vm::value::{Object as VmObject, Value as VmValue};
use num::ToPrimitive;
use serde_json::{Map, Number, Value as JsonValue, json};
use std::rc::Rc;

fn value_to_json(value: VmValue) -> Result<JsonValue, anyhow::Error> {
    match value {
        VmValue::None => Ok(JsonValue::Null),
        VmValue::Bool(b) => Ok(json!(b)),
        VmValue::Int(i) => Ok(json!(i)),
        VmValue::Float(f) => Ok(json!(f)),
        VmValue::Object(obj) => match obj.as_ref() {
            VmObject::Some(inner) => value_to_json(inner.clone()),
            VmObject::BigInt(big_int) => {
                use std::str::FromStr;
                Number::from_str(&big_int.to_string())
                    .map(JsonValue::Number)
                    .context("Cannot convert bigint to JSON number")
            }
            VmObject::Rational(ratio) => Ok(json!(ratio.to_f64())),
            VmObject::Complex(complex) => Ok(json!(format!("{complex}"))),
            VmObject::String(s) => Ok(json!(&*s.borrow())),
            VmObject::List(v) => Ok(JsonValue::Array(
                v.borrow()
                    .iter()
                    .map(|v| value_to_json(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            VmObject::Tuple(v) => match v.len() {
                0 => Ok(JsonValue::Null),
                _ => Ok(JsonValue::Array(
                    v.iter()
                        .map(|v| value_to_json(v.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
            },
            VmObject::Map { entries, .. } => Ok(JsonValue::Object(
                entries
                    .borrow()
                    .iter()
                    .map(|(key, value)| {
                        value_to_json(value.clone()).map(|value| (key.to_string(), value))
                    })
                    .collect::<Result<Map<String, JsonValue>, _>>()?,
            )),
            VmObject::Iterator(i) => {
                let mut out = Vec::new();
                let mut iter = i.borrow_mut();
                while let Some(v) = iter.next() {
                    out.push(value_to_json(v)?);
                }
                Ok(JsonValue::Array(out))
            }
            VmObject::MaxHeap(h) => Ok(JsonValue::Array(
                h.borrow()
                    .iter()
                    .map(|v| value_to_json(v.0.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            VmObject::MinHeap(h) => Ok(JsonValue::Array(
                h.borrow()
                    .iter()
                    .map(|v| value_to_json(v.0.0.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            VmObject::Deque(d) => Ok(JsonValue::Array(
                d.borrow()
                    .iter()
                    .map(|v| value_to_json(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            VmObject::Function(_) | VmObject::OverloadSet(_) => {
                Err(anyhow::anyhow!("Unable to serialize function"))
            }
        },
    }
}

fn json_to_value(value: JsonValue) -> Result<VmValue, anyhow::Error> {
    Ok(match value {
        JsonValue::Null => VmValue::unit(),
        JsonValue::Bool(b) => VmValue::Bool(b),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                VmValue::Int(i)
            } else if let Some(f) = n.as_f64() {
                VmValue::Float(f)
            } else {
                return Err(anyhow::anyhow!("Cannot parse JSON number"));
            }
        }
        JsonValue::String(s) => VmValue::string(s),
        JsonValue::Array(a) => VmValue::list(
            a.into_iter()
                .map(json_to_value)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        JsonValue::Object(o) => VmValue::Object(Rc::new(VmObject::map(
            o.into_iter()
                .map(|(key, value)| json_to_value(value).map(|value| (VmValue::string(key), value)))
                .collect::<Result<HashMap<VmValue, VmValue>, _>>()?,
            None,
        ))),
    })
}

#[export_module]
mod inner {
    /// Converts a JSON string to a value
    pub fn json_decode(input: &str) -> anyhow::Result<ndc_vm::value::Value> {
        let json: JsonValue = serde_json::from_str(input)?;
        json_to_value(json)
    }

    /// Converts the input value to JSON
    pub fn json_encode(input: ndc_vm::value::Value) -> anyhow::Result<String> {
        let v = value_to_json(input)?;
        Ok(v.to_string())
    }
}
