use andy_cpp_macros::export_module;
use std::cell::RefCell;
use std::rc::Rc;

use crate::hash_map::HashMap;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use anyhow::Context;
use num::BigInt;
use serde_json::Value as JsonValue;

fn try_into_andy_cpp_value(value: JsonValue) -> anyhow::Result<Value> {
    Ok(match value {
        JsonValue::Null => Value::unit(),
        JsonValue::Bool(b) => Value::Bool(b),
        // TODO: also handle i128 for completeness
        JsonValue::Number(n) => n.as_str().parse::<BigInt>().map(Value::from).or_else(|_| {
            n.as_f64()
                .map(Value::from)
                .context("Cannot parse number as int or float")
        })?,
        JsonValue::String(s) => Value::string(s),
        JsonValue::Array(a) => Value::list(
            a.into_iter()
                .map(try_into_andy_cpp_value)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        JsonValue::Object(o) => Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(
                o.into_iter()
                    .map(|(key, value)| {
                        try_into_andy_cpp_value(value).map(|value| (Value::string(key), value))
                    })
                    .collect::<Result<HashMap<Value, Value>, _>>()?,
            )),
            None,
        )),
    })
}
#[export_module]
mod inner {
    use crate::interpreter::value::Value;

    pub fn json_decode(input: &str) -> anyhow::Result<Value> {
        let v = serde_json::from_str::<JsonValue>(input)?;
        try_into_andy_cpp_value(v)
    }
}
