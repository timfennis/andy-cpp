use anyhow::{Context, bail};
use ndc_core::hash_map::HashMap;
use ndc_macros::export_module;
use ndc_vm::value::{Object, Value};
use num::ToPrimitive;
use serde_json::{Map, Number, Value as JsonValue, json};
use std::collections::HashSet;
use std::rc::Rc;
use std::str::FromStr;

/// Converts a value to JSON. In strict mode (`lossy == false`) any value that
/// would not survive `json_decode(json_encode(value)) == value` is rejected
/// with an error. In lossy mode every value that can be reasonably represented
/// is accepted: rationals become floats, complex numbers become strings,
/// options are unwrapped, tuples and deques become arrays, heaps become arrays
/// in priority order, iterators are drained, structs become objects, non-string
/// map keys are stringified, and `None` and non-finite floats become null.
///
/// `active` holds the containers currently being converted on the recursion
/// path, so a value that (transitively) contains itself is detected instead of
/// recursing forever.
fn value_to_json(
    value: &Value,
    lossy: bool,
    active: &mut HashSet<*const Object>,
) -> Result<JsonValue, anyhow::Error> {
    match value {
        Value::None if lossy => Ok(JsonValue::Null),
        Value::None => bail!("cannot convert None to JSON, JSON null maps to the unit value ()"),
        Value::Bool(b) => Ok(json!(b)),
        Value::Int(i) => Ok(json!(i)),
        Value::Float(f) if f.is_finite() => Ok(json!(f)),
        Value::Float(_) if lossy => Ok(JsonValue::Null),
        Value::Float(f) => bail!("cannot convert non-finite float {f} to JSON"),
        Value::Object(obj) => {
            // Only these variants have interior mutability through which a
            // value can contain itself; all other variants are leaves or
            // immutable, so they never need to be tracked.
            let cycle_guard = matches!(
                obj.as_ref(),
                Object::List(_)
                    | Object::Deque(_)
                    | Object::Map { .. }
                    | Object::MaxHeap(_)
                    | Object::MinHeap(_)
                    | Object::Iterator(_)
                    | Object::Struct { .. }
            );
            if cycle_guard && !active.insert(Rc::as_ptr(obj)) {
                bail!("cannot convert a value that contains itself to JSON");
            }
            let result = object_to_json(obj, lossy, active);
            if cycle_guard {
                active.remove(&Rc::as_ptr(obj));
            }
            result
        }
    }
}

fn object_to_json(
    obj: &Rc<Object>,
    lossy: bool,
    active: &mut HashSet<*const Object>,
) -> Result<JsonValue, anyhow::Error> {
    let values_to_array = |values: &mut dyn Iterator<Item = &Value>,
                           active: &mut HashSet<*const Object>| {
        values
            .map(|v| value_to_json(v, lossy, active))
            .collect::<Result<Vec<_>, _>>()
            .map(JsonValue::Array)
    };

    match obj.as_ref() {
        Object::BigInt(big_int) => Number::from_str(&big_int.to_string())
            .map(JsonValue::Number)
            .context("cannot convert bigint to JSON number"),
        Object::Rational(ratio) if lossy => Ok(json!(ratio.to_f64())),
        Object::Rational(_) => {
            bail!("cannot convert a rational number to JSON, convert it to a float first")
        }
        Object::Complex(complex) if lossy => Ok(json!(format!("{complex}"))),
        Object::Complex(_) => bail!("cannot convert a complex number to JSON"),
        Object::Some(inner) if lossy => value_to_json(inner, lossy, active),
        Object::Some(_) => bail!("cannot convert an option to JSON, unwrap it first"),
        Object::Iterator(i) if lossy => {
            let mut out = Vec::new();
            let mut iter = i.borrow_mut();
            while let Some(v) = iter.next() {
                out.push(value_to_json(&v, lossy, active)?);
            }
            Ok(JsonValue::Array(out))
        }
        Object::Iterator(_) => {
            bail!("cannot convert an iterator to JSON, collect it into a list first")
        }
        Object::MaxHeap(h) if lossy => {
            // Priority order: the order `pop` would produce
            let mut sorted = h.borrow().clone().into_sorted_vec();
            sorted.reverse();
            values_to_array(&mut sorted.iter().map(|v| &v.0), active)
        }
        Object::MinHeap(h) if lossy => {
            let mut sorted = h.borrow().clone().into_sorted_vec();
            sorted.reverse();
            values_to_array(&mut sorted.iter().map(|v| &v.0.0), active)
        }
        Object::MaxHeap(_) | Object::MinHeap(_) => {
            bail!("cannot convert a heap to JSON, convert it to a list first")
        }
        Object::Function(_) | Object::OverloadSet { .. } => {
            bail!("cannot convert a function to JSON")
        }
        Object::String(s) => Ok(json!(&*s.borrow())),
        Object::Tuple(v) if v.is_empty() => Ok(JsonValue::Null),
        Object::Tuple(v) if lossy => values_to_array(&mut v.iter(), active),
        Object::Tuple(_) => bail!("cannot convert a tuple to JSON, convert it to a list first"),
        Object::List(v) => values_to_array(&mut v.borrow().iter(), active),
        Object::Deque(d) if lossy => values_to_array(&mut d.borrow().iter(), active),
        Object::Deque(_) => bail!("cannot convert a deque to JSON, convert it to a list first"),
        Object::Map { entries, default } => {
            if default.is_some() && !lossy {
                bail!("cannot convert a map with a default value to JSON");
            }
            entries
                .borrow()
                .iter()
                .map(|(key, value)| {
                    let key = match key {
                        _ if lossy => key.to_string(),
                        Value::Object(obj) => match obj.as_ref() {
                            Object::String(key) => key.borrow().clone(),
                            _ => bail!("cannot convert a map with non-string key {key} to JSON"),
                        },
                        _ => bail!("cannot convert a map with non-string key {key} to JSON"),
                    };
                    let value = value_to_json(value, lossy, active)?;
                    Ok((key, value))
                })
                .collect::<Result<Map<String, JsonValue>, _>>()
                .map(JsonValue::Object)
        }
        Object::Struct { info, fields } if lossy => Ok(JsonValue::Object(
            info.fields
                .iter()
                .zip(fields.borrow().iter())
                .map(|((name, _), value)| {
                    value_to_json(value, lossy, active).map(|value| (name.clone(), value))
                })
                .collect::<Result<Map<String, JsonValue>, _>>()?,
        )),
        Object::Struct { .. } => bail!(
            "cannot convert a struct to JSON, use json_encode_lossy to convert it to a JSON object"
        ),
    }
}

fn json_to_value(value: JsonValue) -> Result<Value, anyhow::Error> {
    Ok(match value {
        JsonValue::Null => Value::unit(),
        JsonValue::Bool(b) => Value::Bool(b),
        JsonValue::Number(n) => {
            // With serde_json's arbitrary_precision feature the number's
            // original text is preserved exactly, so integers of any size can
            // be converted without going through a lossy f64.
            let repr = n.to_string();
            if repr.contains(['.', 'e', 'E']) {
                let float: f64 = repr.parse().context("cannot parse JSON number")?;
                if !float.is_finite() {
                    bail!("JSON number {repr} does not fit in a float");
                }
                Value::Float(float)
            } else if let Ok(int) = repr.parse::<i64>() {
                Value::Int(int)
            } else {
                Value::bigint(repr.parse().context("cannot parse JSON number")?)
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
    /// Converts a JSON string to a value: `json_decode("{\"a\": [1, null]}") == %{"a": [1, ()]}`.
    ///
    /// `null` becomes the unit value `()`, arrays become lists, objects become
    /// maps with string keys, and integers too big for `Int` decode losslessly
    /// to big integers.
    pub fn json_decode(input: &str) -> anyhow::Result<Value> {
        let json: JsonValue = serde_json::from_str(input)?;
        json_to_value(json)
    }

    /// Converts a value to a JSON string: `json_encode(%{"a": [1, ()]}) == "{\"a\":[1,null]}"`.
    ///
    /// Only values that decode back to an equal value are accepted, so this is
    /// the exact inverse of `json_decode`. The unit value `()` converts to
    /// `null`; sets convert to objects whose values are all `null`. Anything
    /// else is rejected with an error: rationals, complex numbers, non-finite
    /// floats, options (both `Some` and `None`), tuples, deques, iterators,
    /// heaps, structs, functions, maps with non-string keys or a default value,
    /// and values that contain themselves. Use `json_encode_lossy` to convert
    /// those anyway.
    pub fn json_encode(input: Value) -> anyhow::Result<String> {
        let v = value_to_json(&input, false, &mut HashSet::new())?;
        Ok(v.to_string())
    }

    /// Converts any value to a JSON string, accepting values `json_encode`
    /// rejects by degrading them: `json_encode_lossy((1, Some(1/2))) == "[1,0.5]"`.
    ///
    /// Rationals become floats, complex numbers become strings, `Some(x)` is
    /// unwrapped to `x`, tuples and deques become arrays, heaps become arrays
    /// in priority order, iterators are drained, structs become objects with
    /// their field names as keys, non-string map keys are stringified, and
    /// `None` and non-finite floats become `null`. There is no lossy
    /// counterpart for `json_decode` because these conversions cannot be
    /// reversed. Functions and values that contain themselves are still errors.
    pub fn json_encode_lossy(input: Value) -> anyhow::Result<String> {
        let v = value_to_json(&input, true, &mut HashSet::new())?;
        Ok(v.to_string())
    }
}
