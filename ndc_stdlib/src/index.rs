use ndc_core::{ANY_INDEX, FunctionRegistry, StaticType};
use ndc_vm::Vm;
use ndc_vm::error::VmError;
use ndc_vm::value::{NativeFunc, Object, Value};
use ndc_vm::{NativeOp, NativeSpec};
use std::rc::Rc;

pub fn register(env: &mut FunctionRegistry<NativeSpec>) {
    register_get(env);
    register_set(env);
}

fn make_get_func() -> NativeFunc {
    NativeFunc::WithVm(Box::new(|args: &[Value], vm: &mut Vm| {
        let [container, index_value] = args else {
            return Err(VmError::native(format!(
                "[] requires exactly 2 arguments, got {}",
                args.len()
            )));
        };
        vm_get_at_index(container, index_value, vm)
    }))
}

/// Allocation-free `[]` for overloads whose parameter type guarantees the
/// container can never trigger the Map default-function callback path
/// (a slice of a list, a string, a deque). Using `NativeFunc::Simple` here
/// lets the VM pass the argument registers by slice instead of moving them
/// into a `Vec<Value>` on every call.
fn make_get_func_simple() -> NativeFunc {
    NativeFunc::Simple(Box::new(|args: &[Value]| {
        let [container, index_value] = args else {
            return Err(VmError::native(format!(
                "[] requires exactly 2 arguments, got {}",
                args.len()
            )));
        };
        vm_get_at_index_simple(container, index_value)
    }))
}

fn make_set_func() -> NativeFunc {
    NativeFunc::Simple(Box::new(|args: &[Value]| {
        let [container, index_value, rhs] = args else {
            return Err(VmError::native(format!(
                "[]= requires exactly 3 arguments, got {}",
                args.len()
            )));
        };
        vm_set_at_index(container, index_value, rhs.clone())?;
        Ok(Value::unit())
    }))
}

fn register_get(env: &mut FunctionRegistry<NativeSpec>) {
    let doc = "Retrieves an element by index. Supports negative indices and range slicing.";

    // An element by its index, or a slice by a range of indices. A range of a
    // deque is a list.
    // An element of a list, which the VM reads with its own instruction.
    env.declare_global_fn(NativeSpec::operator_with_signature(
        "[]",
        doc,
        "<T>([T], Int) -> T",
        NativeOp::ListIndex,
    ));

    for signature in [
        "<T>([T], Iterator<Int>) -> [T]",
        "(String, Int) -> String",
        "(String, Iterator<Int>) -> String",
        "<T>(Deque<T>, Int) -> T",
        "<T>(Deque<T>, Iterator<Int>) -> [T]",
    ] {
        env.declare_global_fn(NativeSpec::with_signature(
            "[]",
            doc,
            signature,
            make_get_func_simple,
        ));
    }

    // Indexing a tuple or an `Any`: see `ANY_INDEX`. A map can come here as an
    // `Any`, and its default function takes the VM.
    env.declare_global_fn(NativeSpec::new(
        ANY_INDEX,
        doc,
        || StaticType::Function {
            parameters: Some(vec![StaticType::Any, StaticType::Any]),
            return_type: Box::new(StaticType::Any),
        },
        make_get_func,
    ));

    env.declare_global_fn(NativeSpec::with_signature(
        "[]",
        doc,
        "<K, V>(%{K: V}, K) -> V",
        make_get_func,
    ));
}

fn register_set(env: &mut FunctionRegistry<NativeSpec>) {
    let doc = "Sets an element by index. Supports negative indices and range slicing for lists and strings.";

    // An element of a list, which the VM writes with its own instruction.
    env.declare_global_fn(NativeSpec::operator_with_signature(
        "[]=",
        doc,
        "<T>([T], Int, T)",
        NativeOp::ListSet,
    ));

    // A slice by a range of indices.
    env.declare_global_fn(NativeSpec::with_signature(
        "[]=",
        doc,
        "<T>([T], Iterator<Int>, Iterable<T>)",
        make_set_func,
    ));

    for signature in ["(String, Int, String)", "(String, Iterator<Int>, String)"] {
        env.declare_global_fn(NativeSpec::with_signature(
            "[]=",
            doc,
            signature,
            make_set_func,
        ));
    }

    env.declare_global_fn(NativeSpec::with_signature(
        "[]=",
        doc,
        "<K, V>(%{K: V}, K, V)",
        make_set_func,
    ));
}

fn vm_sequence_length(v: &Value) -> Option<usize> {
    match v {
        Value::Object(obj) => match obj.as_ref() {
            Object::String(s) => Some(s.char_count()),
            Object::List(l) => Some(l.borrow().len()),
            Object::Tuple(t) => Some(t.len()),
            Object::Map { entries, .. } => Some(entries.borrow().len()),
            Object::Deque(d) => Some(d.borrow().len()),
            _ => None,
        },
        _ => None,
    }
}

/// The position of element `i` in a sequence of `size`, counting a negative
/// `i` from the end; `None` when there's no element there.
fn element_index(i: i64, size: usize) -> Option<usize> {
    if i < 0 {
        size.checked_sub(i.unsigned_abs() as usize)
    } else {
        let idx = i as usize;
        (idx < size).then_some(idx)
    }
}

/// A slice endpoint `i` in a sequence of `size`, counting a negative `i` from
/// the end and clamped into `0..=size`.
fn slice_index(i: i64, size: usize) -> usize {
    if i < 0 {
        size.saturating_sub(i.unsigned_abs() as usize)
    } else {
        (i as usize).min(size)
    }
}

/// Resolves a (possibly negative) slice endpoint to a forward position without
/// clamping it into `0..=size`. Used purely to detect reversed ranges: clamping
/// would collapse out-of-range endpoints to the boundary (e.g. `6` and `5` both
/// becoming `size` on a 5-element string), masking a reversal like `6..=5`.
fn unclamped_slice_index(i: i64, size: usize) -> i64 {
    if i < 0 { size as i64 + i } else { i }
}

enum VmOffset {
    Element(usize),
    Range(usize, usize),
}

fn char_index_to_byte_offset(string: &str, index: usize) -> usize {
    string
        .char_indices()
        .nth(index)
        .map_or(string.len(), |(byte_offset, _)| byte_offset)
}

/// Where `index_value` points in `container`, which holds `size` elements.
fn extract_vm_offset(
    index_value: &Value,
    container: &Value,
    size: usize,
) -> Result<VmOffset, VmError> {
    if let Value::Object(obj) = index_value
        && let Object::Iterator(iter) = obj.as_ref()
    {
        let iter_ref = iter.borrow();
        if let Some((start, end, inclusive)) = iter_ref.range_bounds() {
            // Detect reversal from the unclamped endpoints, so an out-of-range
            // reversed range can't collapse to equality once clamped (e.g.
            // `6..=5` on a 5-element string would otherwise clamp both ends to
            // 5 and silently yield an empty slice instead of an error).
            let from_unclamped = unclamped_slice_index(start, size);
            let end_unclamped = unclamped_slice_index(end, size);
            if end_unclamped < from_unclamped {
                return Err(VmError::native(format!(
                    "{from_unclamped}..{end_unclamped} out of bounds"
                )));
            }
            let from_idx = slice_index(start, size);
            let to_idx = slice_index(end, size);
            let to_idx = if inclusive {
                (to_idx + 1).min(size)
            } else {
                to_idx
            };
            return Ok(VmOffset::Range(from_idx, to_idx));
        }
        if let Some(start) = iter_ref.unbounded_range_start() {
            let from_idx = slice_index(start, size);
            return Ok(VmOffset::Range(from_idx, size));
        }
        return Err(VmError::native("cannot use non-range iterator as index"));
    }
    let i = match index_value {
        Value::Int(i) => *i,
        _ => {
            return Err(VmError::native(
                "Invalid list index. List indices must be Int values.",
            ));
        }
    };
    let idx = element_index(i, size).ok_or_else(|| out_of_bounds(i, container, size))?;
    Ok(VmOffset::Element(idx))
}

/// The error for indexing `container`, which holds `size` elements, at `i`:
/// "index 5 is out of bounds for a list of 3 elements".
fn out_of_bounds(i: i64, container: &Value, size: usize) -> VmError {
    let (kind, unit) = match container {
        Value::Object(obj) => match obj.as_ref() {
            Object::String(_) => ("string", "character"),
            Object::Tuple(_) => ("tuple", "element"),
            Object::Deque(_) => ("deque", "element"),
            _ => ("list", "element"),
        },
        _ => ("list", "element"),
    };
    let plural = if size == 1 { "" } else { "s" };
    VmError::native(format!(
        "index {i} is out of bounds for a {kind} of {size} {unit}{plural}"
    ))
}

/// `[]` on any container, a map's default function included, which takes
/// the VM to call.
fn vm_get_at_index(container: &Value, index_value: &Value, vm: &mut Vm) -> Result<Value, VmError> {
    let Value::Object(obj) = container else {
        return vm_get_at_index_simple(container, index_value);
    };
    let Object::Map { entries, default } = obj.as_ref() else {
        return vm_get_at_index_simple(container, index_value);
    };
    if matches!(
        index_value,
        Value::Object(o) if matches!(o.as_ref(), Object::Iterator(_))
    ) {
        return Err(VmError::native(
            "cannot use range expression as index in map",
        ));
    }
    if let Some(value) = entries.borrow().get(index_value) {
        return Ok(value.clone());
    }
    let value = match default {
        None => {
            return Err(VmError::native(format!(
                "Key not found in map: {index_value}"
            )));
        }
        Some(Value::Object(o)) if let Object::Function(f) = o.as_ref() => {
            vm.call_callback(f.clone(), &[])?
        }
        Some(value) => value.clone(),
    };
    entries
        .borrow_mut()
        .insert(index_value.clone(), value.clone());
    Ok(value)
}

/// `[]` on a container other than a map, which needs no VM.
fn vm_get_at_index_simple(container: &Value, index_value: &Value) -> Result<Value, VmError> {
    let Some(size) = vm_sequence_length(container) else {
        return Err(VmError::native(format!(
            "cannot index into {}",
            container.static_type()
        )));
    };
    match container {
        Value::Object(obj) => match obj.as_ref() {
            Object::List(list) => {
                let list = list.borrow();
                match extract_vm_offset(index_value, container, size)? {
                    VmOffset::Element(idx) => Ok(list[idx].clone()),
                    VmOffset::Range(from, to) => {
                        let values = list.get(from..to).ok_or_else(|| {
                            VmError::native(format!("{from}..{to} out of bounds"))
                        })?;
                        Ok(Value::Object(Rc::new(Object::list(values.to_vec()))))
                    }
                }
            }
            Object::String(s) => {
                let s = s.indexed();
                match extract_vm_offset(index_value, container, size)? {
                    VmOffset::Element(idx) => {
                        let ch = s.get_char(idx).expect("bounds already checked");
                        Ok(Value::string(ch.to_string()))
                    }
                    VmOffset::Range(from, to) => {
                        let from = s.get_index(from).unwrap_or(s.len());
                        let to = s.get_index(to).unwrap_or(s.len());
                        Ok(Value::string(&s[from..to]))
                    }
                }
            }
            Object::Tuple(tuple) => match extract_vm_offset(index_value, container, size)? {
                VmOffset::Element(idx) => tuple
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| VmError::native("index out of bounds")),
                VmOffset::Range(from, to) => {
                    let values = tuple
                        .get(from..to)
                        .ok_or_else(|| VmError::native("index out of bounds"))?;
                    Ok(Value::Object(Rc::new(Object::Tuple(values.to_vec()))))
                }
            },
            Object::Deque(deque) => {
                let deque = deque.borrow();
                match extract_vm_offset(index_value, container, size)? {
                    VmOffset::Element(idx) => deque
                        .get(idx)
                        .cloned()
                        .ok_or_else(|| VmError::native("index out of bounds")),
                    VmOffset::Range(from, to) => {
                        let out: Vec<Value> =
                            deque.iter().skip(from).take(to - from).cloned().collect();
                        Ok(Value::Object(Rc::new(Object::list(out))))
                    }
                }
            }
            _ => Err(VmError::native(format!(
                "cannot index into {}",
                container.static_type()
            ))),
        },
        _ => Err(VmError::native(format!(
            "cannot index into {}",
            container.static_type()
        ))),
    }
}

fn vm_set_at_index(container: &Value, index_value: &Value, rhs: Value) -> Result<(), VmError> {
    let Some(size) = vm_sequence_length(container) else {
        return Err(VmError::native(format!(
            "cannot insert into {} at index",
            container.static_type()
        )));
    };
    match container {
        Value::Object(obj) => match obj.as_ref() {
            Object::List(list) => {
                let mut list = list.try_borrow_mut().map_err(|_borrow_err| {
                    VmError::native("Mutation error: you cannot mutate a value in a list while you're iterating over this list")
                })?;
                match extract_vm_offset(index_value, container, size)? {
                    VmOffset::Element(idx) => {
                        list[idx] = rhs;
                    }
                    VmOffset::Range(from, to) => {
                        // Any sequence goes in a slice: a list, a tuple, a
                        // range or another iterator.
                        let rhs_vec = match &rhs {
                            Value::Object(o) if matches!(o.as_ref(), Object::List(_)) => {
                                let Object::List(l) = o.as_ref() else {
                                    unreachable!("matched a list");
                                };
                                l.borrow().clone()
                            }
                            _ => rhs
                                .clone()
                                .try_into_iter()
                                .ok_or_else(|| {
                                    VmError::native("cannot assign non-sequence to list slice")
                                })?
                                .collect(),
                        };
                        let tail: Vec<Value> = list.drain(from..).collect();
                        list.extend(rhs_vec);
                        list.extend_from_slice(&tail[(to - from)..]);
                    }
                }
            }
            Object::String(s) => {
                let rhs_str = match &rhs {
                    Value::Object(o) => match o.as_ref() {
                        Object::String(r) => r.borrow().clone(),
                        _ => {
                            return Err(VmError::native(format!(
                                "cannot insert {} into a string",
                                rhs.static_type()
                            )));
                        }
                    },
                    _ => {
                        return Err(VmError::native(format!(
                            "cannot insert {} into a string",
                            rhs.static_type()
                        )));
                    }
                };
                let mut s = s.borrow_mut();
                match extract_vm_offset(index_value, container, size)? {
                    VmOffset::Element(idx) => {
                        let from = char_index_to_byte_offset(&s, idx);
                        let to = char_index_to_byte_offset(&s, idx + 1);
                        s.replace_range(from..to, &rhs_str);
                    }
                    VmOffset::Range(from, to) => {
                        let from = char_index_to_byte_offset(&s, from);
                        let to = char_index_to_byte_offset(&s, to);
                        s.replace_range(from..to, &rhs_str);
                    }
                }
            }
            Object::Map { entries, .. } => {
                if matches!(
                    index_value,
                    Value::Object(o) if matches!(o.as_ref(), Object::Iterator(_))
                ) {
                    return Err(VmError::native("cannot use range expression as index"));
                }
                entries.borrow_mut().insert(index_value.clone(), rhs);
            }
            _ => {
                return Err(VmError::native(format!(
                    "cannot insert into {} at index",
                    container.static_type()
                )));
            }
        },
        _ => {
            return Err(VmError::native(format!(
                "cannot insert into {} at index",
                container.static_type()
            )));
        }
    }
    Ok(())
}
