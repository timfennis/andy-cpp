use ndc_core::{FunctionRegistry, StaticType};
use ndc_vm::Vm;
use ndc_vm::error::VmError;
use ndc_vm::value::{NativeFunc, NativeFunction, Object as VmObject, Value as VmValue};
use std::rc::Rc;

pub fn register(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    let get_native = Rc::new(NativeFunction {
        name: "[]".to_string(),
        documentation: None, // TODO figure out how to get the docs in here
        static_type: StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::Any),
        },
        func: NativeFunc::WithVm(Box::new(|args: &[VmValue], vm: &mut Vm| {
            let [container, index_value] = args else {
                return Err(VmError::native(format!(
                    "[] requires exactly 2 arguments, got {}",
                    args.len()
                )));
            };
            vm_get_at_index(container, index_value, vm)
        })),
    });
    env.declare_global_fn(get_native);

    let set_native = Rc::new(NativeFunction {
        name: "[]=".to_string(),
        documentation: None, // TODO figure out how to get the docs in here
        static_type: StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::Any),
        },
        func: NativeFunc::Simple(Box::new(|args: &[VmValue]| {
            let [container, index_value, rhs] = args else {
                return Err(VmError::native(format!(
                    "[]= requires exactly 3 arguments, got {}",
                    args.len()
                )));
            };
            vm_set_at_index(container, index_value, rhs.clone())?;
            Ok(VmValue::unit())
        })),
    });
    env.declare_global_fn(set_native);
}

fn vm_sequence_length(v: &VmValue) -> Option<usize> {
    match v {
        VmValue::Object(obj) => match obj.as_ref() {
            VmObject::String(s) => Some(s.borrow().chars().count()),
            VmObject::List(l) => Some(l.borrow().len()),
            VmObject::Tuple(t) => Some(t.len()),
            VmObject::Map { entries, .. } => Some(entries.borrow().len()),
            VmObject::Deque(d) => Some(d.borrow().len()),
            _ => None,
        },
        _ => None,
    }
}

fn to_forward_index(i: i64, size: usize, for_slice: bool) -> Result<usize, VmError> {
    if i < 0 {
        let abs = i.unsigned_abs() as usize;
        if for_slice {
            Ok(size.saturating_sub(abs))
        } else {
            size.checked_sub(abs)
                .ok_or_else(|| VmError::native("index out of bounds"))
        }
    } else {
        let idx = i as usize;
        if for_slice {
            Ok(idx.min(size))
        } else if idx >= size {
            Err(VmError::native("index out of bounds"))
        } else {
            Ok(idx)
        }
    }
}

enum VmOffset {
    Element(usize),
    Range(usize, usize),
}

fn extract_vm_offset(index_value: &VmValue, size: usize) -> Result<VmOffset, VmError> {
    if let VmValue::Object(obj) = index_value {
        if let VmObject::Iterator(iter) = obj.as_ref() {
            let iter_ref = iter.borrow();
            if let Some((start, end, inclusive)) = iter_ref.range_bounds() {
                let from_idx = to_forward_index(start, size, true)?;
                let to_idx = to_forward_index(end, size, true)?;
                let to_idx = if inclusive {
                    (to_idx + 1).min(size)
                } else {
                    to_idx
                };
                return Ok(VmOffset::Range(from_idx, to_idx));
            }
            if let Some(start) = iter_ref.unbounded_range_start() {
                let from_idx = to_forward_index(start, size, true)?;
                return Ok(VmOffset::Range(from_idx, size));
            }
            return Err(VmError::native("cannot use non-range iterator as index"));
        }
    }
    let i = match index_value {
        VmValue::Int(i) => *i,
        VmValue::Object(obj) => match obj.as_ref() {
            VmObject::BigInt(n) => num::ToPrimitive::to_i64(n)
                .ok_or_else(|| VmError::native("index too large for i64"))?,
            _ => {
                return Err(VmError::native(
                    "Invalid list index. List indices must be convertible to a signed 64-bit integer.",
                ));
            }
        },
        _ => {
            return Err(VmError::native(
                "Invalid list index. List indices must be convertible to a signed 64-bit integer.",
            ));
        }
    };
    Ok(VmOffset::Element(to_forward_index(i, size, false)?))
}

fn vm_get_at_index(
    container: &VmValue,
    index_value: &VmValue,
    vm: &mut Vm,
) -> Result<VmValue, VmError> {
    let Some(size) = vm_sequence_length(container) else {
        return Err(VmError::native(format!(
            "cannot index into {}",
            container.static_type()
        )));
    };
    match container {
        VmValue::Object(obj) => match obj.as_ref() {
            VmObject::List(list) => {
                let list = list.borrow();
                match extract_vm_offset(index_value, size)? {
                    VmOffset::Element(idx) => Ok(list[idx].clone()),
                    VmOffset::Range(from, to) => {
                        let values = list.get(from..to).ok_or_else(|| {
                            VmError::native(format!("{from}..{to} out of bounds"))
                        })?;
                        Ok(VmValue::Object(Rc::new(VmObject::list(values.to_vec()))))
                    }
                }
            }
            VmObject::String(s) => {
                let s = s.borrow();
                match extract_vm_offset(index_value, size)? {
                    VmOffset::Element(idx) => {
                        let ch = s.chars().nth(idx).expect("bounds already checked");
                        Ok(VmValue::string(ch.to_string()))
                    }
                    VmOffset::Range(from, to) => {
                        let result: String = s.chars().skip(from).take(to - from).collect();
                        Ok(VmValue::string(result))
                    }
                }
            }
            VmObject::Map { entries, default } => {
                if matches!(
                    index_value,
                    VmValue::Object(o) if matches!(o.as_ref(), VmObject::Iterator(_))
                ) {
                    return Err(VmError::native(
                        "cannot use range expression as index in map",
                    ));
                }
                let key = index_value.clone();
                let value = entries.borrow().get(&key).cloned();
                if let Some(v) = value {
                    return Ok(v);
                }
                match default {
                    None => Err(VmError::native(format!("Key not found in map: {key}"))),
                    Some(default_val) => match default_val {
                        VmValue::Object(o) if matches!(o.as_ref(), VmObject::Function(_)) => {
                            let VmObject::Function(f) = o.as_ref() else {
                                unreachable!()
                            };
                            let result = vm.call_callback(f.clone(), vec![])?;
                            entries.borrow_mut().insert(key, result.clone());
                            Ok(result)
                        }
                        non_fn => {
                            let v = non_fn.clone();
                            entries.borrow_mut().insert(key, v.clone());
                            Ok(v)
                        }
                    },
                }
            }
            VmObject::Tuple(tuple) => match extract_vm_offset(index_value, size)? {
                VmOffset::Element(idx) => tuple
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| VmError::native("index out of bounds")),
                VmOffset::Range(from, to) => {
                    let values = tuple
                        .get(from..to)
                        .ok_or_else(|| VmError::native("index out of bounds"))?;
                    Ok(VmValue::Object(Rc::new(VmObject::Tuple(values.to_vec()))))
                }
            },
            VmObject::Deque(deque) => {
                let deque = deque.borrow();
                match extract_vm_offset(index_value, size)? {
                    VmOffset::Element(idx) => deque
                        .get(idx)
                        .cloned()
                        .ok_or_else(|| VmError::native("index out of bounds")),
                    VmOffset::Range(from, to) => {
                        let out: Vec<VmValue> =
                            deque.iter().skip(from).take(to - from).cloned().collect();
                        Ok(VmValue::Object(Rc::new(VmObject::list(out))))
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

fn vm_set_at_index(
    container: &VmValue,
    index_value: &VmValue,
    rhs: VmValue,
) -> Result<(), VmError> {
    let Some(size) = vm_sequence_length(container) else {
        return Err(VmError::native(format!(
            "cannot insert into {} at index",
            container.static_type()
        )));
    };
    match container {
        VmValue::Object(obj) => match obj.as_ref() {
            VmObject::List(list) => {
                let mut list = list.try_borrow_mut().map_err(|_| {
                    VmError::native("Mutation error: you cannot mutate a value in a list while you're iterating over this list")
                })?;
                match extract_vm_offset(index_value, size)? {
                    VmOffset::Element(idx) => {
                        list[idx] = rhs;
                    }
                    VmOffset::Range(from, to) => {
                        let rhs_vec = match rhs {
                            VmValue::Object(o) => match o.as_ref() {
                                VmObject::List(l) => l.borrow().clone(),
                                VmObject::Tuple(t) => t.clone(),
                                _ => {
                                    return Err(VmError::native(
                                        "cannot assign non-list to list slice",
                                    ));
                                }
                            },
                            _ => {
                                return Err(VmError::native(
                                    "cannot assign non-list to list slice",
                                ));
                            }
                        };
                        let tail: Vec<VmValue> = list.drain(from..).collect();
                        list.extend(rhs_vec);
                        list.extend_from_slice(&tail[(to - from)..]);
                    }
                }
            }
            VmObject::String(s) => {
                let rhs_str = match &rhs {
                    VmValue::Object(o) => match o.as_ref() {
                        VmObject::String(r) => r.borrow().clone(),
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
                match extract_vm_offset(index_value, size)? {
                    VmOffset::Element(idx) => {
                        s.replace_range(idx..=idx, &rhs_str);
                    }
                    VmOffset::Range(from, to) => {
                        s.replace_range(from..to, &rhs_str);
                    }
                }
            }
            VmObject::Map { entries, .. } => {
                if matches!(
                    index_value,
                    VmValue::Object(o) if matches!(o.as_ref(), VmObject::Iterator(_))
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
