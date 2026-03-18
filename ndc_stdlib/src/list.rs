#[ndc_macros::export_module]
mod inner {
    use itertools::Itertools;
    use ndc_interpreter::value::Value;
    use std::rc::Rc;

    use anyhow::anyhow;

    /// Converts any sequence into a list
    pub fn list(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        Ok(ndc_vm::value::Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("list requires a sequence"))?
                .collect::<Vec<_>>(),
        ))
    }

    pub fn contains(list: &[Value], elem: &Value) -> bool {
        list.contains(elem)
    }

    pub fn contains_subsequence(list: &[Value], subsequence: &[Value]) -> bool {
        list.windows(subsequence.len()).contains(&subsequence)
    }

    pub fn find_subsequence(list: &[Value], subsequence: &[Value]) -> Value {
        let result = list
            .windows(subsequence.len())
            .enumerate()
            .find(|(_, seq)| *seq == subsequence)
            .map(|(idx, _)| idx);
        if let Some(result) = result {
            Value::from(result)
        } else {
            Value::unit()
        }
    }

    pub fn insert(
        list: &mut Vec<ndc_vm::value::Value>,
        index: usize,
        elem: ndc_vm::value::Value,
    ) -> anyhow::Result<()> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }
        list.insert(index, elem);
        Ok(())
    }

    /// Removes and returns the element at position `index` within the list, shifting all elements after it to the left.
    pub fn remove(
        list: &mut Vec<ndc_vm::value::Value>,
        index: usize,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(list.remove(index))
    }

    /// Removes all instances of `element` from `list`
    pub fn remove_element(list: &mut Vec<ndc_vm::value::Value>, element: ndc_vm::value::Value) {
        list.retain(|cur| cur != &element);
    }

    /// Appends `elem` to the back of `list`
    pub fn push(list: &mut Vec<ndc_vm::value::Value>, elem: ndc_vm::value::Value) {
        list.push(elem);
    }

    /// Moves elements from `other` to `list` leaving `other` empty
    pub fn append(list: &mut Vec<ndc_vm::value::Value>, other: &mut Vec<ndc_vm::value::Value>) {
        list.append(other);
    }

    /// Copies elements from `other` to `list` not touching `other`.
    pub fn extend(list: &mut Vec<ndc_vm::value::Value>, other: &[ndc_vm::value::Value]) {
        list.extend_from_slice(other);
    }

    /// Extends this `list` with elements from `iter` leaving the iterator empty
    #[function(name = "extend")]
    pub fn extend_from_iter(list: &mut Vec<Value>, iter: impl Iterator<Item = Value>) {
        list.extend(iter);
    }

    /// Removes the last element from a list and returns it, or `Unit` if it is empty
    #[function(name = "pop?", return_type = Option<Value>)]
    pub fn maybe_pop(list: &mut Vec<ndc_vm::value::Value>) -> ndc_vm::value::Value {
        match list.pop() {
            None => ndc_vm::value::Value::None,
            Some(val) => ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::Some(val))),
        }
    }

    pub fn pop(list: &mut Vec<ndc_vm::value::Value>) -> ndc_vm::value::Value {
        list.pop().unwrap_or_else(ndc_vm::value::Value::unit)
    }

    #[function(name = "pop_left?", return_type = Option<Value>)]
    pub fn maybe_pop_left(list: &mut Vec<ndc_vm::value::Value>) -> ndc_vm::value::Value {
        if list.is_empty() {
            return ndc_vm::value::Value::None;
        }
        ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::Some(list.remove(0))))
    }

    pub fn pop_left(list: &mut Vec<ndc_vm::value::Value>) -> ndc_vm::value::Value {
        if list.is_empty() {
            return ndc_vm::value::Value::unit();
        }
        list.remove(0)
    }

    /// Creates a copy of the list with its elements in reverse order
    #[function(return_type = Vec<Value>)]
    pub fn reversed(list: &[Value]) -> Value {
        Value::list(list.iter().rev().cloned().collect::<Vec<Value>>())
    }

    /// Removes all values from the list
    pub fn clear(list: &mut Vec<ndc_vm::value::Value>) {
        list.clear();
    }

    /// Returns `true` if the list contains no elements
    pub fn is_empty(list: &[Value]) -> bool {
        list.is_empty()
    }

    /// Splits the collection into two at the given index.
    pub fn split_off(
        list: &mut Vec<ndc_vm::value::Value>,
        index: usize,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(ndc_vm::value::Value::Object(Rc::new(
            ndc_vm::value::Object::list(list.split_off(index)),
        )))
    }

    /// Shortens the list, keeping the first `len` elements and dropping the rest.
    /// If `len` is greater or equal to the length of the list, nothing happens.
    pub fn truncate(list: &mut Vec<ndc_vm::value::Value>, len: usize) {
        list.truncate(len);
    }

    /// Returns a copy of the first element of the list or results in an error if the list is empty.
    pub fn first(list: &[Value]) -> anyhow::Result<Value> {
        list.first()
            .cloned()
            .ok_or_else(|| anyhow!("collection is empty"))
    }

    /// Returns a copy of the first element or `unit` if the list is empty.
    #[function(name = "first?")]
    pub fn maybe_first(list: &[Value]) -> Value {
        list.first().cloned().map_or_else(Value::none, Value::some)
    }

    /// Returns a copy of the last element of the list or results in an error if the list is empty.
    pub fn last(list: &[Value]) -> anyhow::Result<Value> {
        list.last()
            .cloned()
            .ok_or_else(|| anyhow!("the list is empty"))
    }

    /// Returns a copy of the last element or `unit` if the list is empty.
    #[function(name = "last?")]
    pub fn maybe_last(list: &[Value]) -> Value {
        list.last().cloned().map_or_else(Value::none, Value::some)
    }

    #[function(return_type = Vec<(Value, Value)>)]
    pub fn cartesian_product(list_a: &[Value], list_b: &[Value]) -> Value {
        Value::list(
            list_a
                .iter()
                .cartesian_product(list_b)
                .map(|(a, b)| Value::tuple(vec![a.clone(), b.clone()]))
                .collect_vec(),
        )
    }
}

pub mod ops {
    use ndc_interpreter::environment::Environment;
    use ndc_interpreter::function::{
        FunctionBody, FunctionBuilder, FunctionCarrier, Parameter, StaticType, TypeSignature,
    };
    use ndc_interpreter::sequence::Sequence;
    use ndc_interpreter::value::Value as InterpValue;
    use ndc_vm::error::VmError;
    use ndc_vm::value::{
        NativeFunc, NativeFunction as VmNativeFunction, Object as VmObject, Value as VmValue,
    };
    use std::cell::RefCell;
    use std::rc::Rc;

    pub fn register(env: &mut Environment) {
        register_list_concat(env);
        register_list_append(env);
    }

    // Interpreter path for `++`: works on ListRepr (Rc<RefCell<Vec<InterpValue>>>) directly.
    #[deprecated = "interpreter path — remove when tree-walk interpreter is dropped"]
    fn interp_list_concat(
        args: &mut [InterpValue],
        _env: &Rc<RefCell<Environment>>,
    ) -> Result<InterpValue, FunctionCarrier> {
        let [left, right] = args else {
            return Err(anyhow::anyhow!("++ requires 2 arguments").into());
        };
        let (left_rc, right_rc) = match (left, right) {
            (
                InterpValue::Sequence(Sequence::List(l)),
                InterpValue::Sequence(Sequence::List(r)),
            ) => (l.clone(), r.clone()),
            _ => return Err(anyhow::anyhow!("++ requires list arguments").into()),
        };
        if Rc::strong_count(&left_rc) == 1 {
            left_rc.borrow_mut().extend_from_slice(&right_rc.borrow());
            Ok(InterpValue::Sequence(Sequence::List(left_rc)))
        } else {
            Ok(InterpValue::list(
                left_rc
                    .borrow()
                    .iter()
                    .chain(right_rc.borrow().iter())
                    .cloned()
                    .collect::<Vec<InterpValue>>(),
            ))
        }
    }

    // Interpreter path for `++=`: works on ListRepr directly, handles self-alias correctly.
    #[deprecated = "interpreter path — remove when tree-walk interpreter is dropped"]
    fn interp_list_append(
        args: &mut [InterpValue],
        _env: &Rc<RefCell<Environment>>,
    ) -> Result<InterpValue, FunctionCarrier> {
        let [left, right] = args else {
            return Err(anyhow::anyhow!("++=  requires 2 arguments").into());
        };
        let (left_rc, right_rc) = match (left, right) {
            (
                InterpValue::Sequence(Sequence::List(l)),
                InterpValue::Sequence(Sequence::List(r)),
            ) => (l.clone(), r.clone()),
            _ => return Err(anyhow::anyhow!("++= requires list arguments").into()),
        };
        if Rc::ptr_eq(&left_rc, &right_rc) {
            left_rc.borrow_mut().extend_from_within(..);
        } else if Rc::strong_count(&right_rc) == 1 {
            left_rc.borrow_mut().append(
                &mut right_rc
                    .try_borrow_mut()
                    .expect("Failed to borrow_mut in `++=`"),
            );
        } else {
            left_rc.borrow_mut().extend_from_slice(&right_rc.borrow());
        }
        Ok(InterpValue::unit())
    }

    fn register_list_concat(env: &mut Environment) {
        let native = Rc::new(VmNativeFunction {
            name: "++".to_string(),
            static_type: StaticType::Function {
                parameters: Some(vec![
                    StaticType::List(Box::new(StaticType::Any)),
                    StaticType::List(Box::new(StaticType::Any)),
                ]),
                return_type: Box::new(StaticType::List(Box::new(StaticType::Any))),
            },
            func: NativeFunc::Simple(Box::new(|args| {
                let [left, right] = args else {
                    return Err(VmError::native(format!(
                        "++ requires exactly 2 arguments, got {}",
                        args.len()
                    )));
                };
                let VmValue::Object(left_obj) = left else {
                    return Err(VmError::native(format!(
                        "++ left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let VmValue::Object(right_obj) = right else {
                    return Err(VmError::native(format!(
                        "++ right requires a list, got {}",
                        right.static_type()
                    )));
                };
                let VmObject::List(left_cell) = left_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++ left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let VmObject::List(right_cell) = right_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++ right requires a list, got {}",
                        right.static_type()
                    )));
                };

                if Rc::strong_count(left_obj) == 1 {
                    left_cell
                        .borrow_mut()
                        .extend_from_slice(&right_cell.borrow());
                    Ok(VmValue::Object(left_obj.clone()))
                } else {
                    let new_list: Vec<VmValue> = left_cell
                        .borrow()
                        .iter()
                        .chain(right_cell.borrow().iter())
                        .cloned()
                        .collect();
                    Ok(VmValue::Object(Rc::new(VmObject::list(new_list))))
                }
            })),
        });
        env.declare_global_fn(
            FunctionBuilder::default()
                .name("++".to_string())
                .body(FunctionBody::GenericFunction {
                    function: interp_list_concat,
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::List(Box::new(StaticType::Any))),
                        Parameter::new("right", StaticType::List(Box::new(StaticType::Any))),
                    ]),
                    return_type: StaticType::List(Box::new(StaticType::Any)),
                })
                .vm_native(native)
                .build()
                .expect("must succeed"),
        );
    }

    fn register_list_append(env: &mut Environment) {
        let native = Rc::new(VmNativeFunction {
            name: "++=".to_string(),
            static_type: StaticType::Function {
                parameters: Some(vec![
                    StaticType::List(Box::new(StaticType::Any)),
                    StaticType::List(Box::new(StaticType::Any)),
                ]),
                return_type: Box::new(StaticType::Tuple(vec![])),
            },
            func: NativeFunc::Simple(Box::new(|args| {
                let [left, right] = args else {
                    return Err(VmError::native(format!(
                        "++= requires exactly 2 arguments, got {}",
                        args.len()
                    )));
                };
                let VmValue::Object(left_obj) = left else {
                    return Err(VmError::native(format!(
                        "++= left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let VmValue::Object(right_obj) = right else {
                    return Err(VmError::native(format!(
                        "++= right requires a list, got {}",
                        right.static_type()
                    )));
                };
                let VmObject::List(left_cell) = left_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++= left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let VmObject::List(right_cell) = right_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++= right requires a list, got {}",
                        right.static_type()
                    )));
                };

                if Rc::ptr_eq(left_obj, right_obj) {
                    left_cell.borrow_mut().extend_from_within(..);
                } else if Rc::strong_count(right_obj) == 1 {
                    left_cell.borrow_mut().append(
                        &mut right_cell
                            .try_borrow_mut()
                            .expect("Failed to borrow_mut right in `++=`"),
                    );
                } else {
                    left_cell
                        .borrow_mut()
                        .extend_from_slice(&right_cell.borrow());
                }
                Ok(left.clone())
            })),
        });
        env.declare_global_fn(
            FunctionBuilder::default()
                .name("++=".to_string())
                .body(FunctionBody::GenericFunction {
                    function: interp_list_append,
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::List(Box::new(StaticType::Any))),
                        Parameter::new("right", StaticType::List(Box::new(StaticType::Any))),
                    ]),
                    return_type: StaticType::List(Box::new(StaticType::Any)),
                })
                .vm_native(native)
                .build()
                .expect("must succeed"),
        );
    }
}
