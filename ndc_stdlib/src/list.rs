#[ndc_macros::export_module]
mod inner {
    use itertools::Itertools;
    use std::rc::Rc;

    use anyhow::anyhow;

    /// Converts any sequence into a list
    #[function(return_type = Vec<_>)]
    pub fn list(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        Ok(ndc_vm::value::Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("list requires a sequence"))?
                .collect::<Vec<_>>(),
        ))
    }

    pub fn contains(list: &[ndc_vm::value::Value], elem: ndc_vm::value::Value) -> bool {
        list.contains(&elem)
    }

    pub fn contains_subsequence(
        list: &[ndc_vm::value::Value],
        subsequence: &[ndc_vm::value::Value],
    ) -> bool {
        list.windows(subsequence.len()).contains(&subsequence)
    }

    pub fn find_subsequence(
        list: &[ndc_vm::value::Value],
        subsequence: &[ndc_vm::value::Value],
    ) -> ndc_vm::value::Value {
        let result = list
            .windows(subsequence.len())
            .enumerate()
            .find(|(_, seq)| *seq == subsequence)
            .map(|(idx, _)| idx);
        if let Some(result) = result {
            ndc_vm::value::Value::Int(result as i64)
        } else {
            ndc_vm::value::Value::unit()
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

    /// Removes the last element from a list and returns it, or `Unit` if it is empty
    #[function(name = "pop?", return_type = Option<_>)]
    pub fn maybe_pop(list: &mut Vec<ndc_vm::value::Value>) -> ndc_vm::value::Value {
        match list.pop() {
            None => ndc_vm::value::Value::None,
            Some(val) => ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::Some(val))),
        }
    }

    pub fn pop(list: &mut Vec<ndc_vm::value::Value>) -> ndc_vm::value::Value {
        list.pop().unwrap_or_else(ndc_vm::value::Value::unit)
    }

    #[function(name = "pop_left?", return_type = Option<_>)]
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
    #[function(return_type = Vec<_>)]
    pub fn reversed(list: &[ndc_vm::value::Value]) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(list.iter().rev().cloned().collect())
    }

    /// Removes all values from the list
    pub fn clear(list: &mut Vec<ndc_vm::value::Value>) {
        list.clear();
    }

    /// Returns `true` if the list contains no elements
    pub fn is_empty(list: &[ndc_vm::value::Value]) -> bool {
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
    pub fn first(list: &[ndc_vm::value::Value]) -> anyhow::Result<ndc_vm::value::Value> {
        list.first()
            .cloned()
            .ok_or_else(|| anyhow!("collection is empty"))
    }

    /// Returns a copy of the first element or `unit` if the list is empty.
    #[function(name = "first?", return_type = Option<_>)]
    pub fn maybe_first(list: &[ndc_vm::value::Value]) -> ndc_vm::value::Value {
        match list.first() {
            None => ndc_vm::value::Value::None,
            Some(v) => {
                ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::Some(v.clone())))
            }
        }
    }

    /// Returns a copy of the last element of the list or results in an error if the list is empty.
    pub fn last(list: &[ndc_vm::value::Value]) -> anyhow::Result<ndc_vm::value::Value> {
        list.last()
            .cloned()
            .ok_or_else(|| anyhow!("the list is empty"))
    }

    /// Returns a copy of the last element or `unit` if the list is empty.
    #[function(name = "last?", return_type = Option<_>)]
    pub fn maybe_last(list: &[ndc_vm::value::Value]) -> ndc_vm::value::Value {
        match list.last() {
            None => ndc_vm::value::Value::None,
            Some(v) => {
                ndc_vm::value::Value::Object(Rc::new(ndc_vm::value::Object::Some(v.clone())))
            }
        }
    }

    #[function(return_type = Vec<_>)]
    pub fn cartesian_product(
        list_a: &[ndc_vm::value::Value],
        list_b: &[ndc_vm::value::Value],
    ) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(
            list_a
                .iter()
                .cartesian_product(list_b)
                .map(|(a, b)| ndc_vm::value::Value::tuple(vec![a.clone(), b.clone()]))
                .collect_vec(),
        )
    }
}

pub mod ops {
    use ndc_core::{FunctionRegistry, StaticType};
    use ndc_vm::error::VmError;
    use ndc_vm::value::{
        NativeFunc, NativeFunction as VmNativeFunction, Object as VmObject, Value as VmValue,
    };
    use std::rc::Rc;

    pub fn register(env: &mut FunctionRegistry<Rc<VmNativeFunction>>) {
        register_list_concat(env);
        register_list_append(env);
    }

    fn register_list_concat(env: &mut FunctionRegistry<Rc<VmNativeFunction>>) {
        let native = Rc::new(VmNativeFunction {
            name: "++".to_string(),
            documentation: Some("Concatenates two lists into a new list.".to_string()),
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
        env.declare_global_fn(native);
    }

    fn register_list_append(env: &mut FunctionRegistry<Rc<VmNativeFunction>>) {
        let native = Rc::new(VmNativeFunction {
            name: "++=".to_string(),
            documentation: Some("Appends all elements from the right list to the left list in place.".to_string()),
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
        env.declare_global_fn(native);
    }
}
