use ndc_vm::value::{Object, SeqValue, Value};

#[ndc_macros::export_module]
mod inner {
    use itertools::Itertools;
    use std::rc::Rc;

    use anyhow::anyhow;

    /// Converts any sequence into a list
    #[function(return_type = Vec<_>)]
    pub fn list(seq: SeqValue) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("list requires a sequence"))?
                .collect::<Vec<_>>(),
        ))
    }

    /// Returns `true` if the list contains the given element.
    pub fn contains(list: &[Value], elem: Value) -> bool {
        list.contains(&elem)
    }

    /// Returns `true` if the list contains the given subsequence.
    pub fn contains_subsequence(list: &[Value], subsequence: &[Value]) -> bool {
        list.windows(subsequence.len()).contains(&subsequence)
    }

    /// Returns the starting index of the first occurrence of `subsequence` in `list`, or unit if not found.
    pub fn find_subsequence(list: &[Value], subsequence: &[Value]) -> Value {
        let result = list
            .windows(subsequence.len())
            .enumerate()
            .find(|(_, seq)| *seq == subsequence)
            .map(|(idx, _)| idx);
        if let Some(result) = result {
            Value::Int(result as i64)
        } else {
            Value::unit()
        }
    }

    /// Inserts an element at the given index, shifting all elements after it to the right.
    pub fn insert(list: &mut Vec<Value>, index: usize, elem: Value) -> anyhow::Result<()> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }
        list.insert(index, elem);
        Ok(())
    }

    /// Removes and returns the element at position `index` within the list, shifting all elements after it to the left.
    pub fn remove(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(list.remove(index))
    }

    /// Removes all instances of `element` from `list`
    pub fn remove_element(list: &mut Vec<Value>, element: Value) {
        list.retain(|cur| cur != &element);
    }

    /// Appends `elem` to the back of `list`
    pub fn push(list: &mut Vec<Value>, elem: Value) {
        list.push(elem);
    }

    /// Moves elements from `other` to `list`, leaving `other` empty.
    pub fn append(list: &mut Vec<Value>, other: &mut Vec<Value>) {
        list.append(other);
    }

    /// Copies elements from `other` to `list`, not touching `other`.
    pub fn extend(list: &mut Vec<Value>, other: &[Value]) {
        list.extend_from_slice(other);
    }

    /// Removes and returns the last element from the list, or `None` if empty.
    #[function(name = "pop?", return_type = Option<_>)]
    pub fn maybe_pop(list: &mut Vec<Value>) -> Value {
        match list.pop() {
            None => Value::None,
            Some(val) => Value::Object(Rc::new(Object::Some(val))),
        }
    }

    /// Removes and returns the last element from the list, or unit if empty.
    pub fn pop(list: &mut Vec<Value>) -> Value {
        list.pop().unwrap_or_else(Value::unit)
    }

    /// Removes and returns the first element from the list, or `None` if empty.
    #[function(name = "pop_left?", return_type = Option<_>)]
    pub fn maybe_pop_left(list: &mut Vec<Value>) -> Value {
        if list.is_empty() {
            return Value::None;
        }
        Value::Object(Rc::new(Object::Some(list.remove(0))))
    }

    /// Removes and returns the first element from the list, or unit if empty.
    pub fn pop_left(list: &mut Vec<Value>) -> Value {
        if list.is_empty() {
            return Value::unit();
        }
        list.remove(0)
    }

    /// Creates a copy of the list with its elements in reverse order
    #[function(return_type = Vec<_>)]
    pub fn reversed(list: &[Value]) -> Value {
        Value::list(list.iter().rev().cloned().collect())
    }

    /// Removes all values from the list
    pub fn clear(list: &mut Vec<Value>) {
        list.clear();
    }

    /// Returns `true` if the list contains no elements
    pub fn is_empty(list: &[Value]) -> bool {
        list.is_empty()
    }

    /// Splits the collection into two at the given index.
    pub fn split_off(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(Value::Object(Rc::new(Object::list(list.split_off(index)))))
    }

    /// Shortens the list, keeping the first `len` elements and dropping the rest.
    /// If `len` is greater or equal to the length of the list, nothing happens.
    pub fn truncate(list: &mut Vec<Value>, len: usize) {
        list.truncate(len);
    }

    /// Returns a copy of the first element of the list or results in an error if the list is empty.
    pub fn first(list: &[Value]) -> anyhow::Result<Value> {
        list.first()
            .cloned()
            .ok_or_else(|| anyhow!("collection is empty"))
    }

    /// Returns a copy of the first element, or `None` if the list is empty.
    #[function(name = "first?", return_type = Option<_>)]
    pub fn maybe_first(list: &[Value]) -> Value {
        match list.first() {
            None => Value::None,
            Some(v) => Value::Object(Rc::new(Object::Some(v.clone()))),
        }
    }

    /// Returns a copy of the last element of the list or results in an error if the list is empty.
    pub fn last(list: &[Value]) -> anyhow::Result<Value> {
        list.last()
            .cloned()
            .ok_or_else(|| anyhow!("the list is empty"))
    }

    /// Returns a copy of the last element, or `None` if the list is empty.
    #[function(name = "last?", return_type = Option<_>)]
    pub fn maybe_last(list: &[Value]) -> Value {
        match list.last() {
            None => Value::None,
            Some(v) => Value::Object(Rc::new(Object::Some(v.clone()))),
        }
    }

    /// Returns the Cartesian product of two lists as a list of tuples.
    #[function(return_type = Vec<_>)]
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
    use ndc_core::{FunctionRegistry, StaticType};
    use ndc_vm::error::VmError;
    use ndc_vm::value::{NativeFunc, NativeFunction, Object, Value};
    use std::rc::Rc;

    pub fn register(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
        register_list_concat(env);
        register_list_append(env);
    }

    fn register_list_concat(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
        let native = Rc::new(NativeFunction {
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
                let Value::Object(left_obj) = left else {
                    return Err(VmError::native(format!(
                        "++ left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let Value::Object(right_obj) = right else {
                    return Err(VmError::native(format!(
                        "++ right requires a list, got {}",
                        right.static_type()
                    )));
                };
                let Object::List(left_cell) = left_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++ left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let Object::List(right_cell) = right_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++ right requires a list, got {}",
                        right.static_type()
                    )));
                };

                if Rc::strong_count(left_obj) == 1 {
                    left_cell
                        .borrow_mut()
                        .extend_from_slice(&right_cell.borrow());
                    Ok(Value::Object(left_obj.clone()))
                } else {
                    let new_list: Vec<Value> = left_cell
                        .borrow()
                        .iter()
                        .chain(right_cell.borrow().iter())
                        .cloned()
                        .collect();
                    Ok(Value::Object(Rc::new(Object::list(new_list))))
                }
            })),
        });
        env.declare_global_fn(native);
    }

    fn register_list_append(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
        let native = Rc::new(NativeFunction {
            name: "++=".to_string(),
            documentation: Some(
                "Appends all elements from the right list to the left list in place.".to_string(),
            ),
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
                let Value::Object(left_obj) = left else {
                    return Err(VmError::native(format!(
                        "++= left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let Value::Object(right_obj) = right else {
                    return Err(VmError::native(format!(
                        "++= right requires a list, got {}",
                        right.static_type()
                    )));
                };
                let Object::List(left_cell) = left_obj.as_ref() else {
                    return Err(VmError::native(format!(
                        "++= left requires a list, got {}",
                        left.static_type()
                    )));
                };
                let Object::List(right_cell) = right_obj.as_ref() else {
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
