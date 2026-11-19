use ndc_vm::value::{Object, Value};

#[ndc_macros::export_module]
mod inner {
    use itertools::Itertools;
    use std::rc::Rc;

    use anyhow::anyhow;
    use num::{BigInt, ToPrimitive};

    /// Converts any sequence into a list
    #[function(signature = "<T>(Iterable<T>) -> [T]")]
    pub fn list(seq: Value) -> anyhow::Result<Value> {
        Ok(Value::list(
            seq.try_into_iter()
                .ok_or_else(|| anyhow!("list requires a sequence"))?
                .collect::<Vec<_>>(),
        ))
    }

    /// Returns `true` if the list contains the given element.
    #[function(signature = "<T>([T], T) -> Bool")]
    pub fn contains(list: &[Value], elem: Value) -> bool {
        list.contains(&elem)
    }

    /// Returns `true` if the list contains the given subsequence. Every list contains the empty one.
    #[function(signature = "<T>([T], [T]) -> Bool")]
    pub fn contains_subsequence(list: &[Value], subsequence: &[Value]) -> bool {
        subsequence.is_empty() || list.windows(subsequence.len()).contains(&subsequence)
    }

    /// Returns the starting index of the first occurrence of `subsequence` in `list`, or produces an error if there is none.
    #[function(signature = "<T>([T], [T]) -> Int")]
    pub fn find_subsequence(list: &[Value], subsequence: &[Value]) -> anyhow::Result<Value> {
        subsequence_start(list, subsequence)
            .map(|idx| Value::Int(idx as i64))
            .ok_or_else(|| anyhow!("the list doesn't contain the subsequence"))
    }

    /// Returns the starting index of the first occurrence of `subsequence` in `list`, or `None` if there is none.
    #[function(name = "find_subsequence?", signature = "<T>([T], [T]) -> Option<Int>")]
    pub fn maybe_find_subsequence(list: &[Value], subsequence: &[Value]) -> Value {
        Value::option(subsequence_start(list, subsequence).map(|idx| Value::Int(idx as i64)))
    }

    /// Inserts an element at the given index, shifting all elements after it to the right.
    #[function(signature = "<T>([T], Int, T)")]
    pub fn insert(list: &mut Vec<Value>, index: usize, elem: Value) -> anyhow::Result<()> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }
        list.insert(index, elem);
        Ok(())
    }

    /// Removes and returns the element at position `index` within the list, shifting all elements after it to the left.
    #[function(signature = "<T>([T], Int) -> T")]
    pub fn remove(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index >= list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(list.remove(index))
    }

    /// Removes all instances of `element` from `list`
    #[function(signature = "<T>([T], T)")]
    pub fn remove_element(list: &mut Vec<Value>, element: Value) {
        list.retain(|cur| cur != &element);
    }

    /// Appends `elem` to the back of `list`
    #[function(signature = "<T>([T], T)")]
    pub fn push(list: &mut Vec<Value>, elem: Value) {
        list.push(elem);
    }

    /// Moves elements from `other` to `list`, leaving `other` empty.
    #[function(signature = "<T>([T], [T])")]
    pub fn append(list: &mut Vec<Value>, other: &mut Vec<Value>) {
        list.append(other);
    }

    /// Copies elements from `other` to `list`, not touching `other`.
    #[function(signature = "<T>([T], [T])")]
    pub fn extend(list: &mut Vec<Value>, other: &[Value]) {
        list.extend_from_slice(other);
    }

    /// Removes and returns the last element from the list, or `None` if empty.
    #[function(name = "pop?", signature = "<T>([T]) -> Option<T>")]
    pub fn maybe_pop(list: &mut Vec<Value>) -> Value {
        Value::option(list.pop())
    }

    /// Removes and returns the last element from the list. Use `pop?` to get
    /// `None` from an empty list instead of an error.
    #[function(signature = "<T>([T]) -> T")]
    pub fn pop(list: &mut Vec<Value>) -> anyhow::Result<Value> {
        list.pop()
            .ok_or_else(|| anyhow::anyhow!("the list is empty"))
    }

    /// Removes and returns the first element from the list, or `None` if empty.
    #[function(name = "pop_left?", signature = "<T>([T]) -> Option<T>")]
    pub fn maybe_pop_left(list: &mut Vec<Value>) -> Value {
        Value::option((!list.is_empty()).then(|| list.remove(0)))
    }

    /// Removes and returns the first element from the list. Use `pop_left?`
    /// to get `None` from an empty list instead of an error.
    #[function(signature = "<T>([T]) -> T")]
    pub fn pop_left(list: &mut Vec<Value>) -> anyhow::Result<Value> {
        if list.is_empty() {
            return Err(anyhow::anyhow!("the list is empty"));
        }
        Ok(list.remove(0))
    }

    /// Resolves a possibly-negative index against `len`, wrapping negative
    /// indices from the end (Python-style: `-1` is the last element). Returns
    /// `None` when the index is out of bounds in either direction.
    fn wrap_index(i: &BigInt, len: usize) -> Option<usize> {
        let i = i.to_i64()?; // too large for i64 => out of bounds
        if i >= 0 {
            let idx = usize::try_from(i).ok()?;
            (idx < len).then_some(idx)
        } else {
            // -1 => len - 1, -len => 0, anything more negative => out of bounds
            len.checked_sub(usize::try_from(i.unsigned_abs()).ok()?)
        }
    }

    /// Returns a copy of the element at `index`, or an error if the index is out
    /// of bounds. The index must be non-negative; use `index` for indexing from
    /// the end.
    #[function(signature = "<T>([T], Int) -> T")]
    pub fn get(list: &[Value], index: usize) -> anyhow::Result<Value> {
        list.get(index)
            .cloned()
            .ok_or_else(|| anyhow!("index {index} is out of bounds"))
    }

    /// Returns a copy of the element at `index`, or `None` if `index` is past
    /// the end of the list. A negative or oversized index is a conversion error,
    /// not `None`; use `index?` for indexing from the end.
    #[function(name = "get?", signature = "<T>([T], Int) -> Option<T>")]
    pub fn maybe_get(list: &[Value], index: usize) -> Value {
        Value::option(list.get(index).cloned())
    }

    /// Returns a copy of the element at `i`, wrapping negative indices from the
    /// end of the list; errors if the index is out of bounds. The named form of
    /// the `[]` operator.
    #[function(signature = "<T>([T], Int) -> T")]
    pub fn index(list: &[Value], i: &BigInt) -> anyhow::Result<Value> {
        wrap_index(i, list.len())
            .map(|idx| list[idx].clone())
            .ok_or_else(|| {
                let plural = if list.len() == 1 { "" } else { "s" };
                anyhow!(
                    "index {i} is out of bounds for a list of {} element{plural}",
                    list.len()
                )
            })
    }

    /// Returns a copy of the element at `i`, wrapping negative indices from the
    /// end of the list; returns `None` if the index is out of bounds. The
    /// non-throwing twin of the `[]` operator.
    #[function(name = "index?", signature = "<T>([T], Int) -> Option<T>")]
    pub fn maybe_index(list: &[Value], i: &BigInt) -> Value {
        Value::option(wrap_index(i, list.len()).map(|idx| list[idx].clone()))
    }

    /// Creates a copy of the list with its elements in reverse order
    #[function(signature = "<T>([T]) -> [T]")]
    pub fn reversed(list: &[Value]) -> Value {
        Value::list(list.iter().rev().cloned().collect())
    }

    /// Removes all values from the list
    #[function(signature = "<T>([T])")]
    pub fn clear(list: &mut Vec<Value>) {
        list.clear();
    }

    /// Returns `true` if the list contains no elements
    #[function(signature = "<T>([T]) -> Bool")]
    pub fn is_empty(list: &[Value]) -> bool {
        list.is_empty()
    }

    /// Splits the collection into two at the given index.
    #[function(signature = "<T>([T], Int) -> [T]")]
    pub fn split_off(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(Value::Object(Rc::new(Object::list(list.split_off(index)))))
    }

    /// Shortens the list, keeping the first `len` elements and dropping the rest.
    /// If `len` is greater or equal to the length of the list, nothing happens.
    #[function(signature = "<T>([T], Int)")]
    pub fn truncate(list: &mut Vec<Value>, len: usize) {
        list.truncate(len);
    }

    /// Returns a copy of the first element of the list or results in an error if the list is empty.
    #[function(signature = "<T>([T]) -> T")]
    pub fn first(list: &[Value]) -> anyhow::Result<Value> {
        list.first()
            .cloned()
            .ok_or_else(|| anyhow!("collection is empty"))
    }

    /// Returns a copy of the first element, or `None` if the list is empty.
    #[function(name = "first?", signature = "<T>([T]) -> Option<T>")]
    pub fn maybe_first(list: &[Value]) -> Value {
        Value::option(list.first().cloned())
    }

    /// Returns a copy of the last element of the list or results in an error if the list is empty.
    #[function(signature = "<T>([T]) -> T")]
    pub fn last(list: &[Value]) -> anyhow::Result<Value> {
        list.last()
            .cloned()
            .ok_or_else(|| anyhow!("the list is empty"))
    }

    /// Returns a copy of the last element, or `None` if the list is empty.
    #[function(name = "last?", signature = "<T>([T]) -> Option<T>")]
    pub fn maybe_last(list: &[Value]) -> Value {
        Value::option(list.last().cloned())
    }

    /// Returns the Cartesian product of two lists as a list of tuples.
    #[function(signature = "<T, U>([T], [U]) -> [(T, U)]")]
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
    use ndc_core::FunctionRegistry;
    use ndc_vm::NativeSpec;
    use ndc_vm::error::VmError;
    use ndc_vm::value::{NativeFunc, Object, Value};
    use std::cell::RefCell;
    use std::rc::Rc;

    pub fn register(env: &mut FunctionRegistry<NativeSpec>) {
        register_list_concat(env);
        register_list_append(env);
    }

    /// The list object and the elements of the `side` operand of `operator`.
    fn list_operand<'a>(
        operator: &str,
        side: &str,
        value: &'a Value,
    ) -> Result<(&'a Rc<Object>, &'a RefCell<Vec<Value>>), VmError> {
        if let Value::Object(object) = value
            && let Object::List(elements) = object.as_ref()
        {
            return Ok((object, elements));
        }
        Err(VmError::native(format!(
            "{operator} {side} requires a list, got {}",
            value.static_type()
        )))
    }

    fn register_list_concat(env: &mut FunctionRegistry<NativeSpec>) {
        let native = NativeSpec::with_signature(
            "++",
            "Concatenates two lists into a new list.",
            "<T>([T], [T]) -> [T]",
            move || {
                NativeFunc::Simple(Box::new(|args| {
                    let [left, right] = args else {
                        return Err(VmError::native(format!(
                            "++ requires exactly 2 arguments, got {}",
                            args.len()
                        )));
                    };
                    let (left_obj, left_cell) = list_operand("++", "left", left)?;
                    let (_, right_cell) = list_operand("++", "right", right)?;

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
                }))
            },
        );
        env.declare_global_fn(native);
    }

    fn register_list_append(env: &mut FunctionRegistry<NativeSpec>) {
        let native = NativeSpec::with_signature(
            "++=",
            "Appends all elements from the right list to the left list in place.",
            "<T>([T], [T]) -> [T]",
            move || {
                NativeFunc::Simple(Box::new(|args| {
                    let [left, right] = args else {
                        return Err(VmError::native(format!(
                            "++= requires exactly 2 arguments, got {}",
                            args.len()
                        )));
                    };
                    let (left_obj, left_cell) = list_operand("++=", "left", left)?;
                    let (right_obj, right_cell) = list_operand("++=", "right", right)?;

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
                }))
            },
        );
        env.declare_global_fn(native);
    }
}

/// Where `subsequence` first starts in `list`, if it's in it.
fn subsequence_start(list: &[Value], subsequence: &[Value]) -> Option<usize> {
    // An empty subsequence starts anywhere, and `windows` can't be empty.
    if subsequence.is_empty() {
        return Some(0);
    }
    list.windows(subsequence.len())
        .position(|window| window == subsequence)
}
