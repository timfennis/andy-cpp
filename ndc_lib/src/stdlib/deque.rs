use ndc_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;

    /// Creates a new `Deque` type.
    ///
    /// The `Deque` type allows the user to quickly append and remove elements from both the start and the end of the list.
    #[function(name = "Deque")]
    pub fn create_deque() -> Value {
        Value::Sequence(Sequence::Deque(Rc::new(RefCell::new(VecDeque::new()))))
    }

    /// Pushes the `value` to the front of the `deque`.
    pub fn push_front(deque: &mut VecDeque<Value>, value: Value) {
        deque.push_front(value);
    }

    /// Appends the `value` to the end of the `deque`.
    pub fn push_back(deque: &mut VecDeque<Value>, value: Value) {
        deque.push_back(value);
    }

    /// Removes and returns the first element of the `deque`.
    pub fn pop_front(deque: &mut VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .pop_front()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    /// Removes and returns the first element of the `deque` as an `Option` returning `None` if the queue is empty.
    #[function(name = "pop_front?")]
    pub fn maybe_pop_front(deque: &mut VecDeque<Value>) -> Value {
        deque.pop_front().map_or_else(Value::none, Value::some)
    }

    /// Removes and returns the last element of the `deque`.
    pub fn pop_back(deque: &mut VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .pop_back()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    /// Removes and returns the last element of the `deque` as an `Option` returning `None` if the queue is empty.
    #[function(name = "pop_back?")]
    pub fn maybe_pop_back(deque: &mut VecDeque<Value>) -> Value {
        deque.pop_back().map_or_else(Value::none, Value::some)
    }

    /// Returns (but does not remove) the first element of the `deque`.
    pub fn front(deque: &VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .front()
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    /// Returns (but does not remove) the first element of the `deque` as an `Option` returning `None` if the queue is empty.
    #[function(name = "front?")]
    pub fn maybe_front(deque: &VecDeque<Value>) -> Value {
        deque.front().cloned().map_or_else(Value::none, Value::some)
    }

    /// Returns (but does not remove) the last element of the `deque`.
    pub fn back(deque: &VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .back()
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    /// Returns (but does not remove) the last element of the `deque` as an `Option` returning `None` if the queue is empty.
    #[function(name = "back?")]
    pub fn maybe_back(deque: &VecDeque<Value>) -> Value {
        deque.back().cloned().map_or_else(Value::none, Value::some)
    }

    /// Returns `true` if the `deque` contains the `value` or `false` otherwise.
    pub fn contains(deque: &VecDeque<Value>, value: &Value) -> bool {
        deque.contains(value)
    }

    /// Returns `true` if the `deque` is empty and `false` otherwise.
    pub fn is_empty(deque: &VecDeque<Value>) -> bool {
        deque.is_empty()
    }

    /// Removes all elements from the `deque`
    pub fn clear(deque: &mut VecDeque<Value>) -> Value {
        deque.clear();
        Value::unit()
    }
}
