use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;

    #[function(name = "Deque")]
    pub fn create_deque() -> Value {
        Value::Sequence(Sequence::Deque(Rc::new(RefCell::new(VecDeque::new()))))
    }

    pub fn push_front(deque: &mut VecDeque<Value>, value: Value) {
        deque.push_front(value);
    }

    pub fn push_back(deque: &mut VecDeque<Value>, value: Value) {
        deque.push_back(value);
    }

    pub fn pop_front(deque: &mut VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .pop_front()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    #[function(name = "pop_front?")]
    pub fn maybe_pop_front(deque: &mut VecDeque<Value>) -> Value {
        deque.pop_front().map_or_else(Value::none, Value::some)
    }

    pub fn pop_back(deque: &mut VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .pop_back()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    #[function(name = "pop_back?")]
    pub fn maybe_pop_back(deque: &mut VecDeque<Value>) -> Value {
        deque.pop_back().map_or_else(Value::none, Value::some)
    }

    pub fn front(deque: &VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .front()
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    #[function(name = "front?")]
    pub fn maybe_front(deque: &VecDeque<Value>) -> Value {
        deque.front().cloned().map_or_else(Value::none, Value::some)
    }

    pub fn back(deque: &VecDeque<Value>) -> anyhow::Result<Value> {
        deque
            .back()
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("the queue is empty"))
    }

    #[function(name = "back?")]
    pub fn maybe_back(deque: &VecDeque<Value>) -> Value {
        deque.back().cloned().map_or_else(Value::none, Value::some)
    }
}
