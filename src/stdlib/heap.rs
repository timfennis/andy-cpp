use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::heap::{MaxHeap, MinHeap};
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[function(name = "MinHeap")]
    pub fn create_min_heap() -> Value {
        Value::Sequence(Sequence::MinHeap(Rc::new(RefCell::new(MinHeap::new()))))
    }

    #[function(name = "MaxHeap")]
    pub fn create_max_heap() -> Value {
        Value::Sequence(Sequence::MaxHeap(Rc::new(RefCell::new(MaxHeap::new()))))
    }

    #[function(name = "pop?")]
    pub fn maybe_min_pop(heap: &mut MinHeap) -> Value {
        heap.pop().map_or_else(Value::none, Value::some)
    }

    #[function(name = "pop?")]
    pub fn maybe_max_pop(heap: &mut MaxHeap) -> Value {
        heap.pop().map_or_else(Value::none, Value::some)
    }

    #[function(name = "pop")]
    pub fn min_pop(heap: &mut MinHeap) -> anyhow::Result<Value> {
        heap.pop().ok_or_else(|| anyhow::anyhow!("heap is empty"))
    }

    #[function(name = "pop")]
    pub fn max_pop(heap: &mut MaxHeap) -> anyhow::Result<Value> {
        heap.pop().ok_or_else(|| anyhow::anyhow!("heap is empty"))
    }

    #[function(name = "push")]
    pub fn min_push(heap: &mut MinHeap, value: Value) {
        heap.push(value);
    }

    #[function(name = "push")]
    pub fn max_push(heap: &mut MaxHeap, value: Value) {
        heap.push(value);
    }
}
