use ndc_macros::export_module;

#[export_module]
mod inner {
    use std::cmp::Reverse;
    use std::collections::BinaryHeap;

    /// Creates a new empty min-heap (priority queue where the smallest element is popped first).
    #[function(name = "MinHeap", return_type = MinHeap<_>)]
    pub fn create_min_heap() -> ndc_vm::value::Value {
        ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::MinHeap(
            std::cell::RefCell::new(BinaryHeap::new()),
        )))
    }

    /// Creates a new empty max-heap (priority queue where the largest element is popped first).
    #[function(name = "MaxHeap", return_type = MaxHeap<_>)]
    pub fn create_max_heap() -> ndc_vm::value::Value {
        ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::MaxHeap(
            std::cell::RefCell::new(BinaryHeap::new()),
        )))
    }

    /// Removes and returns the smallest element from the min-heap, or `None` if empty.
    #[function(name = "pop?", return_type = Option<ndc_vm::value::Value>)]
    pub fn maybe_min_pop(
        heap: &mut BinaryHeap<Reverse<ndc_vm::value::OrdValue>>,
    ) -> ndc_vm::value::Value {
        match heap.pop() {
            None => ndc_vm::value::Value::None,
            Some(Reverse(ndc_vm::value::OrdValue(v))) => {
                ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::Some(v)))
            }
        }
    }

    /// Removes and returns the largest element from the max-heap, or `None` if empty.
    #[function(name = "pop?", return_type = Option<ndc_vm::value::Value>)]
    pub fn maybe_max_pop(heap: &mut BinaryHeap<ndc_vm::value::OrdValue>) -> ndc_vm::value::Value {
        match heap.pop() {
            None => ndc_vm::value::Value::None,
            Some(ndc_vm::value::OrdValue(v)) => {
                ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::Some(v)))
            }
        }
    }

    /// Removes and returns the smallest element from the min-heap, or errors if empty.
    #[function(name = "pop")]
    pub fn min_pop(
        heap: &mut BinaryHeap<Reverse<ndc_vm::value::OrdValue>>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        heap.pop()
            .map(|Reverse(ndc_vm::value::OrdValue(v))| v)
            .ok_or_else(|| anyhow::anyhow!("heap is empty"))
    }

    /// Removes and returns the largest element from the max-heap, or errors if empty.
    #[function(name = "pop")]
    pub fn max_pop(
        heap: &mut BinaryHeap<ndc_vm::value::OrdValue>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        heap.pop()
            .map(|ndc_vm::value::OrdValue(v)| v)
            .ok_or_else(|| anyhow::anyhow!("heap is empty"))
    }

    /// Pushes a value onto the min-heap.
    #[function(name = "push")]
    pub fn min_push(
        heap: &mut BinaryHeap<Reverse<ndc_vm::value::OrdValue>>,
        value: ndc_vm::value::Value,
    ) {
        heap.push(Reverse(ndc_vm::value::OrdValue(value)));
    }

    /// Pushes a value onto the max-heap.
    #[function(name = "push")]
    pub fn max_push(heap: &mut BinaryHeap<ndc_vm::value::OrdValue>, value: ndc_vm::value::Value) {
        heap.push(ndc_vm::value::OrdValue(value));
    }
}
