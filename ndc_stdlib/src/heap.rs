use ndc_core::StaticType;
use ndc_macros::export_module;

#[export_module]
mod inner {
    use std::cmp::Reverse;
    use std::collections::BinaryHeap;

    #[function(name = "MinHeap", return_type = MinHeap<_>)]
    pub fn create_min_heap() -> ndc_vm::value::Value {
        ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::MinHeap(
            std::cell::RefCell::new(BinaryHeap::new()),
        )))
    }

    #[function(name = "MaxHeap", return_type = MaxHeap<_>)]
    pub fn create_max_heap() -> ndc_vm::value::Value {
        ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::MaxHeap(
            std::cell::RefCell::new(BinaryHeap::new()),
        )))
    }

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

    #[function(name = "pop?", return_type = Option<ndc_vm::value::Value>)]
    pub fn maybe_max_pop(heap: &mut BinaryHeap<ndc_vm::value::OrdValue>) -> ndc_vm::value::Value {
        match heap.pop() {
            None => ndc_vm::value::Value::None,
            Some(ndc_vm::value::OrdValue(v)) => {
                ndc_vm::value::Value::Object(std::rc::Rc::new(ndc_vm::value::Object::Some(v)))
            }
        }
    }

    #[function(name = "pop")]
    pub fn min_pop(
        heap: &mut BinaryHeap<Reverse<ndc_vm::value::OrdValue>>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        heap.pop()
            .map(|Reverse(ndc_vm::value::OrdValue(v))| v)
            .ok_or_else(|| anyhow::anyhow!("heap is empty"))
    }

    #[function(name = "pop")]
    pub fn max_pop(
        heap: &mut BinaryHeap<ndc_vm::value::OrdValue>,
    ) -> anyhow::Result<ndc_vm::value::Value> {
        heap.pop()
            .map(|ndc_vm::value::OrdValue(v)| v)
            .ok_or_else(|| anyhow::anyhow!("heap is empty"))
    }

    #[function(name = "push")]
    pub fn min_push(
        heap: &mut BinaryHeap<Reverse<ndc_vm::value::OrdValue>>,
        value: ndc_vm::value::Value,
    ) {
        heap.push(Reverse(ndc_vm::value::OrdValue(value)));
    }

    #[function(name = "push")]
    pub fn max_push(heap: &mut BinaryHeap<ndc_vm::value::OrdValue>, value: ndc_vm::value::Value) {
        heap.push(ndc_vm::value::OrdValue(value));
    }
}
