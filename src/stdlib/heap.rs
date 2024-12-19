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

    pub fn pop(seq: &Sequence) -> Value {
        match seq {
            Sequence::MinHeap(heap) => heap.borrow_mut().pop(),

            Sequence::MaxHeap(heap) => heap.borrow_mut().pop(),
            _ => todo!("not implemented"),
        }
    }

    pub fn push(seq: &Sequence, val: Value) {
        match seq {
            Sequence::MinHeap(heap) => heap.borrow_mut().push(val),
            Sequence::MaxHeap(heap) => heap.borrow_mut().push(val),
            _ => todo!("not implemented"),
        }
    }
    #[function(name = "MaxHeap")]
    pub fn create_max_heap() -> Value {
        Value::Sequence(Sequence::MaxHeap(Rc::new(RefCell::new(MaxHeap::new()))))
    }
}
