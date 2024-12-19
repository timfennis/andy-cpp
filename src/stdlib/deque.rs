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
}
