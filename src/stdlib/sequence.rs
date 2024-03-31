use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::value::Sequence;

    pub fn len(seq: &Sequence) -> usize {
        match seq {
            Sequence::String(s) => s.borrow().len(),
            Sequence::List(l) => l.borrow().len(),
        }
    }
}
