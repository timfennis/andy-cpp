use itertools::Itertools;
use std::rc::Rc;

#[andy_cpp_macros::export_module]
mod inner {
    use crate::interpreter::value::{Sequence, Value};
    pub fn contains(list: &[Value], elem: &Value) -> bool {
        list.contains(elem)
    }

    pub fn push(list: &mut Vec<Value>, elem: &Value) {
        list.push(elem.clone());
    }

    pub fn cartesian_product(list_a: &[Value], list_b: &[Value]) -> Vec<Value> {
        list_a
            .iter()
            .cartesian_product(list_b)
            .map(|(a, b)| Value::Sequence(Sequence::Tuple(Rc::new(vec![a.clone(), b.clone()]))))
            .collect_vec()
    }
}
