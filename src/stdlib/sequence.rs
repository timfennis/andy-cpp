use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::value::{Sequence, Value};
    use itertools::Itertools;
    use std::cell::RefCell;
    use std::rc::Rc;

    pub fn max(seq: &Sequence) -> Value {
        match seq {
            Sequence::String(s) => {
                let item =
                    String::from(s.borrow().chars().max().expect("TODO: fix error handling"));
                Value::from(item)
            }
            Sequence::List(l) => {
                let l = l.borrow();
                let item = l
                    .iter()
                    .max_by(|a, b| a.partial_cmp(b).expect("TODO: fix error handling"))
                    .expect("TODO: fix error handling");
                item.clone()
            }
            Sequence::Tuple(l) => {
                let item = l
                    .iter()
                    .max_by(|a, b| a.partial_cmp(b).expect("TODO: fix error handling"))
                    .expect("TODO: fix error handling");
                item.clone()
            }
            Sequence::Map(map, _) => {
                let map = map.borrow();
                let item = map
                    .keys()
                    .max_by(|a, b| a.partial_cmp(b).expect("TODO: fix error handling"))
                    .expect("TODO: fix error handling");
                item.clone()
            }
        }
    }
    pub fn min(seq: &Sequence) -> Value {
        match seq {
            Sequence::String(s) => {
                let item =
                    String::from(s.borrow().chars().min().expect("TODO: fix error handling"));
                Value::from(item)
            }
            Sequence::List(l) => {
                let l = l.borrow();
                let item = l
                    .iter()
                    .min_by(|a, b| a.partial_cmp(b).expect("TODO: fix error handling"))
                    .expect("TODO: fix error handling");
                item.clone()
            }
            Sequence::Tuple(l) => {
                let item = l
                    .iter()
                    .min_by(|a, b| a.partial_cmp(b).expect("TODO: fix error handling"))
                    .expect("TODO: fix error handling");
                item.clone()
            }
            Sequence::Map(map, _) => {
                let map = map.borrow();
                let item = map
                    .keys()
                    .min_by(|a, b| a.partial_cmp(b).expect("TODO: fix error handling"))
                    .expect("TODO: fix error handling");
                item.clone()
            }
        }
    }

    pub fn sorted(seq: &Sequence) -> Sequence {
        let cmp = |a: &&Value, b: &&Value| {
            a.partial_cmp(b).expect(
                "TODO: implement proper error handling in native functions (type cannot be sorted)",
            )
        };
        match seq {
            Sequence::String(str) => {
                let sorted = str.borrow().chars().sorted().collect::<String>();
                Sequence::String(Rc::new(RefCell::new(sorted)))
            }
            Sequence::List(list) => {
                let sorted = list
                    .borrow()
                    .iter()
                    .sorted_unstable_by(cmp)
                    .cloned()
                    .collect::<Vec<Value>>();
                Sequence::List(Rc::new(RefCell::new(sorted)))
            }
            Sequence::Tuple(list) => {
                let sorted = list
                    .iter()
                    .sorted_unstable_by(cmp)
                    .cloned()
                    .collect::<Vec<Value>>();
                Sequence::Tuple(Rc::new(sorted))
            }
            Sequence::Map(map, _) => {
                let sorted = map
                    .borrow()
                    .keys()
                    .sorted_unstable_by(cmp)
                    .cloned()
                    .collect::<Vec<Value>>();
                Sequence::List(Rc::new(RefCell::new(sorted)))
            }
        }
    }
    pub fn len(seq: &Sequence) -> usize {
        match seq {
            Sequence::String(s) => s.borrow().len(),
            Sequence::List(l) => l.borrow().len(),
            Sequence::Tuple(t) => t.len(),
            Sequence::Map(d, _) => d.borrow().len(),
        }
    }
}
