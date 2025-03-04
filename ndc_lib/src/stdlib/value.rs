use ndc_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::heap::{MaxHeap, MinHeap};
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    pub fn ref_count(value: Value) -> usize {
        match value {
            Value::Option(_) | Value::Number(_) | Value::Bool(_) => 0,
            Value::Sequence(seq) => match seq {
                Sequence::String(rc) => Rc::strong_count(&rc),
                Sequence::List(rc) => Rc::strong_count(&rc),
                Sequence::Tuple(rc) => Rc::strong_count(&rc),
                Sequence::Map(rc, _) => Rc::strong_count(&rc),
                Sequence::Iterator(rc) => Rc::strong_count(&rc),
                Sequence::MaxHeap(rc) => Rc::strong_count(&rc),
                Sequence::MinHeap(rc) => Rc::strong_count(&rc),
                Sequence::Deque(rc) => Rc::strong_count(&rc),
            },
            Value::Function(r) => Rc::strong_count(&r),
        }
    }
    #[function(name = "Some")] // <-- fake type constructor
    pub fn some(value: Value) -> Value {
        Value::Option(Some(Box::new(value)))
    }

    pub fn none() -> Value {
        Value::Option(None)
    }

    pub fn is_some(value: &Value) -> Value {
        Value::Bool(matches!(value, Value::Option(Some(_))))
    }

    pub fn is_none(value: &Value) -> Value {
        Value::Bool(matches!(value, Value::Option(None)))
    }

    // TODO: this signature should be Option
    pub fn unwrap(value: Value) -> anyhow::Result<Value> {
        match value {
            Value::Option(Some(val)) => Ok(*val),
            Value::Option(None) => Err(anyhow::anyhow!("option was none")),
            _ => Err(anyhow::anyhow!(
                "incorrect argument to unwrap (temporary error)"
            )),
        }
    }

    pub fn clone(value: &Value) -> Value {
        match value {
            Value::Option(o) => Value::Option(o.clone()),
            number @ Value::Number(_) => number.clone(),
            Value::Bool(b) => Value::Bool(*b),
            Value::Sequence(Sequence::String(string)) => Value::from(string.borrow().to_owned()),
            Value::Sequence(Sequence::List(list)) => Value::from(list.borrow().to_owned()),
            Value::Sequence(Sequence::Map(map, default)) => Value::Sequence(Sequence::Map(
                Rc::new(RefCell::new(map.borrow().clone())),
                default.to_owned(),
            )),
            Value::Sequence(Sequence::Tuple(tuple)) => {
                Value::Sequence(Sequence::Tuple(tuple.clone()))
            }
            Value::Sequence(Sequence::Iterator(iterator)) => {
                Value::Sequence(Sequence::Iterator(iterator.clone()))
            }
            Value::Sequence(Sequence::MaxHeap(heap)) => Value::Sequence(Sequence::MaxHeap(
                Rc::new(RefCell::new(MaxHeap::from_heap(heap.borrow().to_owned()))),
            )),
            Value::Sequence(Sequence::MinHeap(heap)) => Value::Sequence(Sequence::MinHeap(
                Rc::new(RefCell::new(MinHeap::from_heap(heap.borrow().to_owned()))),
            )),
            Value::Sequence(Sequence::Deque(deque)) => Value::Sequence(Sequence::Deque(Rc::new(
                RefCell::new(deque.borrow().to_owned()),
            ))),
            Value::Function(f) => Value::from(f.borrow().to_owned()),
        }
    }

    pub fn deepcopy(value: &Value) -> Value {
        value.deepcopy()
    }
}
