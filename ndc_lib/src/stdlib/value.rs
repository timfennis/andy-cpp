use ndc_macros::export_module;
use std::fmt::Write;

#[export_module]
mod inner {
    use crate::interpreter::function::Callable;
    use crate::interpreter::heap::{MaxHeap, MinHeap};
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Returns the documentation as a string for a given function in Andy C++.
    ///
    /// This function takes a function as its argument and returns a string containing its documentation.
    pub fn docs(func: &Callable<'_>) -> anyhow::Result<String> {
        let mut buf = String::new();

        for (sig, fun) in func.function.borrow().implementations() {
            if fun.name().is_empty() {
                write!(buf, "fn({sig})")?;
            } else {
                write!(buf, "fn {}({sig})", fun.name())?;
            }

            if !fun.short_documentation().is_empty() {
                writeln!(buf, " -> {}", fun.short_documentation())?;
            } else {
                writeln!(buf)?;
            }
        }

        buf.pop(); // Remove last newline

        Ok(buf)
    }

    /// Returns the reference count for the value, if the value is not reference counted it will return 0
    ///
    /// Note: this function does increase the ref count by 1
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

    /// Creates a new instance of `Some`
    #[function(name = "Some", return_type = Option<Value>)] // <-- fake type constructor
    pub fn some(value: Value) -> Value {
        Value::Option(Some(Box::new(value)))
    }

    /// Creates a new instance of `None`
    #[function(return_type = Option)]
    pub fn none() -> Value {
        Value::Option(None)
    }

    /// Returns true if the argument is Some<T>
    pub fn is_some(value: &Value) -> bool {
        matches!(value, Value::Option(Some(_)))
    }

    /// Returns true if the argument is None
    pub fn is_none(value: &Value) -> bool {
        matches!(value, Value::Option(None))
    }

    /// Extracts the value from an Option or errors if it's either None or a non-Option type
    ///
    /// Note: this function should take an Option as parameter
    // TODO: the type of value should be `Option<Value>` but the macro crate probably doesn't support that yet
    pub fn unwrap(value: Value) -> anyhow::Result<Value> {
        match value {
            Value::Option(Some(val)) => Ok(*val),
            Value::Option(None) => Err(anyhow::anyhow!("option was none")),
            _ => Err(anyhow::anyhow!(
                "incorrect argument to unwrap (temporary error)"
            )),
        }
    }

    /// Returns a shallow copy of the given value.
    pub fn clone(value: &Value) -> Value {
        match value {
            Value::Option(o) => Value::Option(o.clone()),
            number @ Value::Number(_) => number.clone(),
            Value::Bool(b) => Value::Bool(*b),
            Value::Sequence(Sequence::String(string)) => Value::string(string.borrow().to_owned()),
            Value::Sequence(Sequence::List(list)) => Value::list(list.borrow().to_owned()),
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

    /// Returns a deep copy of the given value, duplicating all nested structures.
    pub fn deepcopy(value: &Value) -> Value {
        value.deepcopy()
    }
}
