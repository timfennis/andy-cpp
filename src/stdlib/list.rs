use itertools::Itertools;
use std::rc::Rc;

#[andy_cpp_macros::export_module]
mod inner {
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use anyhow::anyhow;

    pub fn contains(list: &[Value], elem: &Value) -> bool {
        list.contains(elem)
    }

    pub fn contains_subsequence(list: &[Value], subsequence: &[Value]) -> bool {
        list.windows(subsequence.len()).contains(&subsequence)
    }

    pub fn find_subsequence(list: &[Value], subsequence: &[Value]) -> Value {
        let result = list
            .windows(subsequence.len())
            .enumerate()
            .find(|(_, seq)| *seq == subsequence)
            .map(|(idx, _)| idx);
        if let Some(result) = result {
            Value::from(result)
        } else {
            Value::Unit
        }
    }

    pub fn insert(list: &mut Vec<Value>, index: usize, elem: Value) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }
        list.insert(index, elem);
        Ok(Value::Unit)
    }

    pub fn remove(list: &mut Vec<Value>, index: usize) -> anyhow::Result<Value> {
        if index > list.len() {
            return Err(anyhow!("index {index} is out of bounds"));
        }

        Ok(list.remove(index))
    }

    pub fn push(list: &mut Vec<Value>, elem: Value) {
        list.push(elem);
    }

    pub fn pop(list: &mut Vec<Value>) -> Value {
        list.pop().unwrap_or(Value::Unit)
    }

    pub fn pop_left(list: &mut Vec<Value>) -> Value {
        if list.is_empty() {
            return Value::Unit;
        }

        list.remove(0)
    }

    pub fn reversed(list: &[Value]) -> Vec<Value> {
        list.iter().rev().cloned().collect::<Vec<Value>>()
    }

    pub fn cartesian_product(list_a: &[Value], list_b: &[Value]) -> Vec<Value> {
        list_a
            .iter()
            .cartesian_product(list_b)
            .map(|(a, b)| Value::Sequence(Sequence::Tuple(Rc::new(vec![a.clone(), b.clone()]))))
            .collect_vec()
    }
}
