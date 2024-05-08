use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::value::{Sequence, Value};
    use anyhow::anyhow;
    use itertools::Itertools;
    use std::rc::Rc;

    pub fn ord(string: &str) -> anyhow::Result<i64> {
        let mut iterator = string.chars().map(|ch| ch as i64);
        let res = iterator
            .next()
            .ok_or_else(|| anyhow!("argument to ord cannot be an empty string"))?;

        if iterator.next().is_some() {
            return Err(anyhow!("argument to ord must be length 1"));
        }

        Ok(res)
    }

    // TODO: remove this function once actual slicing is supported
    pub fn slice(string: &str, i: usize, j: usize) -> anyhow::Result<String> {
        let len = string.chars().count();
        if i + j > len {
            return Err(anyhow::anyhow!("slice offset out of bounds"));
        }
        Ok(string.chars().dropping(i).take(j).collect::<String>())
    }

    pub fn to_lower(string: &str) -> String {
        string.to_lowercase()
    }

    pub fn to_upper(string: &str) -> String {
        string.to_uppercase()
    }

    pub fn reversed(string: &str) -> String {
        string.chars().rev().collect()
    }

    pub fn reverse(string: &mut String) {
        *string = string.chars().rev().collect();
    }

    pub fn append(string: &mut String, value: &str) {
        string.push_str(value);
    }

    pub fn lines(string: &str) -> Vec<String> {
        string.lines().map(ToString::to_string).collect()
    }

    pub fn split(string: &str) -> Vec<String> {
        string.split_whitespace().map(ToString::to_string).collect()
    }

    #[function(name = "split")]
    pub fn split_with_pattern(string: &str, pattern: &str) -> Vec<String> {
        string.split(pattern).map(ToString::to_string).collect()
    }

    pub fn split_once(string: &str, pattern: &str) -> Value {
        match string.split_once(pattern) {
            Some((fst, snd)) => Value::Sequence(Sequence::Tuple(Rc::new(vec![
                Value::from(fst),
                Value::from(snd),
            ]))),
            None => Value::Unit,
        }
    }

    pub fn trim(string: &str) -> &str {
        string.trim()
    }
}
