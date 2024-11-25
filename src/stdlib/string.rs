use andy_cpp_macros::export_module;

use crate::interpreter::evaluate::EvaluationResult;
use crate::interpreter::iterator::mut_seq_into_iterator;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;

use anyhow::{anyhow, Context};
use itertools::Itertools;
use std::fmt::Write;
use std::rc::Rc;

pub fn join_to_string(list: &mut Sequence, sep: &str) -> EvaluationResult {
    let mut iter = mut_seq_into_iterator(list);
    match iter.next() {
        None => Ok(Value::from(String::new())),
        Some(first) => {
            let mut out = String::new();
            write!(out, "{}", first?).map_err(|_| anyhow!("failed to write to String"))?;
            while let Some(item) = iter.next() {
                write!(out, "{sep}{}", item?).map_err(|_| anyhow!("failed to write to String"))?;
            }
            Ok(Value::from(out))
        }
    }
}

#[export_module]
mod inner {

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

    pub fn chr(num: i64) -> anyhow::Result<String> {
        let num = u32::try_from(num).context("this number is not a valid character")?;
        let char = char::from_u32(num).context("this number is not a valid character")?;
        Ok(String::from(char))
    }

    // TODO: remove this function once actual slicing is supported
    pub fn slice(string: &str, i: usize, j: usize) -> anyhow::Result<String> {
        let len = string.chars().count();
        if j > len {
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

    pub fn join(list: &mut Sequence, sep: &str) -> EvaluationResult {
        join_to_string(list, sep)
    }

    pub fn paragraphs(string: &str) -> Vec<String> {
        string.split("\n\n").map(ToString::to_string).collect()
    }

    pub fn unparagraphs(list: &mut Sequence) -> EvaluationResult {
        join_to_string(list, "\n\n")
    }

    pub fn lines(string: &str) -> Vec<String> {
        string.lines().map(ToString::to_string).collect()
    }

    pub fn unlines(list: &mut Sequence) -> EvaluationResult {
        join_to_string(list, "\n")
    }

    pub fn words(string: &str) -> Vec<String> {
        string.split_whitespace().map(ToString::to_string).collect()
    }

    pub fn unwords(list: &mut Sequence) -> EvaluationResult {
        join_to_string(list, " ")
    }

    pub fn split(string: &str) -> Vec<String> {
        string.split_whitespace().map(ToString::to_string).collect()
    }

    pub fn starts_with(haystack: &str, pat: &str) -> bool {
        haystack.starts_with(pat)
    }

    pub fn ends_with(haystack: &str, pat: &str) -> bool {
        haystack.ends_with(pat)
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

    // &str is not a DoubleEnded searcher
    // what happens if you want to trim "aa" from "aaa": "[aa]a" or "a[aa]"
    // #[function(name = "trim")]
    // pub fn trim_matches<'a>(string: &'a str, pat: &str) -> &'a str {
    //     string.trim_matches(pat)
    // }

    pub fn trim_start(string: &str) -> &str {
        string.trim_start()
    }

    #[function(name = "trim_start")]
    pub fn trim_start_matches<'a>(string: &'a str, pat: &str) -> &'a str {
        string.trim_start_matches(pat)
    }
    pub fn trim_end(string: &str) -> &str {
        string.trim_end()
    }

    #[function(name = "trim_end")]
    pub fn trim_end_matches<'a>(string: &'a str, pat: &str) -> &'a str {
        string.trim_end_matches(pat)
    }

    pub fn is_digit(string: &str) -> anyhow::Result<bool> {
        if string.len() == 1 {
            Ok(string
                .chars()
                .next()
                .map(|c| c.is_ascii_digit())
                .expect("previous check guaranteed there must be one char"))
        } else {
            Err(anyhow!("is_digit requires string length to be exactly 1"))
        }
    }

    #[function(name = "is_digit")]
    pub fn is_digit_radix(string: &str, radix: i64) -> anyhow::Result<bool> {
        if string.len() == 1 {
            #[allow(clippy::cast_possible_truncation)]
            let radix = match radix {
                2..=36 => radix as u32,
                _ => {
                    return Err(anyhow!(
                        "Invalid radix: must be an integer between 2 and 36"
                    ))
                }
            };
            Ok(string
                .chars()
                .next()
                .map(|c| c.is_digit(radix))
                .expect("previous check guaranteed there must be one char"))
        } else {
            Err(anyhow!("is_digit requires string length to be exactly 1"))
        }
    }
}
