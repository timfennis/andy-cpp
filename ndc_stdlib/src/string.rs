use ndc_macros::export_module;

use std::cell::RefCell;
use std::rc::Rc;

type StringRepr = Rc<RefCell<String>>;

use anyhow::{Context, anyhow};

pub fn join_to_string(list: ndc_vm::value::SeqValue, sep: &str) -> anyhow::Result<String> {
    use std::fmt::Write;
    let mut iter = list
        .try_into_iter()
        .ok_or_else(|| anyhow!("join requires a sequence"))?;
    match iter.next() {
        None => Ok(String::new()),
        Some(first) => {
            let mut out = String::new();
            write!(out, "{first}").map_err(|_| anyhow!("failed to write to String"))?;
            for item in iter {
                write!(out, "{sep}{item}").map_err(|_| anyhow!("failed to write to String"))?;
            }
            Ok(out)
        }
    }
}

#[export_module]
mod inner {

    /// Concatenates two values into a string.
    #[function(name = "<>")]
    pub fn op_string_concat(left: ndc_vm::value::Value, right: ndc_vm::value::Value) -> String {
        format!("{left}{right}")
    }

    /// Appends the right string to the left string in place.
    #[function(name = "++=")]
    pub fn op_list_concat(left: &mut StringRepr, right: &mut StringRepr) -> ndc_vm::value::Value {
        if Rc::ptr_eq(left, right) {
            let new = right.borrow().repeat(2).clone();
            *left.borrow_mut() = new;
        } else {
            left.borrow_mut().push_str(&right.borrow())
        }
        ndc_vm::value::Value::from_string_rc(Rc::clone(left))
    }

    /// Returns the provided value as a string
    pub fn string(value: ndc_vm::value::Value) -> String {
        format!("{value}")
    }

    /// Returns the Unicode code point of a 1-length string.
    pub fn ord(string: &str) -> anyhow::Result<i64> {
        if string.chars().count() == 1 {
            return Ok(i64::from(u32::from(string.chars().next().expect(
                "guaranteed to succeed thanks to previous call to chars",
            ))));
        }

        anyhow::bail!("invalid input length")
    }

    /// Returns the character corresponding to the given Unicode code point.
    pub fn chr(num: i64) -> anyhow::Result<String> {
        let num = u32::try_from(num).context("this number is not a valid character")?;
        let char = char::from_u32(num).context("this number is not a valid character")?;
        Ok(String::from(char))
    }

    /// Converts the string to lowercase.
    pub fn to_lower(string: &str) -> String {
        string.to_lowercase()
    }

    /// Converts the string to uppercase.
    pub fn to_upper(string: &str) -> String {
        string.to_uppercase()
    }

    /// Returns `true` if the string is entirely uppercase.
    pub fn is_upper(string: &str) -> bool {
        string.chars().all(char::is_uppercase)
    }

    /// Returns `true` if the string is entirely lowercase.
    pub fn is_lower(string: &str) -> bool {
        string.chars().all(char::is_lowercase)
    }

    /// Returns a new string with the characters reversed.
    pub fn reversed(string: &str) -> String {
        string.chars().rev().collect()
    }

    /// Reverses the string in place.
    pub fn reverse(string: &mut String) {
        *string = string.chars().rev().collect();
    }

    /// Appends `value` to the given string.
    pub fn append(string: &mut String, value: &str) {
        string.push_str(value);
    }

    /// Concatenates two strings into a new string.
    #[function(name = "++")]
    pub fn concat(left: &str, right: &str) -> String {
        format!("{left}{right}")
    }

    /// Joins elements of the sequence into a single string using `sep` as the separator.
    pub fn join(list: ndc_vm::value::SeqValue, sep: &str) -> anyhow::Result<String> {
        join_to_string(list, sep)
    }

    /// Splits the string into paragraphs, using blank lines as separators.
    #[function(return_type = Vec<String>)]
    pub fn paragraphs(string: &str) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(
            string
                .split("\n\n")
                .map(ndc_vm::value::Value::string)
                .collect(),
        )
    }

    /// Joins paragraphs into a single string, inserting blank lines between them.
    pub fn unparagraphs(list: ndc_vm::value::SeqValue) -> anyhow::Result<String> {
        join_to_string(list, "\n\n")
    }

    /// Splits the string into lines, using newline characters as separators.
    #[function(return_type = Vec<String>)]
    pub fn lines(string: &str) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(string.lines().map(ndc_vm::value::Value::string).collect())
    }

    /// Joins lines into a single string, inserting newline characters between them.
    pub fn unlines(list: ndc_vm::value::SeqValue) -> anyhow::Result<String> {
        join_to_string(list, "\n")
    }

    /// Splits the string into words, using whitespace as the separator.
    #[function(return_type = Vec<String>)]
    pub fn words(string: &str) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(
            string
                .split_whitespace()
                .map(ndc_vm::value::Value::string)
                .collect(),
        )
    }

    /// Joins words into a single string, separating them with spaces.
    pub fn unwords(list: ndc_vm::value::SeqValue) -> anyhow::Result<String> {
        join_to_string(list, " ")
    }

    /// Splits the string by whitespace into a list of substrings.
    #[function(return_type = Vec<String>)]
    pub fn split(string: &str) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(
            string
                .split_whitespace()
                .map(ndc_vm::value::Value::string)
                .collect(),
        )
    }

    /// Returns `true` if `haystack` starts with `pat`.
    pub fn starts_with(haystack: &str, pat: &str) -> bool {
        haystack.starts_with(pat)
    }

    /// Returns `true` if `haystack` ends with `pat`.
    pub fn ends_with(haystack: &str, pat: &str) -> bool {
        haystack.ends_with(pat)
    }

    /// Splits the string using a given pattern as the delimiter.
    #[function(name = "split", return_type = Vec<String>)]
    pub fn split_with_pattern(string: &str, pattern: &str) -> ndc_vm::value::Value {
        ndc_vm::value::Value::list(
            string
                .split(pattern)
                .map(ndc_vm::value::Value::string)
                .collect(),
        )
    }

    /// Splits the string at the first occurrence of `pattern`, returning a tuple-like value.
    #[function(return_type = (String, String))]
    pub fn split_once(string: &str, pattern: &str) -> ndc_vm::value::Value {
        match string.split_once(pattern) {
            Some((fst, snd)) => ndc_vm::value::Value::tuple(vec![
                ndc_vm::value::Value::string(fst),
                ndc_vm::value::Value::string(snd),
            ]),
            None => ndc_vm::value::Value::unit(),
        }
    }

    /// Removes leading and trailing whitespace from the string.
    pub fn trim(string: &str) -> &str {
        string.trim()
    }

    // &str is not a DoubleEnded searcher
    // what happens if you want to trim "aa" from "aaa": "[aa]a" or "a[aa]"
    // #[function(name = "trim")]
    // pub fn trim_matches<'a>(string: &'a str, pat: &str) -> &'a str {
    //     string.trim_matches(pat)
    // }

    /// Removes leading whitespace from the string.
    pub fn trim_start(string: &str) -> &str {
        string.trim_start()
    }

    /// Removes leading occurrences of `pat` from the string.
    #[function(name = "trim_start")]
    pub fn trim_start_matches<'a>(string: &'a str, pat: &str) -> &'a str {
        string.trim_start_matches(pat)
    }

    /// Removes trailing whitespace from the string.
    pub fn trim_end(string: &str) -> &str {
        string.trim_end()
    }

    /// Removes trailing occurrences of `pat` from the string.
    #[function(name = "trim_end")]
    pub fn trim_end_matches<'a>(string: &'a str, pat: &str) -> &'a str {
        string.trim_end_matches(pat)
    }

    /// Returns `true` if the string consists only of digits.
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

    /// Replaces all occurrences of `from` with `to` in `source`.
    pub fn replace(source: &str, from: &str, to: &str) -> String {
        source.replace(from, to)
    }

    /// Returns `true` if the string consists only of digits in the given radix.
    #[function(name = "is_digit")]
    pub fn is_digit_radix(string: &str, radix: i64) -> anyhow::Result<bool> {
        if string.len() == 1 {
            #[allow(clippy::cast_possible_truncation)]
            let radix = match radix {
                2..=36 => u32::try_from(radix).expect("must be valid"),
                _ => {
                    return Err(anyhow!(
                        "Invalid radix: must be an integer between 2 and 36"
                    ));
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
