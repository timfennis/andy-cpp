use crate::interpreter::value::Value;
use once_cell::sync::Lazy;
use regex::Regex;

#[ndc_macros::export_module]
mod inner {

    /// Extracts all signed integers from the given string.
    pub fn nums(haystack: &str) -> Vec<i64> {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"-?\d+").unwrap());

        RE.captures_iter(haystack)
            .filter_map(|cap| {
                let (full, []) = cap.extract();
                full.parse::<i64>().ok()
            })
            .collect()
    }

    /// Extracts all unsigned integers from the given string.
    pub fn unsigned_nums(haystack: &str) -> Vec<i64> {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\d+").unwrap());
        RE.captures_iter(haystack)
            .filter_map(|cap| {
                let (full, []) = cap.extract();
                full.parse::<i64>().ok()
            })
            .collect()
    }

    /// Returns `true` if the string matches the given regular expression.
    pub fn matches(haystack: &str, regex: &str) -> Result<bool, regex::Error> {
        let r = Regex::new(regex)?;
        Ok(r.is_match(haystack))
    }

    /// Returns all capture groups from the first match of the regular expression.
    pub fn captures(haystack: &str, regex: &str) -> Result<Value, regex::Error> {
        let r = Regex::new(regex)?;

        let list = r
            .captures_iter(haystack)
            .map(|captures| {
                captures
                    .iter()
                    .filter_map(|x| x.map(|x| Value::from(x.as_str())))
                    .collect::<Vec<_>>()
                    .into()
            })
            .collect::<Vec<Value>>();

        Ok(Value::from(list))
    }

    /// Returns the first capture group from the first match of the regular expression.
    pub fn capture_once(haystack: &str, regex: &str) -> Result<Value, regex::Error> {
        let r = Regex::new(regex)?;

        let Some(captures) = r.captures(haystack) else {
            return Ok(Value::empty_list());
        };

        let list = captures
            .iter()
            .filter_map(|x| x.map(|x| Value::from(x.as_str())))
            .collect::<Vec<_>>()
            .into();

        Ok(list)
    }
}
