use once_cell::sync::Lazy;
use regex::Regex;

#[ndc_macros::export_module]
mod inner {
    /// Extracts all signed integers from the given string.
    #[function(return_type = Vec<i64>)]
    pub fn nums(haystack: &str) -> ndc_vm::value::Value {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"-?\d+").unwrap());

        ndc_vm::value::Value::list(
            RE.captures_iter(haystack)
                .filter_map(|cap| {
                    let (full, []) = cap.extract();
                    full.parse::<i64>().ok().map(ndc_vm::value::Value::Int)
                })
                .collect(),
        )
    }

    /// Extracts all unsigned integers from the given string.
    #[function(return_type = Vec<i64>)]
    pub fn unsigned_nums(haystack: &str) -> ndc_vm::value::Value {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\d+").unwrap());

        ndc_vm::value::Value::list(
            RE.captures_iter(haystack)
                .filter_map(|cap| {
                    let (full, []) = cap.extract();
                    full.parse::<i64>().ok().map(ndc_vm::value::Value::Int)
                })
                .collect(),
        )
    }

    /// Returns `true` if the string matches the given regular expression.
    pub fn matches(haystack: &str, regex: &str) -> Result<bool, regex::Error> {
        let r = Regex::new(regex)?;
        Ok(r.is_match(haystack))
    }

    /// Returns all capture groups from the first match of the regular expression.
    #[function(return_type = Vec<String>)]
    pub fn captures(haystack: &str, regex: &str) -> Result<ndc_vm::value::Value, regex::Error> {
        let r = Regex::new(regex)?;

        let list = r
            .captures_iter(haystack)
            .map(|captures| {
                ndc_vm::value::Value::list(
                    captures
                        .iter()
                        .filter_map(|x| x.map(|x| ndc_vm::value::Value::string(x.as_str())))
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<ndc_vm::value::Value>>();

        Ok(ndc_vm::value::Value::list(list))
    }

    /// Returns the first capture group from the first match of the regular expression.
    #[function(return_type = Vec<String>)]
    pub fn capture_once(haystack: &str, regex: &str) -> Result<ndc_vm::value::Value, regex::Error> {
        let r = Regex::new(regex)?;

        let Some(captures) = r.captures(haystack) else {
            return Ok(ndc_vm::value::Value::list(vec![]));
        };

        let list = captures
            .iter()
            .filter_map(|x| x.map(|x| ndc_vm::value::Value::string(x.as_str())))
            .collect::<Vec<_>>();

        Ok(ndc_vm::value::Value::list(list))
    }
}
