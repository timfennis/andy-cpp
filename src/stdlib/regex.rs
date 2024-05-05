#[andy_cpp_macros::export_module]
mod inner {
    use crate::interpreter::value::Value;
    use regex::Regex;

    pub fn nums(haystack: &str) -> Vec<i64> {
        let re = Regex::new(r"\d+").expect("TODO: fix error handling");
        re.captures_iter(haystack)
            .filter_map(|cap| {
                let (full, []) = cap.extract();
                full.parse::<i64>().ok()
            })
            .collect()
    }

    pub fn signed_nums(haystack: &str) -> Vec<i64> {
        let re = Regex::new(r"-?\d+").expect("TODO: fix error handling");
        re.captures_iter(haystack)
            .filter_map(|cap| {
                let (full, []) = cap.extract();
                full.parse::<i64>().ok()
            })
            .collect()
    }

    pub fn matches(haystack: &str, regex: &str) -> bool {
        let r = Regex::new(regex).expect("TODO: fix error handling");
        r.is_match(haystack)
    }

    pub fn captures(haystack: &str, regex: &str) -> Value {
        let r = Regex::new(regex).expect("TODO: fix error handling");

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

        Value::from(list)
    }

    pub fn capture_once(haystack: &str, regex: &str) -> Value {
        let r = Regex::new(regex).expect("TODO: fix error handling");

        let Some(captures) = r.captures(haystack) else {
            return Value::empty_list();
        };

        captures
            .iter()
            .filter_map(|x| x.map(|x| Value::from(x.as_str())))
            .collect::<Vec<_>>()
            .into()
    }
}
