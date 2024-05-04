#[andy_cpp_macros::export_module]
mod inner {
    use regex::Regex;

    pub fn matches(haystack: &str, regex: &str) -> bool {
        let r = Regex::new(regex).expect("TODO: proper error if regex is invalid");
        r.is_match(haystack)
    }
}
