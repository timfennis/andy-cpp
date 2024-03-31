use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    pub fn reversed(string: &str) -> String {
        string.chars().rev().collect()
    }

    pub fn reverse(string: &mut String) {
        *string = string.chars().rev().collect();
    }

    pub fn lines(string: &str) -> Vec<String> {
        string.lines().map(ToString::to_string).collect()
    }

    pub fn split(string: &str) -> Vec<String> {
        string
            .split_whitespace() // TODO: or split_ascii_whitespace?
            .map(ToString::to_string)
            .collect()
    }

    #[function(name = "split")]
    pub fn split_with_pattern(string: &str, pattern: &str) -> Vec<String> {
        string.split(pattern).map(ToString::to_string).collect()
    }

    pub fn trim(string: &str) -> &str {
        string.trim()
    }
}
