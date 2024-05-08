use andy_cpp_macros::export_module;

#[export_module]
mod inner {
    use crate::interpreter::value::Value;
    use anyhow::anyhow;

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
    pub fn slice(string: &str, i: i64, j: i64) -> String {
        let i = usize::try_from(i).expect("TODO: support usize arguments");
        let j = usize::try_from(j).expect("TODO: support usize arguments");
        String::from(&string[i..j])
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
