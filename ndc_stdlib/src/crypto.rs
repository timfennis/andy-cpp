use ndc_macros::export_module;

#[export_module]
mod internal {
    use sha1::Digest;

    /// Computes the md5 hash of an input string and returns it as an hex encoded string
    pub fn md5(val: &str) -> String {
        let digest = md5::compute(val);
        format!("{:x}", digest)
    }

    /// Computes the sha1 hash of an input string and returns it as an hex encoded string
    pub fn sha1(val: &str) -> String {
        let mut hasher = sha1::Sha1::new();
        hasher.update(val);
        let digest = hasher.finalize();
        format!("{:x}", digest)
    }
}
