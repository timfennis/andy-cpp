use ndc_macros::export_module;

#[export_module]
mod internal {
    use ::base64::Engine as _;

    /// Encodes a string using standard base64 (RFC 4648).
    pub fn base64_encode(val: &str) -> String {
        ::base64::engine::general_purpose::STANDARD.encode(val)
    }

    /// Decodes a standard base64-encoded string.
    pub fn base64_decode(val: &str) -> anyhow::Result<String> {
        let bytes = ::base64::engine::general_purpose::STANDARD.decode(val)?;
        String::from_utf8(bytes).map_err(anyhow::Error::new)
    }

    /// Encodes a string using URL-safe base64 without padding (RFC 4648 §5).
    pub fn base64_url_encode(val: &str) -> String {
        ::base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(val)
    }

    /// Decodes a URL-safe base64-encoded string (no padding required).
    pub fn base64_url_decode(val: &str) -> anyhow::Result<String> {
        let bytes = ::base64::engine::general_purpose::URL_SAFE_NO_PAD.decode(val)?;
        String::from_utf8(bytes).map_err(anyhow::Error::new)
    }
}
