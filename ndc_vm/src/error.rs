use ndc_lexer::Span;

#[derive(Debug, Clone)]
pub struct VmError {
    pub message: String,
    pub span: Option<Span>,
}

impl VmError {
    /// Create an error with a known source span (from VM instruction execution).
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
        }
    }

    /// Create an error from a native function (no source span available yet;
    /// the VM will fill in the call-site span when it catches this).
    pub fn native(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
        }
    }
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for VmError {}
