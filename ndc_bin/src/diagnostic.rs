use miette::{Diagnostic, LabeledSpan, SourceSpan};
use ndc_lib::interpreter::InterpreterError;
use ndc_lexer::Span;
use std::fmt;

fn span_to_source_span(span: Span) -> SourceSpan {
    (span.offset(), span.end() - span.offset()).into()
}

pub struct NdcReport {
    message: String,
    span: Option<SourceSpan>,
    label: &'static str,
    help: Option<String>,
}

impl fmt::Display for NdcReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl fmt::Debug for NdcReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for NdcReport {}

impl Diagnostic for NdcReport {
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.help
            .as_ref()
            .map(|h| -> Box<dyn fmt::Display> { Box::new(h) })
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        self.span.map(|s| -> Box<dyn Iterator<Item = LabeledSpan>> {
            Box::new(std::iter::once(LabeledSpan::at(s, self.label)))
        })
    }
}

impl From<InterpreterError> for NdcReport {
    fn from(err: InterpreterError) -> Self {
        match err {
            InterpreterError::Lexer { cause } => Self {
                message: cause.to_string(),
                span: Some(span_to_source_span(cause.span())),
                label: "here",
                help: cause.help_text().map(str::to_owned),
            },
            InterpreterError::Parser { cause } => Self {
                message: cause.to_string(),
                span: Some(span_to_source_span(cause.span())),
                label: "here",
                help: cause.help_text().map(str::to_owned),
            },
            InterpreterError::Resolver { cause } => Self {
                message: cause.to_string(),
                span: Some(span_to_source_span(cause.span())),
                label: "related to this",
                help: None,
            },
            InterpreterError::Evaluation(cause) => Self {
                message: cause.to_string(),
                span: Some(span_to_source_span(cause.span())),
                label: "related to this",
                help: cause.help_text().map(str::to_owned),
            },
        }
    }
}
