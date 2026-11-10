use ndc_interpreter::{AnalysisError, InterpreterError};
use ndc_lexer::{SourceId, Span};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::util::span_to_range;

/// An LSP diagnostic has no separate note channel the way a terminal report
/// does, so help text is folded into the message instead of being dropped.
fn make_diagnostic(text: &str, span: Span, message: String, help: Option<&str>) -> Diagnostic {
    let message = match help {
        Some(help) => format!("{message}. {help}"),
        None => message,
    };
    Diagnostic {
        range: span_to_range(text, span),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        ..Default::default()
    }
}

/// Convert an [`AnalysisError`] into an LSP [`Diagnostic`].
pub fn analysis_error_to_diagnostic(text: &str, err: &AnalysisError) -> Diagnostic {
    make_diagnostic(text, err.span(), err.to_string(), err.help_text())
}

/// Convert an error that stopped the analysis of `text` into LSP diagnostics.
pub fn interpreter_error_to_diagnostics(text: &str, err: &InterpreterError) -> Vec<Diagnostic> {
    match err {
        InterpreterError::Lexer { cause } => {
            vec![make_diagnostic(
                text,
                cause.span(),
                cause.to_string(),
                cause.help_text(),
            )]
        }
        InterpreterError::Parser { cause } => {
            vec![make_diagnostic(
                text,
                cause.span(),
                cause.to_string(),
                cause.help_text(),
            )]
        }
        InterpreterError::Analysis { causes } => causes
            .iter()
            .map(|cause| analysis_error_to_diagnostic(text, cause))
            .collect(),
        InterpreterError::Compiler { cause } => {
            vec![make_diagnostic(text, cause.span(), cause.to_string(), None)]
        }
        InterpreterError::Vm(err) => {
            let span = err.span.unwrap_or(Span::new(SourceId::SYNTHETIC, 0, 0));
            vec![make_diagnostic(text, span, err.message.clone(), None)]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;

    fn syntax_diagnostics(source: &str) -> Vec<Diagnostic> {
        let err = Interpreter::capturing()
            .analyse_str(source)
            .expect_err("source should not lex or parse");
        interpreter_error_to_diagnostics(source, &err)
    }

    #[test]
    fn a_parse_error_is_reported_where_it_happens() {
        let diagnostics = syntax_diagnostics("let x = 1;\nlet = 2;");
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].range.start.line, 1);
    }

    #[test]
    fn a_lexer_error_is_reported_where_it_happens() {
        let diagnostics = syntax_diagnostics("let x = 1;\nlet y = \"open");
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].range.start.line, 1);
    }
}
