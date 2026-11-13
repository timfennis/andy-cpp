use lsp_types::{Diagnostic, DiagnosticSeverity};
use ndc_interpreter::{AnalysisError, AnalysisResult, InterpreterError};
use ndc_lexer::{SourceId, Span};
use ndc_parser::ExpressionLocation;

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

/// Every diagnostic from one analysis of `text`: one per error the analysis
/// found, or those of the error that stopped it.
pub fn analysis_diagnostics(
    text: &str,
    analysed: &Result<(Vec<ExpressionLocation>, AnalysisResult), InterpreterError>,
) -> Vec<Diagnostic> {
    match analysed {
        Ok((_, result)) => result
            .errors
            .iter()
            .map(|err| analysis_error_to_diagnostic(text, err))
            .collect(),
        Err(err) => interpreter_error_to_diagnostics(text, err),
    }
}

/// Convert an [`AnalysisError`] into an LSP [`Diagnostic`].
fn analysis_error_to_diagnostic(text: &str, err: &AnalysisError) -> Diagnostic {
    make_diagnostic(text, err.span(), err.to_string(), err.help_text())
}

/// Convert an error that stopped the analysis of `text` into LSP diagnostics.
fn interpreter_error_to_diagnostics(text: &str, err: &InterpreterError) -> Vec<Diagnostic> {
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

    fn diagnostics(source: &str) -> Vec<Diagnostic> {
        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        analysis_diagnostics(source, &interpreter.analyse_str(source))
    }

    #[test]
    fn a_parse_error_is_reported_where_it_happens() {
        let diagnostics = diagnostics("let x = 1;\nlet = 2;");
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].range.start.line, 1);
    }

    #[test]
    fn a_lexer_error_is_reported_where_it_happens() {
        let diagnostics = diagnostics("let x = 1;\nlet y = \"open");
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].range.start.line, 1);
    }

    #[test]
    fn an_analysis_error_is_reported_at_its_span_with_its_help() {
        // Redefining an overload in the same scope is an error with help text.
        let source = "fn f(x) { 1 }\nfn f(y) { 2 }";
        let mut interpreter = Interpreter::capturing();
        let analysed = interpreter.analyse_str(source);
        let (_, result) = analysed.as_ref().expect("source parses");
        let [error] = result.errors.as_slice() else {
            panic!("expected one analysis error, got {:?}", result.errors);
        };
        let help = error.help_text().expect("the error has help text");

        let diagnostics = analysis_diagnostics(source, &analysed);
        let [diagnostic] = diagnostics.as_slice() else {
            panic!("expected one diagnostic, got {diagnostics:?}");
        };
        assert_eq!(diagnostic.range, span_to_range(source, error.span()));
        assert_eq!(diagnostic.range.start.line, 1);
        assert_eq!(diagnostic.message, format!("{error}. {help}"));
    }

    #[test]
    fn every_analysis_error_becomes_a_diagnostic() {
        let diagnostics = diagnostics("let x: Nonsense = 1;\nlet y: Gibberish = 2;");
        let lines = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.range.start.line)
            .collect::<Vec<_>>();
        assert_eq!(lines, [0, 1]);
    }
}
