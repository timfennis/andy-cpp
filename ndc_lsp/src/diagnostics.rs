use ndc_analyser::AnalysisError;
use ndc_lexer::{Lexer, SourceId, Span, TokenLocation};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::util::span_to_range;

fn make_diagnostic(text: &str, span: Span, message: String) -> Diagnostic {
    Diagnostic {
        range: span_to_range(text, span),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        ..Default::default()
    }
}

/// Convert an [`AnalysisError`] into an LSP [`Diagnostic`].
pub fn analysis_error_to_diagnostic(text: &str, err: &AnalysisError) -> Diagnostic {
    make_diagnostic(text, err.span(), err.to_string())
}

/// Lex and parse the source text, returning any diagnostics and (on success)
/// the token stream's parsed AST.
pub fn lex_and_parse(text: &str) -> (Vec<Diagnostic>, Option<Vec<ndc_parser::ExpressionLocation>>) {
    let scanner = Lexer::new(text, SourceId::SYNTHETIC);
    let tokens: Result<Vec<TokenLocation>, _> = scanner.collect();

    let tokens = match tokens {
        Ok(t) => t,
        Err(err) => {
            let diag = make_diagnostic(text, err.location(), format!("{err}"));
            return (vec![diag], None);
        }
    };

    let mut parser = ndc_parser::Parser::from_tokens(tokens);
    match parser.parse() {
        Ok(ast) => (vec![], Some(ast)),
        Err(err) => {
            let diag = make_diagnostic(text, err.location(), format!("{err}"));
            (vec![diag], None)
        }
    }
}
