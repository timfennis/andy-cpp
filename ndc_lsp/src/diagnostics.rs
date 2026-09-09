use ndc_analyser::AnalysisError;
use ndc_lexer::{Lexer, SourceId, Span, TokenLocation};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Url,
};

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
///
/// An LSP diagnostic has no separate note channel the way a terminal report
/// does, so help text is folded into the message instead of being dropped.
pub fn analysis_error_to_diagnostic(text: &str, uri: &Url, err: &AnalysisError) -> Diagnostic {
    let mut message = err.to_string();
    if let Some(label) = err.primary_label() {
        message.push_str(&format!(". {label}"));
    }
    if let Some(help) = err.help_text() {
        message.push_str(&format!(". {help}"));
    }
    let mut diagnostic = make_diagnostic(text, err.span(), message);
    if !err.related_labels().is_empty() {
        diagnostic.related_information = Some(
            err.related_labels()
                .iter()
                .map(|(span, message)| DiagnosticRelatedInformation {
                    location: Location::new(uri.clone(), span_to_range(text, *span)),
                    message: message.clone(),
                })
                .collect(),
        );
    }
    diagnostic
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

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_interpreter::Interpreter;
    use tower_lsp::lsp_types::{Position, Range};

    fn analyse_diagnostic(source: &str) -> Diagnostic {
        let mut interpreter = Interpreter::new();
        interpreter.configure(ndc_stdlib::register);
        let (_, result) = interpreter.analyse_str(source).expect("source parses");
        assert_eq!(result.errors.len(), 1, "{:?}", result.errors);
        let uri = Url::parse("file:///test.ndc").unwrap();
        analysis_error_to_diagnostic(source, &uri, &result.errors[0])
    }

    #[test]
    fn augmented_assignment_points_to_rhs_and_links_target() {
        let source = "let values = [1];\nvalues ++= [0.5];";
        let diagnostic = analyse_diagnostic(source);
        assert_eq!(
            diagnostic.range,
            Range::new(Position::new(1, 11), Position::new(1, 16))
        );
        assert_eq!(
            diagnostic.message,
            "mismatched types: found List<Float> but expected List<Int>. right operand inferred as List<Float>"
        );
        let related = diagnostic.related_information.unwrap();
        assert_eq!(related.len(), 1);
        assert_eq!(related[0].location.uri.as_str(), "file:///test.ndc");
        assert_eq!(
            related[0].location.range,
            Range::new(Position::new(1, 0), Position::new(1, 6))
        );
        assert_eq!(
            related[0].message,
            "left operand has type List<Int>; `++=` requires a compatible right operand"
        );
    }

    #[test]
    fn indexed_assignment_preserves_grouping_and_utf16_positions() {
        let source =
            "let values = [[1]];\nlet rhs = [0.5];\nlet marker = \"😀\"; (values[0]) ++=\n  rhs;";
        let diagnostic = analyse_diagnostic(source);
        assert_eq!(
            diagnostic.range,
            Range::new(Position::new(3, 2), Position::new(3, 5))
        );
        assert!(
            diagnostic
                .message
                .contains("right operand inferred as List<Float>")
        );
        let related = diagnostic.related_information.unwrap();
        assert_eq!(
            related[0].location.range,
            Range::new(Position::new(2, 19), Position::new(2, 30))
        );
        assert!(
            related[0]
                .message
                .contains("left operand has type List<Int>")
        );
        assert!(related[0].message.contains("`++=`"));
    }

    #[test]
    fn ordinary_type_error_keeps_its_message_and_has_no_related_locations() {
        let diagnostic = analyse_diagnostic("let value: Bool = 1;");
        assert_eq!(
            diagnostic.message,
            "mismatched types: found Int but expected Bool"
        );
        assert!(diagnostic.related_information.is_none());
    }
}
