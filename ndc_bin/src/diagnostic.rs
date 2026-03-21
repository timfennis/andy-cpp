use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ndc_interpreter::InterpreterError;
use ndc_lexer::Span;

fn span_to_range(span: Span) -> std::ops::Range<usize> {
    span.offset()..span.end()
}

fn into_diagnostic(err: InterpreterError) -> Diagnostic<()> {
    match err {
        InterpreterError::Lexer { cause } => {
            let mut d = Diagnostic::error()
                .with_code("lexer")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary((), span_to_range(cause.span())).with_message("here"),
                ]);
            if let Some(help) = cause.help_text() {
                d = d.with_notes(vec![help.to_owned()]);
            }
            d
        }
        InterpreterError::Parser { cause } => {
            let mut d = Diagnostic::error()
                .with_code("parser")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary((), span_to_range(cause.span())).with_message("here"),
                ]);
            if let Some(help) = cause.help_text() {
                d = d.with_notes(vec![help.to_owned()]);
            }
            d
        }
        InterpreterError::Resolver { cause } => Diagnostic::error()
            .with_code("resolver")
            .with_message(cause.to_string())
            .with_labels(vec![
                Label::primary((), span_to_range(cause.span())).with_message("related to this"),
            ]),
        InterpreterError::Compiler { cause } => Diagnostic::error()
            .with_code("compiler")
            .with_message(cause.to_string())
            .with_labels(vec![
                Label::primary((), span_to_range(cause.span())).with_message("related to this"),
            ]),
        InterpreterError::Vm(err) => {
            let mut d = Diagnostic::error()
                .with_code("vm")
                .with_message(&err.message);
            if let Some(span) = err.span {
                d = d.with_labels(vec![
                    Label::primary((), span_to_range(span)).with_message("related to this"),
                ]);
            }
            d
        }
    }
}

pub fn emit_error(filename: &str, source: &str, err: InterpreterError) {
    let diagnostic = into_diagnostic(err);
    let file = SimpleFile::new(filename, source);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let _ = term::emit(&mut writer.lock(), &config, &file, &diagnostic);
}
