use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ndc_interpreter::InterpreterError;
use ndc_lexer::{SourceDb, SourceId, Span};
use std::ops::Range;

fn span_to_range(span: Span) -> Range<usize> {
    span.offset()..span.end()
}

struct DiagnosticFiles<'a>(&'a SourceDb);

impl<'a> files::Files<'a> for DiagnosticFiles<'a> {
    type FileId = SourceId;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, id: SourceId) -> Result<&'a str, files::Error> {
        if id == SourceId::SYNTHETIC {
            return Ok("<synthetic>");
        }
        Ok(self.0.name(id))
    }

    fn source(&'a self, id: SourceId) -> Result<&'a str, files::Error> {
        if id == SourceId::SYNTHETIC {
            return Ok("");
        }
        Ok(self.0.source(id))
    }

    fn line_index(&'a self, id: SourceId, byte_index: usize) -> Result<usize, files::Error> {
        let source = self.source(id)?;
        Ok(files::line_starts(source)
            .take_while(|&start| start <= byte_index)
            .count()
            .saturating_sub(1))
    }

    fn line_range(&'a self, id: SourceId, line_index: usize) -> Result<Range<usize>, files::Error> {
        let source = self.source(id)?;
        let line_starts: Vec<usize> = files::line_starts(source).collect();
        let start = *line_starts
            .get(line_index)
            .ok_or(files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len().saturating_sub(1),
            })?;
        let end = line_starts
            .get(line_index + 1)
            .copied()
            .unwrap_or(source.len());
        Ok(start..end)
    }
}

fn into_diagnostic(err: InterpreterError) -> Diagnostic<SourceId> {
    match err {
        InterpreterError::Lexer { cause } => {
            let span = cause.span();
            let mut d = Diagnostic::error()
                .with_code("lexer")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary(span.source_id(), span_to_range(span)).with_message("here"),
                ]);
            if let Some(help) = cause.help_text() {
                d = d.with_notes(vec![help.to_owned()]);
            }
            d
        }
        InterpreterError::Parser { cause } => {
            let span = cause.span();
            let mut d = Diagnostic::error()
                .with_code("parser")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary(span.source_id(), span_to_range(span)).with_message("here"),
                ]);
            if let Some(help) = cause.help_text() {
                d = d.with_notes(vec![help.to_owned()]);
            }
            d
        }
        InterpreterError::Resolver { cause } => {
            let span = cause.span();
            Diagnostic::error()
                .with_code("resolver")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary(span.source_id(), span_to_range(span))
                        .with_message("related to this"),
                ])
        }
        InterpreterError::Compiler { cause } => {
            let span = cause.span();
            Diagnostic::error()
                .with_code("compiler")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary(span.source_id(), span_to_range(span))
                        .with_message("related to this"),
                ])
        }
        InterpreterError::Vm(err) => {
            let mut d = Diagnostic::error()
                .with_code("vm")
                .with_message(&err.message);
            if let Some(span) = err.span {
                d = d.with_labels(vec![
                    Label::primary(span.source_id(), span_to_range(span))
                        .with_message("related to this"),
                ]);
            }
            d
        }
    }
}

pub fn emit_error(source_db: &SourceDb, err: InterpreterError) {
    let diagnostic = into_diagnostic(err);
    let files = DiagnosticFiles(source_db);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
}
