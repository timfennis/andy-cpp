use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ndc_interpreter::InterpreterError;
use ndc_lexer::{SourceDb, SourceId};
use std::ops::Range;

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

fn into_diagnostics(err: InterpreterError) -> Vec<Diagnostic<SourceId>> {
    match err {
        InterpreterError::Lexer { cause } => {
            let span = cause.span();
            let mut d = Diagnostic::error()
                .with_code("lexer")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary(span.source_id(), span.range()).with_message("here"),
                ]);
            if let Some(help) = cause.help_text() {
                d = d.with_notes(vec![help.to_owned()]);
            }
            vec![d]
        }
        InterpreterError::Parser { cause } => {
            let span = cause.span();
            let mut d = Diagnostic::error()
                .with_code("parser")
                .with_message(cause.to_string())
                .with_labels(vec![
                    Label::primary(span.source_id(), span.range()).with_message("here"),
                ]);
            if let Some(help) = cause.help_text() {
                d = d.with_notes(vec![help.to_owned()]);
            }
            vec![d]
        }
        InterpreterError::Resolver { causes } => causes
            .iter()
            .map(|cause| {
                let span = cause.span();
                let mut labels = vec![
                    Label::primary(span.source_id(), span.range())
                        .with_message(cause.primary_label().unwrap_or("related to this")),
                ];
                labels.extend(cause.related_labels().iter().map(|(span, message)| {
                    Label::secondary(span.source_id(), span.range()).with_message(message)
                }));
                let mut d = Diagnostic::error()
                    .with_code("resolver")
                    .with_message(cause.to_string())
                    .with_labels(labels);
                if let Some(help) = cause.help_text() {
                    d = d.with_notes(vec![help.to_owned()]);
                }
                d
            })
            .collect(),
        InterpreterError::Compiler { cause } => {
            let span = cause.span();
            vec![
                Diagnostic::error()
                    .with_code("compiler")
                    .with_message(cause.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), span.range())
                            .with_message("related to this"),
                    ]),
            ]
        }
        InterpreterError::Vm(err) => {
            let mut d = Diagnostic::error()
                .with_code("vm")
                .with_message(&err.message);
            if let Some(span) = err.span {
                d = d.with_labels(vec![
                    Label::primary(span.source_id(), span.range()).with_message("related to this"),
                ]);
            }
            vec![d]
        }
    }
}

pub fn emit_error(source_db: &SourceDb, err: InterpreterError) {
    let diagnostics = into_diagnostics(err);
    let files = DiagnosticFiles(source_db);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    for diagnostic in &diagnostics {
        let _ = term::emit_to_write_style(&mut writer.lock(), &config, &files, diagnostic);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use codespan_reporting::diagnostic::LabelStyle;
    use ndc_interpreter::Interpreter;

    #[test]
    fn recursive_assignment_labels_both_operands_and_explains_any() {
        let source = "fn possible() { let ok = false; ok |= possible(); return ok; }";
        let mut interpreter = Interpreter::new();
        interpreter.configure(ndc_stdlib::register);
        let error = interpreter
            .disassemble_str(source)
            .expect_err("recursive call uses Any");
        let diagnostics = into_diagnostics(error);
        assert_eq!(diagnostics.len(), 1);
        let diagnostic = &diagnostics[0];
        assert_eq!(
            diagnostic.message,
            "mismatched types: found Any but expected Bool"
        );
        assert_eq!(diagnostic.labels.len(), 2);
        let primary = &diagnostic.labels[0];
        assert_eq!(primary.style, LabelStyle::Primary);
        assert_eq!(&source[primary.range.clone()], "possible()");
        assert_eq!(primary.message, "right operand inferred as Any");
        let secondary = &diagnostic.labels[1];
        assert_eq!(secondary.style, LabelStyle::Secondary);
        assert_eq!(&source[secondary.range.clone()], "ok");
        assert_eq!(secondary.range.start, source.find("ok |=").unwrap());
        assert!(secondary.message.contains("left operand has type Bool"));
        assert!(secondary.message.contains("`|=`"));
        assert_eq!(primary.file_id, secondary.file_id);
        assert!(diagnostic.notes[0].contains("explicit return type"));

        let mut output = Vec::new();
        term::emit_to_io_write(
            &mut output,
            &term::Config::default(),
            &DiagnosticFiles(interpreter.source_db()),
            diagnostic,
        )
        .expect("render diagnostic");
        let output = String::from_utf8(output).unwrap();
        assert!(output.contains("right operand inferred as Any"));
        assert!(output.contains("left operand has type Bool"));
        assert!(output.contains("Unannotated recursive calls use Any"));

        let annotated = source.replace("fn possible()", "fn possible() -> Bool");
        let mut interpreter = Interpreter::new();
        interpreter.configure(ndc_stdlib::register);
        interpreter
            .disassemble_str(&annotated)
            .expect("annotation resolves the mismatch");
    }

    #[test]
    fn ordinary_type_error_keeps_its_single_label() {
        let source = "let value: Bool = 1;";
        let mut interpreter = Interpreter::new();
        interpreter.configure(ndc_stdlib::register);
        let error = interpreter
            .disassemble_str(source)
            .expect_err("type mismatch");
        let diagnostics = into_diagnostics(error);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].labels.len(), 1);
        assert_eq!(diagnostics[0].labels[0].message, "related to this");
        assert!(diagnostics[0].notes.is_empty());
    }
}
