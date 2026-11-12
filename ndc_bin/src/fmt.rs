//! `ndc fmt`: formats files in place, or standard input to standard output.

use ndc_interpreter::InterpreterError;
use ndc_lexer::SourceDb;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::{fs, process};

/// Formats `files`, or standard input when there are none. With `check`,
/// changes nothing and lists the files that aren't formatted. Exits with 1
/// if any file isn't formatted (with `check`) or can't be.
pub fn run(files: &[PathBuf], check: bool) -> anyhow::Result<()> {
    let mut sources = SourceDb::new();
    let mut ok = true;

    if files.is_empty() {
        let mut source = String::new();
        io::stdin().read_to_string(&mut source)?;
        match format(&mut sources, "<stdin>", &source) {
            Some(formatted) if check => ok = formatted == source,
            Some(formatted) => io::stdout().write_all(formatted.as_bytes())?,
            None => ok = false,
        }
    }

    for path in files {
        let source = fs::read_to_string(path)?;
        let Some(formatted) = format(&mut sources, &path.display().to_string(), &source) else {
            ok = false;
            continue;
        };
        if formatted == source {
            continue;
        }
        if check {
            println!("{}", path.display());
            ok = false;
        } else {
            fs::write(path, &formatted)?;
        }
    }

    if !ok {
        process::exit(1);
    }
    Ok(())
}

/// `source` formatted, or `None` after reporting why it can't be
fn format(sources: &mut SourceDb, name: &str, source: &str) -> Option<String> {
    let id = sources.add(name, source);
    let error: InterpreterError = match ndc_fmt::format(source, id) {
        Ok(formatted) => return Some(formatted),
        Err(ndc_fmt::Error::Lexer(cause)) => cause.into(),
        Err(ndc_fmt::Error::Parser(cause)) => cause.into(),
        Err(error @ ndc_fmt::Error::Changed(_)) => {
            eprintln!(
                "{name}: {error}. This is a bug in the formatter; the file is left as it is."
            );
            return None;
        }
    };
    ndc_diagnostics::emit_error(sources, error);
    None
}
