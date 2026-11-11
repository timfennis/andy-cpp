#![allow(clippy::print_stdout, clippy::print_stderr)]
use itertools::Itertools;
use ndc_interpreter::{Interpreter, InterpreterError};
use rustyline::Helper;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::CmdKind;
use rustyline::{ColorMode, Completer, Editor, Hinter, Validator};
use std::borrow::Cow;

use crate::highlighter::AndycppHighlighter;

#[derive(Helper, Completer, Hinter, Validator)]
struct RustylineHelper {}

impl rustyline::highlight::Highlighter for RustylineHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let out = AndycppHighlighter::highlight_parsed(line);

        Cow::Owned(out.into_iter().join(""))
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: CmdKind) -> bool {
        true
    }
}

fn new_interpreter() -> Interpreter {
    let mut interpreter = Interpreter::new();
    interpreter.configure(ndc_stdlib::register);
    interpreter
}

enum Outcome {
    Succeeded,
    /// The line didn't parse, so it never ran.
    Rejected,
    Failed,
}

/// Runs one line and prints its value or its error.
fn eval_and_print(interpreter: &mut Interpreter, name: String, line: &str) -> Outcome {
    match interpreter.eval_named(name, line) {
        Ok(value) => {
            let output = value.to_string();
            if !output.is_empty() {
                println!("{output}")
            }
            Outcome::Succeeded
        }
        Err(err) => {
            let outcome = match err {
                InterpreterError::Lexer { .. } | InterpreterError::Parser { .. } => {
                    Outcome::Rejected
                }
                _ => Outcome::Failed,
            };
            ndc_diagnostics::emit_error(interpreter.source_db(), err);
            outcome
        }
    }
}

/// Runs a line where fork isn't available, in this process: after a line that
/// fails the interpreter starts the session over. Compiled everywhere, so it
/// can't break unnoticed on the platforms that use it.
#[cfg_attr(unix, allow(dead_code))]
fn run_in_process(interpreter: &mut Interpreter, name: String, line: &str) {
    if let Outcome::Failed = eval_and_print(interpreter, name, line) {
        println!("The session started over, as the line failed.");
    }
}

pub fn run() -> anyhow::Result<()> {
    #[cfg(unix)]
    let session = crate::isolation::start()?;

    let h = RustylineHelper {};

    let mut rl = Editor::new()?;
    rl.set_color_mode(ColorMode::Enabled);
    rl.set_helper(Some(h));

    let mut interpreter = new_interpreter();
    for command_nr in 1.. {
        match rl.readline("λ ") {
            Ok(line) => {
                // If we can't append the history we just ignore this
                let _ = rl.add_history_entry(line.as_str());
                let name = format!("<repl:{command_nr}>");

                // A line that fails leaves no trace in the session.
                #[cfg(unix)]
                crate::isolation::run(|| {
                    matches!(
                        eval_and_print(&mut interpreter, name, &line),
                        Outcome::Succeeded
                    )
                })?;

                #[cfg(not(unix))]
                run_in_process(&mut interpreter, name, &line);
            }
            Err(ReadlineError::Interrupted) => {
                // Do nothing!!?
            }
            Err(ReadlineError::Eof) => {
                // User wants to exit the REPL
                println!("Bye!");
                break;
            }
            Err(err) => {
                eprintln!("{err}");
                break;
            }
        }
    }

    #[cfg(unix)]
    session.end();
    Ok(())
}
