#![allow(clippy::print_stdout, clippy::print_stderr)]
use itertools::Itertools;
use ndc_interpreter::Interpreter;
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
        let out = AndycppHighlighter::highlight_line(line);

        Cow::Owned(out.into_iter().join(""))
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: CmdKind) -> bool {
        true
    }
}

pub fn run() -> anyhow::Result<()> {
    let h = RustylineHelper {};

    let mut rl = Editor::new()?;
    rl.set_color_mode(ColorMode::Enabled);
    rl.set_helper(Some(h));

    let mut interpreter = Interpreter::new();
    interpreter.configure(ndc_stdlib::register);
    loop {
        match rl.readline("λ ") {
            Ok(line) => {
                // If we can't append the history we just ignore this
                let _ = rl.add_history_entry(line.as_str());

                // Run the line we just read through the interpreter
                match interpreter.eval(line.as_str()) {
                    Ok(value) => {
                        let output = value.to_string();
                        if !output.is_empty() {
                            println!("{output}")
                        }
                    }
                    Err(err) => {
                        crate::diagnostic::emit_error("<repl>", &line, err);
                    }
                }
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

    Ok(())
}
