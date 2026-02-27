#![allow(clippy::print_stdout, clippy::print_stderr)]
use itertools::Itertools;
use miette::highlighters::HighlighterState;
use ndc_lib::interpreter::Interpreter;
use rustyline::Helper;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::CmdKind;
use rustyline::{ColorMode, Completer, Editor, Hinter, Validator};
use std::borrow::Cow;

use crate::highlighter::AndycppHighlighterState;
use crate::into_miette_result;

#[derive(Helper, Completer, Hinter, Validator)]
struct RustlylineHelper {}

impl rustyline::highlight::Highlighter for RustlylineHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut state = AndycppHighlighterState {};
        let out = state.highlight_line(line);

        Cow::Owned(out.into_iter().join(""))
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: CmdKind) -> bool {
        true
    }
}

pub fn run() -> anyhow::Result<()> {
    let h = RustlylineHelper {};

    let mut rl = Editor::new()?;
    rl.set_color_mode(ColorMode::Enabled);
    rl.set_helper(Some(h));

    let stdout = std::io::stdout();
    let mut interpreter = Interpreter::new(stdout);
    loop {
        match rl.readline("λ ") {
            Ok(line) => {
                // If we can't append the history we just ignore this
                let _ = rl.add_history_entry(line.as_str());

                // Run the line we just read through the interpreter
                match into_miette_result(interpreter.run_str(line.as_str())) {
                    Ok(output) => {
                        if !output.is_empty() {
                            println!("{output}")
                        }
                    }
                    Err(report) => {
                        let report = report.with_source_code(line.clone());
                        eprintln!("{report:?}")
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
