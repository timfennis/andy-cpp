use itertools::Itertools;
use miette::highlighters::HighlighterState;
use ndc_lib::interpreter::Interpreter;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::Helper;
use rustyline::{ColorMode, Completer, Editor, Hinter, Validator};
use std::borrow::Cow;

use crate::highlighter::AndycppHighlighterState;
use crate::miette_hack;

#[derive(Helper, Completer, Hinter, Validator)]
struct RustlylineHelper {}

impl rustyline::highlight::Highlighter for RustlylineHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut state = AndycppHighlighterState {};
        let out = state.highlight_line(line);

        Cow::Owned(out.into_iter().join(""))
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: bool) -> bool {
        true
    }
}

pub fn run(debug: bool) -> anyhow::Result<()> {
    // impl Validator for Foobar {
    //     fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
    //         let input = ctx.input();
    //         let lexer = Lexer::new(input);
    //         let result = lexer.collect::<Result<Vec<Token>, _>>();
    //
    //         match result {
    //             Err(LexerError::UnexpectedCharacter { char, .. }) => Ok(ValidationResult::Invalid(
    //                 Some(format!("          Unexpected character: {char}").to_string()),
    //             )),
    //             Err(LexerError::UnterminatedString { })
    //             _ => Ok(ValidationResult::Valid(None)),
    //         }
    //     }
    // }

    let h = RustlylineHelper {};

    let mut rl = Editor::new()?;
    rl.set_color_mode(ColorMode::Enabled);
    rl.set_helper(Some(h));
    // let mut rl = DefaultEditor::with_config(Config::builder().build())?;

    let stdout = std::io::stdout();
    let mut interpreter = Interpreter::new(Box::new(stdout));
    loop {
        match rl.readline("λ ") {
            Ok(line) => {
                // If we can't append the history we just ignore this
                let _ = rl.add_history_entry(line.as_str());

                // Run the line we just read through the interpreter
                match miette_hack(interpreter.run_str(line.as_str(), debug)) {
                    Ok(output) => {
                        if !output.is_empty() {
                            println!("{output}")
                        }
                    }
                    Err(report) => {
                        let report = report.with_source_code(line.to_string());
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
