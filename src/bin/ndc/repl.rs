use ndc_lib::interpreter::Interpreter;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::{ColorMode, Completer, Editor, Hinter, Validator};
use rustyline::{Helper, Highlighter};

#[derive(Helper, Completer, Highlighter, Hinter, Validator)]
struct RustlylineHelper {
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
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

    let h = RustlylineHelper {
        highlighter: MatchingBracketHighlighter::new(),
    };

    let mut rl = Editor::new()?;
    rl.set_color_mode(ColorMode::Enabled);
    rl.set_helper(Some(h));
    // let mut rl = DefaultEditor::with_config(Config::builder().build())?;

    let mut interpreter = Interpreter::default();
    loop {
        match rl.readline("λ ") {
            Ok(line) => {
                // If we can't append the history we just ignore this
                let _ = rl.add_history_entry(line.as_str());

                // Run the line we just read through the interpreter
                match interpreter.run_str(line.as_str(), debug) {
                    Ok(output) => println!("{output}"),
                    Err(err) => eprintln!("{err}"),
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
