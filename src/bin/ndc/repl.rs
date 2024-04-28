use colored::Colorize;
use ndc_lib::interpreter::Interpreter;
use ndc_lib::lexer::{Lexer, Token};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::Helper;
use rustyline::{ColorMode, Completer, Editor, Hinter, Validator};
use std::borrow::Cow;
use std::fmt::Write as _;

#[derive(Helper, Completer, Hinter, Validator)]
struct RustlylineHelper {}

impl rustyline::highlight::Highlighter for RustlylineHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let lexer = Lexer::new(line);
        let mut it = line.chars().enumerate().peekable();

        if let Ok(tokens) = lexer.collect::<Result<Vec<_>, _>>() {
            let mut out = String::new();
            for token in tokens {
                while it
                    .peek()
                    .map_or(false, |(i, _)| i < &(token.location.column - 1))
                {
                    out.push(it.next().unwrap().1);
                }
                // dbg!(&token);

                let colored_token = match &token.token {
                    Token::String(s) => format!("{s:?}").bright_green(),
                    Token::BigInt(n) => format!("{n}").truecolor(253, 151, 31),
                    Token::Int64(n) => format!("{n}").truecolor(253, 151, 31),
                    Token::Float64(f) => {
                        let mut buffer = ryu::Buffer::new();
                        buffer.format(*f).to_string().truecolor(253, 151, 31)
                    }
                    // TODO: rendering of this token is completely broken, probably because this token should be multiple tokens instead
                    Token::Complex(c) => format!("{c}").truecolor(253, 151, 31),
                    t @ (Token::True | Token::False) => format!("{t}").truecolor(253, 151, 31),
                    t @ (Token::LeftSquareBracket
                    | Token::RightSquareBracket
                    | Token::LeftCurlyBracket
                    | Token::RightCurlyBracket
                    | Token::LeftParentheses
                    | Token::RightParentheses
                    | Token::MapOpen) => format!("{t}").truecolor(229, 181, 103),
                    Token::Identifier(ident) => ident.bright_cyan(),
                    t => format!("{t:?}").bright_blue().bold(),
                };

                // println!("{} {}", colored_token, colored_token.len());

                let skip = colored_token.len();
                write!(out, "{}", colored_token).expect("write must succeed");

                for _ in 0..skip {
                    it.next();
                }
            }
            Cow::Owned(out)
        } else {
            // There is probably an easier way to do this?
            Cow::Owned(line.red().to_string())
        }
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
                match interpreter.run_str(line.as_str(), debug) {
                    Ok(output) => {
                        if !output.is_empty() {
                            println!("{output}")
                        }
                    }
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
