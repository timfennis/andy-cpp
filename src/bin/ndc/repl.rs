use colored::{Color, ColoredString, Colorize};
use itertools::Itertools;
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

const NUMBER_LITERAL_COLOR: Color = Color::TrueColor {
    r: 253,
    g: 151,
    b: 31,
};

const PARENTHESES_COLOR: Color = Color::TrueColor {
    r: 229,
    g: 181,
    b: 103,
};

const STRING_LITERAL_COLOR: Color = Color::BrightGreen;
const IDENTIFIER_COLOR: Color = Color::BrightCyan;

impl rustyline::highlight::Highlighter for RustlylineHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let Ok(tokens) = Lexer::new(line).collect::<Result<Vec<_>, _>>() else {
            return Cow::Owned(line.red().to_string());
        };

        if tokens.is_empty() {
            return Cow::Owned(line.red().to_string());
        }

        let mut start = 0;
        let mut ranges = vec![];
        for token in tokens.iter().dropping(1) {
            let end = token.location.column - 1;
            ranges.push(start..end);
            start = end;
        }
        if !line.is_empty() {
            ranges.push(start..line.len());
        }

        let mut buf = String::new();
        for (range, token) in ranges.into_iter().zip(tokens.into_iter()) {
            let sub = &line[range];

            let colored = match &token.token {
                Token::String(_) => sub.color(STRING_LITERAL_COLOR),
                Token::BigInt(_) | Token::Int64(_) | Token::Float64(_) | Token::Complex(_) => {
                    sub.color(NUMBER_LITERAL_COLOR)
                }
                Token::True | Token::False => sub.color(NUMBER_LITERAL_COLOR),
                Token::LeftSquareBracket
                | Token::RightSquareBracket
                | Token::LeftCurlyBracket
                | Token::RightCurlyBracket
                | Token::LeftParentheses
                | Token::RightParentheses
                | Token::MapOpen => sub.color(PARENTHESES_COLOR),
                Token::Identifier(_) => sub.color(IDENTIFIER_COLOR),
                _ => sub.bright_blue().bold(), // BOLD
            };

            buf.push_str(&colored.to_string());
        }

        Cow::Owned(buf)
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
