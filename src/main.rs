mod ast;
mod interpreter;
mod lexer;

use crate::ast::parser::ParserError;
use crate::interpreter::Evaluate;
use crate::lexer::{Lexer, LexerError, Token};
use anyhow::{anyhow, Context};
use clap::Parser;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "Andy C++")]
#[command(author = "Tim Fennis <fennis.tim@gmail.com>")]
#[command(version = "0.1")]
#[command(about = "An interpreter for the Andy C++ language")]
struct Cli {
    file: Option<PathBuf>,
    #[arg(long)]
    debug: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    if let Some(path) = cli.file {
        let mut file = File::open(path)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        println!("{}", run(&string, cli.debug)?);
    } else {
        #[cfg(feature = "repl")]
        {
            crate::run_repl(cli.debug)?;
            return Ok(());
        }

        Err(anyhow!("You must supply a filename"))?;
    }
    Ok(())
}

#[cfg(feature = "repl")]
fn run_repl(debug: bool) -> anyhow::Result<()> {
    use rustyline::error::ReadlineError;
    use rustyline::{Config, DefaultEditor};

    let mut rl = DefaultEditor::new()?;
    // let mut rl = DefaultEditor::with_config(Config::builder().build())?;

    loop {
        match rl.readline("λ ") {
            Ok(line) => {
                // If we can't append the history we just ignore this
                let _ = rl.add_history_entry(line.as_str());

                // Run the line we just read through the interpreter
                match run(line.as_str(), debug) {
                    Ok(output) => println!("{}", output),
                    Err(err) => eprintln!("{}", err),
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                // User wants to exit the REPL
                println!("Bye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {err}");
                break;
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
enum InterpreterError {
    LexerError { cause: LexerError },
    ParserError { cause: ParserError },
}

fn run(input: &str, debug: bool) -> Result<String, InterpreterError> {
    let scanner = Lexer::from_str(input);
    let tokens = scanner.collect::<Result<Vec<Token>, _>>()?;

    if debug {
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    let mut parser = ast::Parser::from_tokens(tokens);
    let expression = parser.parse()?;

    if debug {
        println!("Expression: {:?}", expression);
        println!("Result: {:?}", expression.evaluate());
    }
    Ok(format!("{}", expression.evaluate()))
}

impl From<LexerError> for InterpreterError {
    fn from(value: LexerError) -> Self {
        InterpreterError::LexerError { cause: value }
    }
}

impl From<ParserError> for InterpreterError {
    fn from(value: ParserError) -> Self {
        InterpreterError::ParserError { cause: value }
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::LexerError { cause } => write!(f, "Lexer error: {cause}"),
            InterpreterError::ParserError { cause } => write!(f, "Parser error: {cause}"),
        }
    }
}

impl Error for InterpreterError {}

#[cfg(test)]
mod test {
    use crate::Cli;
    use clap::CommandFactory;

    #[test]
    fn test_clap() {
        Cli::command().debug_assert();
    }
}
