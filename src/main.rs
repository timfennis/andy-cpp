#![deny(clippy::all)]
#![warn(clippy::pedantic)]

mod ast;
mod interpreter;
mod lexer;

#[cfg(feature = "repl")]
mod repl;

use crate::ast::Error;
use crate::interpreter::{EvaluationError, Interpreter};
use crate::lexer::Error as LexerError;
use clap::Parser;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::exit;

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
        let mut interpreter = Interpreter::default();
        match interpreter.run_str(&string, cli.debug) {
            // we can just ignore successful runs because we have print statements
            Ok(_final_value) => {}
            Err(err) => {
                eprintln!("{err}");
                exit(1);
            }
        }
    } else {
        #[cfg(feature = "repl")]
        repl::run(cli.debug)?;

        #[cfg(not(feature = "repl"))]
        Err(anyhow::anyhow!("You must supply a filename"))?;
    }
    Ok(())
}

#[derive(Debug)]
enum InterpreterError {
    Lexer { cause: LexerError },
    Parser { cause: Error },
    Evaluation { cause: EvaluationError },
}

impl From<LexerError> for InterpreterError {
    fn from(value: LexerError) -> Self {
        InterpreterError::Lexer { cause: value }
    }
}

impl From<Error> for InterpreterError {
    fn from(value: Error) -> Self {
        InterpreterError::Parser { cause: value }
    }
}

impl From<EvaluationError> for InterpreterError {
    fn from(value: EvaluationError) -> Self {
        InterpreterError::Evaluation { cause: value }
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::Lexer { cause } => write!(f, "Lexer error: {cause}"),
            InterpreterError::Parser { cause } => write!(f, "Parser error: {cause}"),
            InterpreterError::Evaluation { cause } => write!(f, "Evaluation error: {cause}"),
        }
    }
}

impl std::error::Error for InterpreterError {}

#[cfg(test)]
mod test {
    use crate::Cli;
    use clap::CommandFactory;

    #[test]
    fn test_clap() {
        Cli::command().debug_assert();
    }
}
