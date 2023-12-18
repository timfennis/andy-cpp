mod ast;
mod interpreter;
mod lexer;

#[cfg(feature = "repl")]
mod repl;

use crate::ast::parser::ParserError;
use crate::interpreter::{Evaluate, EvaluationError};
use crate::lexer::{Lexer, LexerError, Token};
use clap::Parser;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::Read;
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
        repl::run_repl(cli.debug)?;

        #[cfg(not(feature = "repl"))]
        Err(anyhow::anyhow!("You must supply a filename"))?;
    }
    Ok(())
}

#[derive(Debug)]
enum InterpreterError {
    LexerError { cause: LexerError },
    ParserError { cause: ParserError },
    EvaluationError { cause: EvaluationError },
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
    Ok(format!("{}", expression.evaluate()?))
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

impl From<EvaluationError> for InterpreterError {
    fn from(value: EvaluationError) -> Self {
        InterpreterError::EvaluationError { cause: value }
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::LexerError { cause } => write!(f, "Lexer error: {cause}"),
            InterpreterError::ParserError { cause } => write!(f, "Parser error: {cause}"),
            InterpreterError::EvaluationError { cause } => write!(f, "Evaluation error: {cause}")
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
