mod ast;
mod lexer;

use crate::ast::parser::ParserError;
use crate::lexer::{Lexer, LexerError, Token};
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
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    if let Some(path) = cli.file {
        let mut file = File::open(path)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        println!("{}", run(&string)?);
    } else {
        let mut line = String::new();
        loop {
            // print prompt
            print!("λ ");
            std::io::stdout().flush()?;

            // Read a line
            let len = std::io::stdin().read_line(&mut line)?;

            // If the line was empty (from pressing CTRL+D) we quit
            if len == 0 {
                break;
            }

            // Print the response from the interpreter
            match run(&line) {
                Ok(output) => println!("{}", output),
                Err(err) => eprintln!("Error: {}", err),
            }

            line.clear();
        }
        println!("Bye!");
    }

    Ok(())
}

#[derive(Debug)]
enum InterpreterError {
    LexerError { cause: LexerError },
    ParserError { cause: ParserError },
}

fn run(input: &str) -> Result<String, InterpreterError> {
    let scanner = Lexer::from_str(input);
    let tokens = scanner.collect::<Result<Vec<Token>, _>>()?;

    for token in &tokens {
        println!("{:?}", token);
    }

    let mut parser = ast::parser::Parser::from_tokens(tokens);
    let expression = parser.parse()?;

    println!("Expression: {:?}", expression);
    Ok(String::from(""))
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
