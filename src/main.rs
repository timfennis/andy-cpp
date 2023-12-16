mod scanner;

use clap::Parser;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use crate::scanner::{Scanner, ScannerError};

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
            let result = run(&line);
            match run(&line) {
                Ok(output) => println!("{}", output),
                Err(err) => eprintln!("Error: {}", err),
            }
        }
        println!("Bye!");
    }

    Ok(())
}

#[derive(Debug)]
enum InterpreterError {
    ScannerError { cause: ScannerError },
}

fn run(input: &str) -> Result<String, InterpreterError> {
    let scanner = Scanner::from_str(input);
    for token in scanner {
        let token = token.map_err(|error| InterpreterError::ScannerError { cause: error })?;
        println!("{:?}", token);

    }

    Ok(String::from(""))
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::ScannerError { cause } => write!(f, "Scanner error: {cause}"),
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
