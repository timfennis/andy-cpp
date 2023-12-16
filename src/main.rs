use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use clap::Parser;

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
        println!("{}", run(&string));
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
            println!("{}", run(&line));
        }
        println!("Bye!");
    }

    Ok(())
}

fn run(input: &str) -> String {
    String::from("TODO: implement the interpreter")
}


#[cfg(test)]
mod test {
    use clap::CommandFactory;
    use crate::Cli;

    #[test]
    fn test_clap() {
        Cli::command().debug_assert();
    }
}