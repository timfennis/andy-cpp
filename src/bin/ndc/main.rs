use clap::Parser;
use ndc_lib::interpreter::Interpreter;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::exit;

mod repl;

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

#[cfg(test)]
mod test {
    use crate::Cli;
    use clap::CommandFactory;

    #[test]
    fn test_clap() {
        Cli::command().debug_assert();
    }
}
