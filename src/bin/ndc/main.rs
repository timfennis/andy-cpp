use std::io::Read;
use std::path::PathBuf;
use std::process::exit;
use std::{fs::File, io::Write};

use clap::Parser;

use highlighter::{AndycppHighlighter, AndycppHighlighterState};
use miette::{highlighters::HighlighterState, NamedSource};
use ndc_lib::interpreter::{Interpreter, InterpreterError};

#[cfg(feature = "repl")]
mod repl;

mod highlighter;

#[derive(Parser)]
#[command(name = "Andy C++")]
#[command(author = "Tim Fennis <fennis.tim@gmail.com>")]
#[command(version = "0.1")]
#[command(about = "An interpreter for the Andy C++ language")]
struct Cli {
    file: Option<PathBuf>,

    #[arg(long)]
    debug: bool,

    #[arg(long)]
    highlight: bool,

    #[arg(short = 'C', long, default_value_t = 1)]
    context_lines: usize,
}

pub fn miette_hack<T>(result: Result<T, InterpreterError>) -> miette::Result<T> {
    match result {
        Err(err) => Err(err)?,
        Ok(val) => Ok(val),
    }
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let context_lines = cli.context_lines;

    miette::set_hook(Box::new(move |_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .color(true)
                .unicode(true)
                .context_lines(context_lines)
                .with_syntax_highlighting(AndycppHighlighter::default())
                .build(),
        )
    }))?;

    if let Some(path) = cli.file {
        // Create a copy of the filename for error reporting later
        let filename = path
            .file_name()
            .and_then(|name| name.to_str())
            .map(|string| string.to_string());

        let mut file = File::open(path)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;

        if cli.highlight {
            let mut foo = AndycppHighlighterState {};
            let out = foo.highlight_line(&string);
            for styled in out {
                print!("{}", styled);
            }
            std::io::stdout().flush()?;

            exit(0);
        }

        let stdout = std::io::stdout();
        let mut interpreter = Interpreter::new(Box::new(stdout));
        match miette_hack(interpreter.run_str(&string, cli.debug)) {
            // we can just ignore successful runs because we have print statements
            Ok(_final_value) => {}
            Err(report) => {
                let source =
                    NamedSource::new(filename.expect("filename must exist"), string.to_string());
                let report = report.with_source_code(source);
                eprintln!("{:?}", report);
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
    use clap::CommandFactory;

    use crate::Cli;

    #[test]
    fn test_clap() {
        Cli::command().debug_assert();
    }
}
