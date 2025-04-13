#![allow(clippy::print_stdout, clippy::print_stderr, clippy::exit)]

use anyhow::{Context, anyhow};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process;
use std::{fs, io::Write};

#[cfg(feature = "fancy")]
use highlighter::{AndycppHighlighter, AndycppHighlighterState};
#[cfg(feature = "fancy")]
use highlighters::HighlighterState;

use miette::NamedSource;

use ndc_lib::interpreter::{Interpreter, InterpreterError};

#[cfg(feature = "repl")]
mod repl;

#[cfg(feature = "fancy")]
mod highlighter;

#[derive(Parser)]
#[command(name = "Andy C++")]
#[command(author = "Tim Fennis <fennis.tim@gmail.com>")]
#[command(version = "0.2.0")]
#[command(about = "An interpreter for the Andy C++ language")]
struct Cli {
    #[arg(long)]
    debug: bool,

    #[arg(short = 'C', long, default_value_t = 1)]
    context_lines: usize,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Execute an .ndc file or start the repl (this default action may be omitted)
    Run { file: Option<PathBuf> },
    /// Output an .ndc file using the built-in syntax highlighting engine
    Highlight { file: PathBuf },

    // This is a fallback case
    #[command(external_subcommand)]
    Unknown(Vec<String>),
}

impl Default for Command {
    fn default() -> Self {
        Self::Run { file: None }
    }
}

enum Action {
    RunFile(PathBuf),
    HighlightFile(PathBuf),
    StartRepl,
}

impl TryFrom<Command> for Action {
    type Error = anyhow::Error;

    fn try_from(value: Command) -> Result<Self, Self::Error> {
        let action = match value {
            Command::Run { file: Some(file) } => Self::RunFile(file),
            Command::Run { file: None } => Self::StartRepl,
            Command::Highlight { file } => Self::HighlightFile(file),
            Command::Unknown(args) => {
                match args.len() {
                    0 => {
                        // This case should have defaulted to `Command::Run { file: None }`
                        unreachable!("fallback case reached with 0 arguments (should never happen)")
                    }
                    1 => Self::RunFile(args[0].parse::<PathBuf>().context("invalid path")?),
                    n => return Err(anyhow!("invalid number of arguments: {n}")),
                }
            }
        };
        Ok(action)
    }
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let context_lines = cli.context_lines;

    #[cfg(feature = "fancy")]
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

    let action: Action = cli.command.unwrap_or_default().try_into()?;

    match action {
        Action::RunFile(path) => {
            let filename = path
                .file_name()
                .and_then(|name| name.to_str())
                .map(|string| string.to_string());

            let string = fs::read_to_string(path)?;

            let stdout = std::io::stdout();
            let mut interpreter = Interpreter::new(Box::new(stdout));
            match into_miette_result(interpreter.run_str(&string, cli.debug)) {
                // we can just ignore successful runs because we have print statements
                Ok(_final_value) => {}
                Err(report) => {
                    let source =
                        NamedSource::new(filename.expect("filename must exist"), string.clone());
                    let report = report.with_source_code(source);
                    eprintln!("{:?}", report);

                    process::exit(1);
                }
            }
        }
        Action::HighlightFile(path) => {
            let string = fs::read_to_string(path)?;

            #[cfg(feature = "fancy")]
            {
                let mut highlighter = AndycppHighlighterState {};
                let out = highlighter.highlight_line(&string);
                for styled in out {
                    print!("{}", styled);
                }
                std::io::stdout().flush()?;
            }
            #[cfg(not(feature = "fancy"))]
            println!("{}", string);
        }
        Action::StartRepl => {
            #[cfg(feature = "repl")]
            repl::run(cli.debug)?;
            #[cfg(not(feature = "repl"))]
            {
                eprintln!("there is no repl");
                process::exit(1);
            }
        }
    }
    Ok(())
}

pub fn into_miette_result<T>(result: Result<T, InterpreterError>) -> miette::Result<T> {
    match result {
        Err(err) => Err(err)?,
        Ok(val) => Ok(val),
    }
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
