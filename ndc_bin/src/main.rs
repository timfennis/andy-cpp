#![allow(clippy::print_stdout, clippy::print_stderr, clippy::exit)]

use crate::docs::docs;
use anyhow::{Context, anyhow};
use clap::{Parser, Subcommand};
use highlighter::{AndycppHighlighter, AndycppHighlighterState};
use miette::{NamedSource, highlighters::HighlighterState};
use ndc_lib::interpreter::{Interpreter, InterpreterError};
use std::path::PathBuf;
use std::process;
use std::{fs, io::Write};

mod repl;

mod docs;
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
    Run {
        /// A path to the source to execute
        file: Option<PathBuf>,
        /// The source code to execute
        #[arg(short = 'e')]
        source: Option<String>,
    },
    /// Output an .ndc file using the built-in syntax highlighting engine
    Highlight { file: PathBuf },

    /// Start a language server
    Lsp {
        #[arg(long)]
        stdio: bool,
    },

    /// Output the documentation optionally searched using a query string
    Docs { query: Option<String> },

    // This is a fallback case
    #[command(external_subcommand)]
    Unknown(Vec<String>),
}

impl Default for Command {
    fn default() -> Self {
        Self::Run {
            file: None,
            source: None,
        }
    }
}

enum Action {
    RunLsp,
    RunFile(PathBuf),
    RunSource(String),
    HighlightFile(PathBuf),
    StartRepl,
    Docs(Option<String>),
}

impl TryFrom<Command> for Action {
    type Error = anyhow::Error;

    fn try_from(value: Command) -> Result<Self, Self::Error> {
        let action = match value {
            Command::Run {
                file: Some(file),
                source: None,
            } => Self::RunFile(file),
            Command::Run {
                file: None,
                source: Some(source),
            } => Self::RunSource(source),
            Command::Run {
                file: Some(_),
                source: Some(_),
            } => return Err(anyhow!("not allowed to pass both source and a file")),
            Command::Run {
                file: None,
                source: None,
            } => Self::StartRepl,
            Command::Lsp { stdio: _ } => Self::RunLsp,
            Command::Highlight { file } => Self::HighlightFile(file),
            Command::Docs { query } => Self::Docs(query),
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
            let mut interpreter = Interpreter::new(stdout);
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
        Action::RunSource(source) => {
            let stdout = std::io::stdout();
            let mut interpreter = Interpreter::new(stdout);
            match into_miette_result(interpreter.run_str(&source, cli.debug)) {
                Ok(_final_value) => {}
                Err(report) => {
                    let report = report.with_source_code(source);
                    eprintln!("{:?}", report);

                    process::exit(1);
                }
            }
        }
        Action::HighlightFile(path) => {
            let string = fs::read_to_string(path)?;

            let mut highlighter = AndycppHighlighterState {};
            let out = highlighter.highlight_line(&string);
            for styled in out {
                print!("{}", styled);
            }
            std::io::stdout().flush()?;
        }
        Action::Docs(query) => return docs(query.as_deref()),
        Action::StartRepl => {
            repl::run(cli.debug)?;
        }
        Action::RunLsp => start_lsp(),
    }
    Ok(())
}

fn start_lsp() {
    #[allow(
        clippy::expect_used,
        clippy::diverging_sub_expression,
        clippy::needless_return,
        clippy::unwrap_in_result
    )]
    {
        return tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("Failed building the Runtime")
            .block_on(async { ndc_lsp::start_lsp().await });
    }
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
