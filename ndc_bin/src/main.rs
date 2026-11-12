#![allow(clippy::print_stdout, clippy::print_stderr, clippy::exit)]

use crate::docs::docs;
use anyhow::anyhow;
use clap::{Args, Parser, Subcommand};
use highlighter::AndycppHighlighter;
use ndc_interpreter::Interpreter;
use phase_timing::write_phase_timings;
use std::path::PathBuf;
use std::process;
use std::{fs, io::Write};

mod phase_timing;
mod repl;

mod docs;
mod fmt;
mod highlighter;
#[cfg(unix)]
mod isolation;
#[cfg(feature = "trace")]
mod span_tracer;

#[derive(Parser)]
#[command(name = "Andy C++")]
#[command(author = "Tim Fennis <fennis.tim@gmail.com>")]
#[command(version, long_version = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_HASH"), ")"))]
#[command(about = "An interpreter for the Andy C++ language")]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Execute an .ndc file or start the repl (this default action may be omitted)
    Run {
        file: Option<PathBuf>,
        /// Run SOURCE as the program instead of reading a file
        #[arg(short, long, value_name = "SOURCE", conflicts_with = "file")]
        eval: Option<String>,
        #[command(flatten)]
        options: RunOptions,
    },
    /// Output an .ndc file using the built-in syntax highlighting engine
    Highlight { file: PathBuf },

    /// Start a language server
    Lsp {
        #[arg(long)]
        stdio: bool,
    },

    /// Print the disassembled bytecode for an .ndc file
    Disassemble { file: PathBuf },

    /// Format .ndc files in place, or standard input to standard output
    Fmt {
        files: Vec<PathBuf>,
        /// Change nothing; list the files that aren't formatted and fail if there are any
        #[arg(long)]
        check: bool,
    },

    /// Output the documentation optionally searched using a query string
    Docs {
        query: Option<String>,
        /// Disable color output
        #[arg(long)]
        no_color: bool,
    },

    // This is a fallback case
    #[command(external_subcommand)]
    Unknown(Vec<String>),
}

#[derive(Args, Clone, Copy, Default)]
struct RunOptions {
    /// Print total time spent in each interpreter phase
    #[arg(long)]
    time: bool,
    /// Print each instruction as it is dispatched
    #[cfg(feature = "trace")]
    #[arg(long)]
    trace_print: bool,
    /// Print a histogram of instruction dispatch counts
    #[cfg(feature = "trace")]
    #[arg(long)]
    trace_histogram: bool,
    /// Print cumulative time spent per instruction type
    #[cfg(feature = "trace")]
    #[arg(long)]
    trace_time: bool,
    /// Render source as a heat map colored by time spent per span
    #[cfg(feature = "trace")]
    #[arg(long)]
    trace_span: bool,
}

#[derive(Parser)]
#[command(name = "ndc", disable_help_subcommand = true)]
struct ImplicitRunArgs {
    file: PathBuf,
    #[command(flatten)]
    options: RunOptions,
}

impl Default for Command {
    fn default() -> Self {
        Self::Run {
            file: None,
            eval: None,
            options: RunOptions::default(),
        }
    }
}

/// Where the program to run comes from.
#[derive(Debug, PartialEq, Eq)]
enum Program {
    File(PathBuf),
    /// Source passed on the command line with `--eval`
    Eval(String),
}

enum Action {
    RunLsp,
    Run {
        program: Program,
        options: RunOptions,
    },
    DisassembleFile(PathBuf),
    Format {
        files: Vec<PathBuf>,
        check: bool,
    },
    HighlightFile(PathBuf),
    StartRepl,
    Docs {
        query: Option<String>,
        no_color: bool,
    },
}

impl TryFrom<Command> for Action {
    type Error = anyhow::Error;

    fn try_from(value: Command) -> Result<Self, Self::Error> {
        let action = match value {
            Command::Run {
                file: Some(file),
                options,
                ..
            } => Self::Run {
                program: Program::File(file),
                options,
            },
            Command::Run {
                eval: Some(source),
                options,
                ..
            } => Self::Run {
                program: Program::Eval(source),
                options,
            },
            Command::Run { .. } => Self::StartRepl,
            Command::Lsp { stdio: _ } => Self::RunLsp,
            Command::Disassemble { file } => Self::DisassembleFile(file),
            Command::Fmt { files, check } => Self::Format { files, check },
            Command::Highlight { file } => Self::HighlightFile(file),
            Command::Docs { query, no_color } => Self::Docs { query, no_color },
            Command::Unknown(args) => {
                let implicit_run = ImplicitRunArgs::try_parse_from(
                    std::iter::once("ndc").chain(args.iter().map(String::as_str)),
                )
                .map_err(|err| anyhow!(err.render().to_string()))?;

                Self::Run {
                    program: Program::File(implicit_run.file),
                    options: implicit_run.options,
                }
            }
        };
        Ok(action)
    }
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let action: Action = cli.command.unwrap_or_default().try_into()?;

    match action {
        Action::Run { program, options } => {
            let (name, string) = match program {
                Program::File(path) => {
                    let name = path
                        .file_name()
                        .and_then(|name| name.to_str())
                        .unwrap_or("<input>")
                        .to_string();
                    (name, fs::read_to_string(path)?)
                }
                Program::Eval(source) => ("<eval>".to_string(), source),
            };

            let mut interpreter = Interpreter::one_shot();
            interpreter.configure(ndc_stdlib::register);

            #[cfg(feature = "trace")]
            {
                use ndc_interpreter::tracer;
                let mut tracers: Vec<Box<dyn tracer::VmTracer>> = Vec::new();
                if options.trace_print {
                    tracers.push(Box::new(tracer::PrintTracer));
                }
                if options.trace_histogram {
                    tracers.push(Box::new(tracer::HistogramTracer::new()));
                }
                if options.trace_time {
                    tracers.push(Box::new(tracer::TimingTracer::new()));
                }
                if options.trace_span {
                    tracers.push(Box::new(span_tracer::SpanTracer::new()));
                }
                if !tracers.is_empty() {
                    interpreter.set_tracer(Box::new(tracer::CompositeTracer::new(tracers)));
                }
            }

            if options.time {
                match interpreter.eval_named_with_timings(name, &string) {
                    Ok((_, timings)) => {
                        write_phase_timings(&mut std::io::stderr(), &timings)?;
                    }
                    Err(err) => {
                        ndc_diagnostics::emit_error(interpreter.source_db(), err);
                        process::exit(1);
                    }
                }
            } else if let Err(err) = interpreter.eval_named(name, &string) {
                ndc_diagnostics::emit_error(interpreter.source_db(), err);
                process::exit(1);
            }
        }
        Action::DisassembleFile(path) => {
            let string = fs::read_to_string(path)?;
            let mut interpreter = Interpreter::new();
            interpreter.configure(ndc_stdlib::register);
            match interpreter.disassemble_str(&string) {
                Ok(output) => print!("{output}"),
                Err(e) => {
                    ndc_diagnostics::emit_error(interpreter.source_db(), e);
                    process::exit(1);
                }
            }
        }
        Action::Format { files, check } => fmt::run(&files, check)?,
        Action::HighlightFile(path) => {
            let string = fs::read_to_string(path)?;

            let out = AndycppHighlighter::highlight_parsed(&string);
            for styled in out {
                print!("{}", styled);
            }
            std::io::stdout().flush()?;
        }
        Action::Docs { query, no_color } => return docs(query.as_deref(), no_color),
        Action::StartRepl => {
            repl::run()?;
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
            .block_on(async { ndc_lsp::start_lsp(ndc_stdlib::register).await });
    }
}

#[cfg(test)]
mod test {
    use clap::CommandFactory;
    use std::path::PathBuf;

    use clap::Parser;

    use crate::{Action, Cli, Command, Program};

    fn parse(args: &[&str]) -> Result<Action, String> {
        let cli = Cli::try_parse_from(args).map_err(|err| err.to_string())?;
        cli.command
            .unwrap_or_default()
            .try_into()
            .map_err(|err: anyhow::Error| err.to_string())
    }

    #[test]
    fn test_clap() {
        Cli::command().debug_assert();
    }

    #[test]
    fn implicit_run_honors_time_flag() {
        let action = Action::try_from(Command::Unknown(vec![
            "script.ndc".to_string(),
            "--time".to_string(),
        ]))
        .expect("implicit run flags should parse");

        match action {
            Action::Run { program, options } => {
                assert_eq!(program, Program::File(PathBuf::from("script.ndc")));
                assert!(options.time);
            }
            _ => panic!("expected run action"),
        }
    }

    #[test]
    fn run_eval_runs_the_given_source() {
        for flag in ["--eval", "-e"] {
            match parse(&["ndc", "run", flag, "print(1);", "--time"]) {
                Ok(Action::Run { program, options }) => {
                    assert_eq!(program, Program::Eval("print(1);".to_string()));
                    assert!(options.time);
                }
                _ => panic!("expected run action for {flag}"),
            }
        }
    }

    #[test]
    fn run_eval_conflicts_with_a_file() {
        let err = parse(&["ndc", "run", "script.ndc", "--eval", "print(1);"])
            .err()
            .expect("a file and --eval together should be rejected");
        assert!(err.contains("cannot be used with"), "{err}");
    }
}
