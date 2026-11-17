//! Where the VM's I/O built-ins write to and read from.

use crate::error::VmError;
use std::io::{IsTerminal, Read, Write};

/// Output destination for the VM's I/O built-ins (`print`, `dbg`, etc.).
pub enum OutputSink {
    Stdout,
    Buffer(Vec<u8>),
}

impl Write for OutputSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::Stdout => std::io::stdout().write(buf),
            Self::Buffer(vec) => vec.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::Stdout => std::io::stdout().flush(),
            Self::Buffer(vec) => vec.flush(),
        }
    }
}

/// Where `read_stdin` reads from.
pub enum InputSource {
    /// The process's standard input.
    Stdin,
    /// Input given up front, as a test gives it.
    Buffer(String),
    /// No input at all, as in the REPL, whose standard input is the prompt.
    None,
}

impl InputSource {
    /// Everything left to read, which leaves nothing for the next read.
    // The hint for a terminal goes to stderr, keeping stdout for the program.
    #[allow(clippy::print_stderr)]
    pub(crate) fn read_to_end(&mut self) -> Result<String, VmError> {
        let input = match self {
            Self::Stdin => {
                let stdin = std::io::stdin();
                if stdin.is_terminal() {
                    eprintln!("{TERMINAL_HINT}");
                }
                let mut input = String::new();
                stdin.lock().read_to_string(&mut input).map_err(|err| {
                    VmError::native(match err.kind() {
                        std::io::ErrorKind::InvalidData => {
                            "standard input isn't valid UTF-8".to_string()
                        }
                        _ => format!("failed to read standard input: {err}"),
                    })
                })?;
                input
            }
            Self::Buffer(input) => std::mem::take(input),
            Self::None => return Err(VmError::native("there is no standard input to read here")),
        };
        *self = Self::Buffer(String::new());
        Ok(input)
    }
}

/// What a program reading standard input from a terminal says first, as it
/// would otherwise seem to hang: run without `< input.txt`, it waits for
/// input that has to be typed.
const TERMINAL_HINT: &str = if cfg!(windows) {
    "reading input from the terminal: type it and end it with Ctrl-Z and Enter, or run with `< input.txt`"
} else {
    "reading input from the terminal: type it and end it with Ctrl-D, or run with `< input.txt`"
};
