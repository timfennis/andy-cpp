//! Runs each REPL line in a forked copy of the REPL, so a line that fails has
//! no effect on the session: not on its variables, not on the heap, not on
//! what the analyser knows. Only what the line printed or wrote to files stays.
//!
//! The process that reads a line forks before running it. The child runs the
//! line; the parent waits, holding the session as it was before the line. If
//! the line succeeds the child tells the parent, which exits, and the child
//! carries on as the REPL. If the child dies any other way (an error, a panic,
//! a stack overflow, or Ctrl-C while the line runs) the parent carries on.
//!
//! Since the REPL changes process with every successful line, the process the
//! shell started stays behind as a supervisor that only waits until the last
//! REPL process has exited, so the shell keeps the terminal away until then.
#![allow(unsafe_code)]

use std::io::{self, PipeWriter, Read, Write};
use std::process;

/// Turns this process into the supervisor and returns in the first REPL
/// process. The returned pipe end must stay open for as long as the REPL runs:
/// every REPL process inherits it, and the supervisor exits once all of them
/// have closed it.
pub fn start() -> io::Result<PipeWriter> {
    let (mut reader, writer) = io::pipe()?;
    match fork()? {
        Fork::Child => Ok(writer),
        Fork::Parent(child) => {
            drop(writer);
            ignore_interrupts(true);
            // Reads until every REPL process has closed its end.
            let _ = reader.read_to_end(&mut Vec::new());
            wait(child);
            process::exit(0)
        }
    }
}

/// Runs `line` in a forked child, keeping its effects only if it returns
/// `true`. Returns in whichever process carries on as the REPL.
pub fn run(line: impl FnOnce() -> bool) -> io::Result<()> {
    // Anything still buffered would be written twice, once by each process.
    io::stdout().flush()?;
    io::stderr().flush()?;

    let (mut reader, mut writer) = io::pipe()?;
    match fork()? {
        Fork::Child => {
            drop(reader);
            if !line() {
                let _ = io::stdout().flush();
                process::exit(1);
            }
            io::stdout().flush()?;
            // Tells the parent to exit: this process is the REPL from now on.
            writer.write_all(&[1])?;
            Ok(())
        }
        Fork::Parent(child) => {
            drop(writer);
            // Ctrl-C while the line runs stops the child only.
            ignore_interrupts(true);
            let mut committed = [0];
            if reader.read(&mut committed)? == 1 {
                process::exit(0);
            }
            wait(child);
            ignore_interrupts(false);
            Ok(())
        }
    }
}

enum Fork {
    Parent(libc::pid_t),
    Child,
}

fn fork() -> io::Result<Fork> {
    // SAFETY: the REPL is single-threaded, so the child is a complete copy of
    // the process and may run any code.
    match unsafe { libc::fork() } {
        -1 => Err(io::Error::last_os_error()),
        0 => Ok(Fork::Child),
        child => Ok(Fork::Parent(child)),
    }
}

/// Reaps `child` once it has exited.
fn wait(child: libc::pid_t) {
    // SAFETY: a null status pointer is allowed; nothing is written through it.
    unsafe { libc::waitpid(child, std::ptr::null_mut(), 0) };
}

fn ignore_interrupts(ignore: bool) {
    let handler = if ignore { libc::SIG_IGN } else { libc::SIG_DFL };
    // SAFETY: setting a signal's disposition to ignore or default installs no
    // handler of our own.
    unsafe { libc::signal(libc::SIGINT, handler) };
}
