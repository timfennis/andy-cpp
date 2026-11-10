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
//! shell started stays behind as a supervisor that waits until the last REPL
//! process has exited, so whoever started `ndc` keeps waiting until then. The
//! REPL processes form a process group of their own, which the supervisor
//! forwards termination signals to. In a terminal, that group takes the
//! terminal, and the supervisor takes it back once the REPL has ended.
//! It can't see that group being suspended, so Ctrl-Z doesn't suspend the REPL,
//! and nothing can forward SIGKILL.
#![allow(unsafe_code)]

use std::io::{self, IsTerminal, PipeWriter, Read, Write};
use std::process;
use std::sync::atomic::{AtomicI32, Ordering};

/// The process group of the REPL processes.
static REPL_GROUP: AtomicI32 = AtomicI32::new(0);
/// The last signal the supervisor forwarded to the REPL, or 0.
static FORWARDED: AtomicI32 = AtomicI32::new(0);
/// The signals sent to the `ndc` process that the REPL processes get too.
const FORWARD: [libc::c_int; 4] = [libc::SIGHUP, libc::SIGINT, libc::SIGQUIT, libc::SIGTERM];
/// What the REPL writes to the supervisor when it ends as it should.
const ENDED: u8 = 1;

/// The REPL's side of the supervisor. Every REPL process inherits its pipe end,
/// and the supervisor exits once all of them have closed it.
pub struct Session {
    supervisor: PipeWriter,
}

impl Session {
    /// Tells the supervisor the REPL ended as it should, so `ndc` exits
    /// successfully. Without it the supervisor reports a failure.
    pub fn end(mut self) {
        let _ = self.supervisor.write_all(&[ENDED]);
    }
}

/// Turns this process into the supervisor and returns in the first REPL
/// process.
pub fn start() -> io::Result<Session> {
    let (mut reader, writer) = io::pipe()?;
    // Only take the terminal if it's ours to give: `ndc` runs in the foreground.
    let terminal = io::stdin().is_terminal() && foreground_group() == own_group();
    match fork()? {
        Fork::Child => {
            drop(reader);
            join_own_group(0);
            if terminal {
                // Take the terminal before reading from it: reading it from a
                // background group would stop this process.
                set_disposition(libc::SIGTTOU, Disposition::Ignore);
                give_terminal(own_group());
                set_disposition(libc::SIGTTOU, Disposition::Default);
            }
            set_disposition(libc::SIGTSTP, Disposition::Ignore);
            Ok(Session { supervisor: writer })
        }
        Fork::Parent(repl) => {
            drop(writer);
            // Both sides set the group, so it exists whichever runs first.
            join_own_group(repl);
            REPL_GROUP.store(repl, Ordering::Relaxed);
            for signal in FORWARD {
                set_disposition(signal, Disposition::Forward);
            }
            // The REPL takes the terminal itself; taking it back at the end
            // happens from the background.
            set_disposition(libc::SIGTTOU, Disposition::Ignore);

            // Reads until every REPL process has closed its end.
            let mut messages = Vec::new();
            let _ = reader.read_to_end(&mut messages);
            wait(repl);
            if terminal {
                give_terminal(own_group());
            }

            if messages.contains(&ENDED) {
                process::exit(0);
            }
            // End the way the REPL was ended, so whoever stopped `ndc` sees it.
            let signal = FORWARDED.load(Ordering::Relaxed);
            if signal != 0 {
                set_disposition(signal, Disposition::Default);
                raise(signal);
            }
            process::exit(1)
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
            set_disposition(libc::SIGINT, Disposition::Ignore);
            let mut committed = [0];
            if reader.read(&mut committed)? == 1 {
                process::exit(0);
            }
            wait(child);
            set_disposition(libc::SIGINT, Disposition::Default);
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

#[derive(Clone, Copy)]
enum Disposition {
    Ignore,
    Default,
    /// Forward the signal to the REPL's process group.
    Forward,
}

fn set_disposition(signal: libc::c_int, disposition: Disposition) {
    let handler = match disposition {
        Disposition::Ignore => libc::SIG_IGN,
        Disposition::Default => libc::SIG_DFL,
        Disposition::Forward => forward as extern "C" fn(libc::c_int) as libc::sighandler_t,
    };
    // SAFETY: `forward` only does what a signal handler may do.
    unsafe { libc::signal(signal, handler) };
}

extern "C" fn forward(signal: libc::c_int) {
    FORWARDED.store(signal, Ordering::Relaxed);
    let group = REPL_GROUP.load(Ordering::Relaxed);
    if group > 0 {
        // SAFETY: `kill` may be called from a signal handler.
        unsafe { libc::kill(-group, signal) };
    }
}

/// Makes `process` (0 for this one) the leader of a process group of its own.
fn join_own_group(process: libc::pid_t) {
    // SAFETY: plain system call. It fails harmlessly if the other side of the
    // fork already did it.
    unsafe { libc::setpgid(process, process) };
}

fn own_group() -> libc::pid_t {
    // SAFETY: plain system call without arguments.
    unsafe { libc::getpgrp() }
}

/// The process group that the terminal on stdin delivers input and Ctrl-C to.
fn foreground_group() -> libc::pid_t {
    // SAFETY: plain system call on stdin.
    unsafe { libc::tcgetpgrp(libc::STDIN_FILENO) }
}

fn give_terminal(group: libc::pid_t) {
    // SAFETY: plain system call on stdin.
    unsafe { libc::tcsetpgrp(libc::STDIN_FILENO, group) };
}

fn raise(signal: libc::c_int) {
    // SAFETY: plain system call.
    unsafe { libc::raise(signal) };
}
