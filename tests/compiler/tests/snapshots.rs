//! Checks what each compiler writes for each program under `programs`
//! against the listing expected of it, kept next to the program: for the
//! stack backend, `programs/blocks/a_block_yields_its_last_expression.ndc` is
//! expected to compile to `a_block_yields_its_last_expression.stack.txt`, and
//! for the register backend to `a_block_yields_its_last_expression.register.txt`.
//!
//! A program says how to compile it in a directive, one line such as
//! `// compile: stdlib optimized`, which may list several modes separated by
//! commas; the listing then has a section per mode. The register compiler
//! has no peephole optimizer, so its listing has one section for a setup
//! with and without `optimized`, titled without it.
//!
//! `BLESS=1 cargo test -p compiler_tests` writes what the compiler gives into
//! the expected files instead of comparing.

use ndc_analysis::Analysis;
use ndc_interpreter::Interpreter;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_vm::Natives;
use ndc_vm::disassemble::disassemble;
use ndc_vm::register::Compiler as RegisterCompiler;
use std::fmt::{self, Write};
use std::fs;
use std::path::{Path, PathBuf};

include!(concat!(env!("OUT_DIR"), "/snapshot_tests.rs"));

/// A compiler the snapshots check: its name and how it lists a program.
struct Backend {
    /// The name the backend's expected files carry: `stack` for
    /// `program.stack.txt`.
    name: &'static str,
    /// The listing of a program's code compiled in a mode.
    listing: fn(&str, Mode) -> String,
    /// Whether the backend has a peephole optimizer, which `optimized` runs.
    optimizes: bool,
}

/// Every backend, each of which has an expected listing for every program.
const BACKENDS: &[Backend] = &[Backend {
    name: "register",
    listing: register_listing,
    optimizes: false,
}];

/// How a program is compiled: `analysed`, or `stdlib optimized`.
#[derive(Clone, Copy, PartialEq, Eq)]
struct Mode {
    setup: Setup,
    /// Whether the peephole optimizer runs.
    optimized: bool,
}

/// What the compiler knows about a program.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Setup {
    /// `parsed`: nothing, as nothing analysed it and there are no natives.
    Parsed,
    /// `analysed`: what the analyser works out, with no natives.
    Analysed,
    /// `stdlib`: what the analyser works out, with the standard library.
    Stdlib,
}

impl Mode {
    /// The mode a directive names: `analysed` or `stdlib optimized`.
    fn parse(text: &str) -> Result<Self, String> {
        let mut words = text.split_whitespace();
        let setup = match words.next() {
            Some("parsed") => Setup::Parsed,
            Some("analysed") => Setup::Analysed,
            Some("stdlib") => Setup::Stdlib,
            _ => {
                return Err(format!(
                    "`{text}` names no setup: parsed, analysed or stdlib"
                ));
            }
        };
        let optimized = match words.next() {
            None => false,
            Some("optimized") => true,
            Some(word) => return Err(format!("`{word}` in `{text}` isn't `optimized`")),
        };
        if words.next().is_some() {
            return Err(format!("`{text}` has more than a setup and `optimized`"));
        }
        Ok(Self { setup, optimized })
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self.setup {
            Setup::Parsed => "parsed",
            Setup::Analysed => "analysed",
            Setup::Stdlib => "stdlib",
        })?;
        if self.optimized {
            f.write_str(" optimized")?;
        }
        Ok(())
    }
}

/// The register VM's code for `source`, as `ndc disassemble` lists it with
/// `NDC_VM=register`, without the source excerpts. The register compiler has
/// no peephole optimizer, so `mode` is never `optimized`.
fn register_listing(source: &str, mode: Mode) -> String {
    match mode.setup {
        Setup::Parsed => {
            let tokens = Lexer::new(source, SourceId::SYNTHETIC)
                .collect::<Result<Vec<_>, _>>()
                .expect("lex failed");
            let expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
            let natives = Natives::default();
            let function =
                RegisterCompiler::compile(expressions.into_iter(), Analysis::default(), &natives)
                    .expect("compile failed");
            disassemble(&function, &natives, None)
        }
        Setup::Analysed | Setup::Stdlib => {
            let mut interpreter = Interpreter::capturing();
            if matches!(mode.setup, Setup::Stdlib) {
                interpreter.configure(ndc_stdlib::register);
            }
            let function = interpreter
                .compile_str(source)
                .unwrap_or_else(|error| panic!("compile failed: {error:?}"));
            disassemble(&function, interpreter.natives(), None)
        }
    }
}

fn programs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("programs")
}

/// Whether to write the compiler's listings into the expected files: `BLESS`
/// is set to anything but `0`.
fn blessing() -> bool {
    std::env::var("BLESS").is_ok_and(|value| !value.is_empty() && value != "0")
}

/// The modes the `// compile:` directive of `source` lists.
fn modes(source: &str) -> Vec<Mode> {
    let directive = source
        .lines()
        .find_map(|line| line.trim().strip_prefix("// compile:"))
        .expect("the program has no `// compile:` directive");
    directive
        .split(',')
        .map(|mode| Mode::parse(mode).unwrap_or_else(|error| panic!("{error}")))
        .collect()
}

/// What `backend` gives for `source` in each of `modes`: the listing alone
/// for one mode, and a section titled with each mode for more. For a backend
/// that doesn't optimize, `optimized` is left out of each mode, and a mode
/// that is then the same as an earlier one is left out.
fn snapshot(backend: &Backend, source: &str, modes: &[Mode]) -> String {
    let mut own_modes = Vec::new();
    for &mode in modes {
        let mode = Mode {
            optimized: mode.optimized && backend.optimizes,
            ..mode
        };
        if !own_modes.contains(&mode) {
            own_modes.push(mode);
        }
    }
    let modes = own_modes.as_slice();
    if let [mode] = modes {
        return (backend.listing)(source, *mode);
    }
    modes
        .iter()
        .map(|mode| format!("--- {mode} ---\n{}", (backend.listing)(source, *mode)))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Compares what each backend gives for the program at `relative`, under
/// `programs`, with its expected listing, or writes the listing there when
/// blessing.
fn check_snapshots(relative: &str) {
    let path = programs_dir().join(relative);
    let source = fs::read_to_string(&path).expect("the program can't be read");
    let modes = modes(&source);

    let mut failures = Vec::new();
    for backend in BACKENDS {
        let actual = snapshot(backend, &source, &modes);
        let expected_path = path.with_extension(format!("{}.txt", backend.name));
        let expected_name = expected_path
            .strip_prefix(programs_dir())
            .unwrap()
            .display();
        let expected = fs::read_to_string(&expected_path).ok();
        if expected.as_deref() == Some(actual.as_str()) {
            continue;
        }
        if blessing() {
            fs::write(&expected_path, &actual).expect("the expected listing can't be written");
            continue;
        }
        failures.push(match expected {
            Some(expected) => format!(
                "programs/{expected_name} isn't what the {} backend writes now \
                 (- expected, + actual):\n{}",
                backend.name,
                diff(&expected, &actual)
            ),
            None => format!(
                "programs/{expected_name} is missing; the {} backend writes:\n{actual}",
                backend.name
            ),
        });
    }
    assert!(
        failures.is_empty(),
        "\n{}\nIf the change is right, run `BLESS=1 cargo test -p compiler_tests` \
         to write it into the expected files.\n",
        failures.join("\n")
    );
}

/// How many unchanged lines [`diff`] shows around each change.
const CONTEXT: usize = 3;

/// The lines of `expected` and `actual` that differ, `-` before a line only
/// `expected` has and `+` before one only `actual` has, each change with up
/// to [`CONTEXT`] unchanged lines around it and `...` where lines are left out.
fn diff(expected: &str, actual: &str) -> String {
    let old = expected.lines().collect::<Vec<_>>();
    let new = actual.lines().collect::<Vec<_>>();

    // `common[i][j]`: how long the longest common subsequence of `old[i..]`
    // and `new[j..]` is.
    let mut common = vec![vec![0usize; new.len() + 1]; old.len() + 1];
    for i in (0..old.len()).rev() {
        for j in (0..new.len()).rev() {
            common[i][j] = if old[i] == new[j] {
                common[i + 1][j + 1] + 1
            } else {
                common[i + 1][j].max(common[i][j + 1])
            };
        }
    }

    let mut lines = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < old.len() || j < new.len() {
        if i < old.len() && j < new.len() && old[i] == new[j] {
            lines.push((' ', old[i]));
            i += 1;
            j += 1;
        } else if i < old.len() && (j == new.len() || common[i + 1][j] >= common[i][j + 1]) {
            lines.push(('-', old[i]));
            i += 1;
        } else {
            lines.push(('+', new[j]));
            j += 1;
        }
    }

    let changed = lines
        .iter()
        .enumerate()
        .filter(|(_, (marker, _))| *marker != ' ')
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    if changed.is_empty() {
        return "(the lines are the same; the line endings or the final newline differ)\n"
            .to_string();
    }
    let shown = |index: usize| {
        changed
            .iter()
            .any(|&change| index + CONTEXT >= change && index <= change + CONTEXT)
    };

    let mut out = String::new();
    let mut skipped = false;
    for (index, (marker, line)) in lines.iter().enumerate() {
        if shown(index) {
            if skipped {
                out.push_str("...\n");
                skipped = false;
            }
            writeln!(out, "{marker} {line}").unwrap();
        } else {
            skipped = true;
        }
    }
    if skipped {
        out.push_str("...\n");
    }
    out
}

/// Every expected listing is of a program and a backend that exist, so none
/// is left behind when a program is renamed or a backend removed.
#[test]
fn every_expected_listing_belongs_to_a_program_and_a_backend() {
    fn check(dir: &Path, stray: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                check(&path, stray);
                continue;
            }
            if path.extension().is_some_and(|e| e == "ndc") {
                continue;
            }
            let file_name = path.file_name().unwrap().to_string_lossy();
            let belongs = BACKENDS.iter().any(|backend| {
                file_name
                    .strip_suffix(&format!(".{}.txt", backend.name))
                    .is_some_and(|stem| path.with_file_name(format!("{stem}.ndc")).is_file())
            });
            if !belongs {
                stray.push(path);
            }
        }
    }

    let mut stray = Vec::new();
    check(&programs_dir(), &mut stray);
    assert!(
        stray.is_empty(),
        "these files are no program's expected listing: {stray:?}"
    );
}
