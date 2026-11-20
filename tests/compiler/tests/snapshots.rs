//! Checks what the compiler writes for each program under `programs`
//! against the listing expected of it, kept next to the program:
//! `programs/blocks/a_block_yields_its_last_expression.ndc` is expected to
//! compile to `a_block_yields_its_last_expression.txt`.
//!
//! A program says how to compile it in a directive, one line such as
//! `// compile: stdlib`, which may list several setups separated by commas;
//! the listing then has a section per setup.
//!
//! `BLESS=1 cargo test -p compiler_tests` writes what the compiler gives into
//! the expected files instead of comparing.

use ndc_analysis::Analysis;
use ndc_interpreter::Interpreter;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use ndc_vm::Natives;
use ndc_vm::compiler::Compiler;
use ndc_vm::disassemble::disassemble;
use std::fmt::{self, Write};
use std::fs;
use std::path::{Path, PathBuf};

include!(concat!(env!("OUT_DIR"), "/snapshot_tests.rs"));

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

impl Setup {
    /// The setup a directive names: `parsed`, `analysed` or `stdlib`.
    fn parse(text: &str) -> Result<Self, String> {
        match text.trim() {
            "parsed" => Ok(Self::Parsed),
            "analysed" => Ok(Self::Analysed),
            "stdlib" => Ok(Self::Stdlib),
            _ => Err(format!(
                "`{text}` names no setup: parsed, analysed or stdlib"
            )),
        }
    }
}

impl fmt::Display for Setup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Parsed => "parsed",
            Self::Analysed => "analysed",
            Self::Stdlib => "stdlib",
        })
    }
}

/// The code of `source` compiled with `setup`, as `ndc disassemble` lists
/// it, without the source excerpts.
fn listing(source: &str, setup: Setup) -> String {
    match setup {
        Setup::Parsed => {
            let tokens = Lexer::new(source, SourceId::SYNTHETIC)
                .collect::<Result<Vec<_>, _>>()
                .expect("lex failed");
            let expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
            let natives = Natives::default();
            let function =
                Compiler::compile(expressions.into_iter(), Analysis::default(), &natives)
                    .expect("compile failed");
            disassemble(&function, &natives, None)
        }
        Setup::Analysed | Setup::Stdlib => {
            let mut interpreter = Interpreter::capturing();
            if setup == Setup::Stdlib {
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

/// The setups the `// compile:` directive of `source` lists.
fn setups(source: &str) -> Vec<Setup> {
    let directive = source
        .lines()
        .find_map(|line| line.trim().strip_prefix("// compile:"))
        .expect("the program has no `// compile:` directive");
    directive
        .split(',')
        .map(|setup| Setup::parse(setup).unwrap_or_else(|error| panic!("{error}")))
        .collect()
}

/// What the compiler gives for `source` with each of `setups`: the listing
/// alone for one setup, and a section titled with each setup for more.
fn snapshot(source: &str, setups: &[Setup]) -> String {
    if let [setup] = setups {
        return listing(source, *setup);
    }
    setups
        .iter()
        .map(|setup| format!("--- {setup} ---\n{}", listing(source, *setup)))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Compares what the compiler gives for the program at `relative`, under
/// `programs`, with its expected listing, or writes the listing there when
/// blessing.
fn check_snapshots(relative: &str) {
    let path = programs_dir().join(relative);
    let source = fs::read_to_string(&path).expect("the program can't be read");
    let actual = snapshot(&source, &setups(&source));
    let expected_path = path.with_extension("txt");
    let expected = fs::read_to_string(&expected_path).ok();
    if expected.as_deref() == Some(actual.as_str()) {
        return;
    }
    if blessing() {
        fs::write(&expected_path, &actual).expect("the expected listing can't be written");
        return;
    }
    let expected_name = expected_path
        .strip_prefix(programs_dir())
        .unwrap()
        .display();
    let failure = match expected {
        Some(expected) => format!(
            "programs/{expected_name} isn't what the compiler writes now \
             (- expected, + actual):\n{}",
            diff(&expected, &actual)
        ),
        None => format!("programs/{expected_name} is missing; the compiler writes:\n{actual}"),
    };
    panic!(
        "\n{failure}\nIf the change is right, run `BLESS=1 cargo test -p compiler_tests` \
         to write it into the expected file.\n"
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

/// Every expected listing is of a program that exists, so none is left
/// behind when a program is renamed or removed.
#[test]
fn every_expected_listing_belongs_to_a_program() {
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
            let belongs = path.extension().is_some_and(|e| e == "txt")
                && path.with_extension("ndc").is_file();
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
