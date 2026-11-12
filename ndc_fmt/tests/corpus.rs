//! Formats every program in the functional tests and every example in the
//! manual. `format` itself checks that the output means the same; this checks
//! that formatting it again changes nothing.

use ndc_lexer::SourceId;
use ndc_parser as _;
use std::fs;
use std::path::{Path, PathBuf};
use thiserror as _;

fn files(dir: &Path, extension: &str, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("the directory is readable") {
        let path = entry.expect("the directory entry is readable").path();
        if path.is_dir() {
            files(&path, extension, out);
        } else if path.extension().is_some_and(|found| found == extension) {
            out.push(path);
        }
    }
}

/// Formats each of `programs`, named by the first of each pair, and fails
/// on any that formats to something that formats differently again. Returns
/// how many were formatted; the others don't parse.
fn assert_stable(programs: &[(String, String)]) -> usize {
    let mut failures = Vec::new();
    let mut formatted_count = 0;
    for (name, source) in programs {
        let formatted = match ndc_fmt::format(source, SourceId::SYNTHETIC) {
            Ok(formatted) => {
                formatted_count += 1;
                formatted
            }
            // Programs that test syntax errors
            Err(ndc_fmt::Error::Lexer(_) | ndc_fmt::Error::Parser(_)) => continue,
            Err(error) => {
                failures.push(format!("{name}: {error}\n{source}"));
                continue;
            }
        };
        match ndc_fmt::format(&formatted, SourceId::SYNTHETIC) {
            Ok(again) if again == formatted => {}
            Ok(again) => failures.push(format!(
                "{name}: formatting twice changes it\n--- once\n{formatted}\n--- twice\n{again}"
            )),
            Err(error) => {
                failures.push(format!(
                    "{name}: the formatted program fails: {error}\n{formatted}"
                ));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
    formatted_count
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..")
}

#[test]
fn formatting_the_test_programs_is_stable() {
    let mut paths = Vec::new();
    files(&root().join("tests/functional/programs"), "ndc", &mut paths);
    paths.sort();
    let programs: Vec<_> = paths
        .iter()
        .map(|path| {
            let source = fs::read_to_string(path).expect("the program is readable");
            (path.display().to_string(), source)
        })
        .collect();

    let formatted = assert_stable(&programs);
    // Only the programs that test syntax errors are left out
    assert!(
        formatted > programs.len() * 9 / 10,
        "only {formatted} of {} programs were formatted",
        programs.len()
    );
}

#[test]
fn formatting_the_manual_examples_is_stable() {
    let mut paths = Vec::new();
    files(&root().join("manual/src"), "md", &mut paths);
    paths.sort();
    let mut examples = Vec::new();
    for path in &paths {
        let page = fs::read_to_string(path).expect("the page is readable");
        let mut example: Option<(usize, String)> = None;
        for (number, line) in page.lines().enumerate() {
            match &mut example {
                None if line.trim_start().starts_with("```ndc") => {
                    example = Some((number + 1, String::new()));
                }
                Some((start, source)) if line.trim_start().starts_with("```") => {
                    examples.push((
                        format!("{}:{start}", path.display()),
                        std::mem::take(source),
                    ));
                    example = None;
                }
                Some((_, source)) => {
                    source.push_str(line);
                    source.push('\n');
                }
                None => {}
            }
        }
    }

    let formatted = assert_stable(&examples);
    assert!(
        formatted > examples.len() / 2,
        "only {formatted} of {} examples were formatted",
        examples.len()
    );
}
