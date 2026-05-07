# Contributing

Thanks for considering a contribution. This document covers how to build,
test, and submit changes.

If you used AI tools to write any part of your patch, please mention which
parts and which models in your PR description (see [README.md](README.md)).

## Prerequisites

A working [Rust toolchain](https://rustup.rs/). The workspace targets
stable Rust.

## Build

```bash
cargo build
```

## Running the test suite

The workspace has three test crates, each living under `tests/`:

| Crate              | Path                | What it covers |
|---|---|---|
| `functional_tests` | `tests/functional/` | End-to-end `.ndc` programs and REPL integration tests. The crate's `build.rs` auto-generates one Rust `#[test]` per `.ndc` file under `tests/functional/programs/`. |
| `compiler_tests`   | `tests/compiler/`   | Asserts exact bytecode `OpCode` sequences for given source. |
| `proptest_tests`   | `tests/proptest/`   | Property-based fuzz harness that drives random token streams through the parser → analyser → compiler → VM and looks for panics. |

### Run everything

```bash
cargo test
```

This runs all three crates. The proptest crate's fuzz test is `#[ignore]`d
by default (see below); only its **regression replay** runs as part of
`cargo test`.

### Run a single crate

```bash
cargo test -p functional_tests
cargo test -p compiler_tests
cargo test -p proptest_tests
```

### Run a single test by name (substring match)

```bash
cargo test test_001_math_001_addition
```

### Run the proptest fuzz harness

The harness lives in `tests/proptest/tests/panic.rs` and contains two
test functions sharing a single `panic.regressions` file:

* `known_regressions_do_not_panic` — replays previously-saved failure
  seeds and runs as part of normal `cargo test`. If a known panic
  resurfaces, this test fails immediately.
* `fuzz_random_tokens` — generates fresh random token streams.
  `#[ignore]`d because each run typically turns up new panics that have
  not been triaged.

```bash
# Fuzz only (1024 cases by default)
cargo test -p proptest_tests -- --ignored

# Regressions + fuzz
cargo test -p proptest_tests -- --include-ignored

# Long fuzz run
PROPTEST_CASES=100000 cargo test -p proptest_tests -- --ignored
```

When a fresh fuzz run finds a new panic, proptest shrinks it and appends
the seed to `tests/proptest/tests/panic.regressions`. **Commit the
updated `.regressions` file** — every subsequent `cargo test` will
guard against that input.

## Writing new tests

### Functional `.ndc` tests

Drop a new `.ndc` file under `tests/functional/programs/<category>/`.
The file's name becomes the Rust test function name. Use comment
directives at the top of the file to assert behaviour:

```ndc
// expect-output: 42      ← stdout must equal this exactly
// expect-error: divide   ← error message must contain this substring
```

### Compiler bytecode tests

Add cases to `tests/compiler/tests/compiler.rs` when introducing a new
`OpCode` or changing emit logic. Each test asserts the exact instruction
sequence for a given source snippet.

## Before opening a PR

```bash
cargo fmt
cargo clippy            # fix all warnings
cargo test
```

If you find yourself writing a `TODO` comment, please open a GitHub
issue instead and record it in [`TODO.md`](TODO.md).

## Git conventions

* Commit subjects and PR titles use [Conventional Commits]: `feat`,
  `fix`, `refactor`, `perf`, `style`, `test`, `docs`, `build`, `ops`,
  `chore`. Scope is optional; prefer a crate or subsystem name when it
  helps (e.g. `fix(parser): …`).
* Place an emoji at the end of the title, for example
  `perf(lexer): make token scanning faster 🐌`.
* Branches use a category prefix: `feature/…`, `bugfix/…`,
  `housekeeping/…`, etc.
* The repository squash-merges PRs, so the PR title becomes the merged
  commit message — keep it valid as a Conventional Commit subject.

[Conventional Commits]: https://www.conventionalcommits.org/
