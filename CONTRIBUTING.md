# Contributing

Thanks for considering a contribution. This document covers how to build,
test, and submit changes.

## Using AI tools

You're welcome to write your patch with an LLM or a coding agent. A lot of
this codebase was written that way. Please mention which parts and which
models in your PR description (see [README.md](README.md)).

What we ask is that a person who's interested in the project stands behind
the PR: someone who has read the change, can answer questions about it, and
will respond to review. For anything beyond a small fix, please open an
issue or comment on an existing one to agree on the approach first.

We close PRs that look like automated drive-by contributions without
reviewing them, for example an agent working through open issues across
many repositories. Issues labelled `good first issue` are there to help people
find their way into the codebase; an agent can fix them for us just as well.

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
| `compiler_tests`   | `tests/compiler/`   | Disassembly snapshots: each `.ndc` program under `tests/compiler/programs/` is compiled and its listing compared with the expected one next to it. |
| `proptest_tests`   | `tests/proptest/`   | Property-based fuzz harnesses: random token streams through the parser → analyser → compiler → VM, generated programs that run, and generated programs that stress the analyser's overload choices. They look for panics, hangs, and generated programs that run differently inside a function than at the top level. |

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
test functions sharing a single `panic.regressions` file. Each runs a
random token stream through the whole pipeline, and fails on a panic or a
hang:

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

### Run the generated-programs fuzz harness

Random token streams rarely get past the parser.
`tests/proptest/tests/generated_programs.rs` generates programs that do
run, built from what the language has (arithmetic, variables, `if`,
bounded `while` and `for` loops, lists, tuples, indexing, functions,
closures, assignment with operators, `break`, `continue`, `return`), and
runs each twice: at the top level, where its variables are globals, and
inside a function `main`, where they are registers. Both must print the
same, give the same value and fail with the same error at the same place,
and neither may panic or hang. Every loop is bounded, so no program runs
or allocates without end. A failure shows the shrunk program as source.

```bash
# The short run (300 programs) and the saved seeds, part of `cargo test`
cargo test -p proptest_tests --test generated_programs

# Long run (10,000 programs by default)
cargo test -p proptest_tests --test generated_programs -- --ignored
PROPTEST_CASES=100000 cargo test -p proptest_tests --test generated_programs -- --ignored
```

Its seeds go to `tests/proptest/tests/generated_programs.regressions`;
commit them with the fix.

### Run the overload fuzz harness

`tests/proptest/tests/overloads.rs` generates well-formed programs built
from what makes the analyser commit to an overload before the types are
fully known: overloaded functions, empty literals, lambdas, function
values, `map`, `if`, writes and annotations. In debug builds the analyser
checks that each such early commit is the overload the call or value gets
once the types are known, so any disagreement panics. The minimal
failing program is printed as source.

```bash
# Fuzz only (1024 cases by default)
cargo test -p proptest_tests --test overloads -- --ignored

# Long fuzz run
PROPTEST_CASES=100000 cargo test -p proptest_tests --test overloads -- --ignored
```

Its seeds go to `tests/proptest/tests/overloads.regressions`, which
`known_overload_regressions_do_not_panic` replays in every `cargo test`.

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

Add a program under `tests/compiler/programs/<topic>/` when introducing a
new instruction or changing what the compiler emits. Start it with a
comment that says what it shows, then a directive that says how to compile
it:

```ndc
// A string literal makes a new string with the text of its constant
// (`MakeString`).
// compile: parsed
"abc"
```

The setup is `parsed` (no analysis, no natives), `analysed` (no natives) or
`stdlib` (with the standard library). `// compile: parsed, analysed` lists
the program once per setup, in a section each.

The expected listing lives next to the program as `<name>.txt`, in the
format `ndc disassemble` prints, without source excerpts: one instruction
per line with the registers it reads and writes (`AddInt r2, r0, r1`).
Natives and intrinsics show by name (`CallNative r0, len, r1, 1`) and the
program's own globals by their position among them (`SetGlobal g0, r1`),
so adding a native to the standard library changes no listing. To write
the listing of a new program, or accept a change to existing ones, run:

```bash
BLESS=1 cargo test -p compiler_tests
```

and review the diff of the `.txt` files before committing. Without `BLESS`,
a listing that differs fails its test with a diff.

## Before opening a PR

```bash
cargo fmt
cargo clippy            # fix all warnings
cargo test
```

If you find yourself writing a `TODO` comment, please open a GitHub
issue instead.

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
