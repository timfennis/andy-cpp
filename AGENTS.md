# Repository Guidelines

## Project Structure & Module Organization
This repository is a Rust workspace for the Andy C++ language. Core crates live at the root: `ndc_lexer`, `ndc_parser`, `ndc_analyser`, `ndc_vm`, `ndc_interpreter`, `ndc_stdlib`, `ndc_core`, `ndc_lsp`, `ndc_macros`, and the CLI in `ndc_bin`. Integration tests live in `tests/` and `compiler_tests/`. Benchmarks are split between the Criterion crate in `benches/` and the `bench.sh` runner. End-user docs are in `manual/`, and the VS Code extension is isolated in `ext/andy-cpp/`.

## Build, Test, and Development Commands
Use workspace commands from the repository root:

- `cargo build --workspace` builds all Rust crates.
- `cargo test --workspace` runs the main test suites and matches CI.
- `cargo build --no-default-features` checks the reduced feature set used in CI.
- `cargo install --path ndc_bin` installs the `ndc` CLI locally.
- `cargo run -p ndc_bin -- run script.ndc` runs a script without installing.
- `./bench.sh benches/programs/fibonacci.ndc` builds a release binary and benchmarks it with `hyperfine`.

For the VS Code extension:

- `cd ext/andy-cpp && npm run compile` builds the extension.
- `cd ext/andy-cpp && npm test` runs the extension test workflow.

## Coding Style & Naming Conventions
Follow standard Rust formatting and run `cargo fmt --all` before submitting changes. Use 4-space indentation, `snake_case` for functions/modules/files, `PascalCase` for types, and keep crate names prefixed with `ndc_` for language components. Prefer small, focused modules over large multi-purpose files. In the extension, keep TypeScript files under `src/` and rely on the existing `eslint` setup.

## Testing Guidelines
Add language behavior tests as `.ndc` programs under `tests/programs/<category>/` using the existing numeric prefixes such as `001_addition.ndc`. Encode expectations inline with `// expect-output:` or `// expect-error:` comments. Add compiler bytecode assertions to `compiler_tests/tests/compiler.rs` for VM-level changes. Run `cargo test --workspace` before opening a PR.

## Commit & Pull Request Guidelines
Recent commits use short, imperative subjects, often with an emoji prefix, for example `🐛 Fix incorrect return types for sequence functions`. Keep subjects specific and under one line. PRs should describe the behavioral change, link the relevant issue or PR when applicable, and call out AI-generated contributions as requested in `README.md`. Include screenshots only for editor-extension UI changes.
