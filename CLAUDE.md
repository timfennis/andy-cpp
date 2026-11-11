# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Before commit
- Run `cargo fmt`
- Run `cargo clippy` and fix all warnings
- Ensure all tests pass (`cargo test`)
- Do not leave `TODO` comments in code — either fix the issue immediately or open a GitHub issue

## Common Commands

```bash
# Build
cargo build

# Run all tests
cargo test

# Run a single test by name (substring match on the filename)
cargo test test_001_math_001_addition

# Run benchmarks
cargo bench -p benches

# Start REPL
cargo run --bin ndc

# Run a .ndc script
cargo run --bin ndc -- script.ndc

# Disassemble bytecode
cargo run --bin ndc -- disassemble script.ndc

# Build the browser playground into ndc_wasm/web (needs the wasm32-unknown-unknown
# target and wasm-bindgen-cli at the version in Cargo.lock), then serve that folder
./ndc_wasm/build.sh

# Show documentation (optionally filtered by query)
cargo run --bin ndc -- docs [query] [--no-color]

# Profile with perf (requires release-with-debug profile in Cargo.toml)
cargo build --profile release-with-debug
hyperfine --warmup 3 './target/release-with-debug/ndc script.ndc'
perf stat ./target/release-with-debug/ndc script.ndc
perf record -g --call-graph=dwarf -o /tmp/out.perf ./target/release-with-debug/ndc script.ndc
perf report -i /tmp/out.perf --stdio --no-children --percent-limit=1
```

## Manual

User-facing language documentation lives in `manual/src/`. It is an mdBook project. The entry point is `manual/src/SUMMARY.md`.

When making changes that affect language behaviour or runtime semantics, update the relevant manual page.

## Architecture

This is a custom language interpreter ("Andy C++") with a bytecode VM backend:

```
Source → [Lexer] → Tokens → [Parser] → AST → [Analyser] → Analysis
                                        │                     │
                                        └───→ [Compiler] ←────┘
                                                   ↓
                                             [Bytecode VM] → Value
```

The AST is syntax only. The analyser runs name resolution (`ndc_resolver`) on each statement before typing it, and storage layout (`ndc_layout`) once at the end of each batch, and records what it works out in an `Analysis` keyed by each node's `NodeId`. The analyser itself only deals in symbols and types; the layout pass turns the symbols it picked into slots.

### Git Workflow
- Use Conventional Commit subjects for commits and pull request titles: `feat`, `fix`, `refactor`, `perf`, `style`, `test`, `docs`, `build`, `ops`, `chore`
- Scopes are optional; when a scope helps, prefer a crate or subsystem such as `lexer`, `parser`, `vm`, `compiler`, or `lsp`
- This repository uses squash merging, so every pull request title must already be a valid Conventional Commit subject because it becomes the merged commit message
- Included an emoji in a commit or pull request title. Place it at the end of the title, for example `perf(lexer): make token scanning faster 🐌`
- Branch names use category prefixes: `feature/`, `bugfix/`, `housekeeping/`, etc.

### Crate Layout

| Crate | Role |
|---|---|
| `ndc_lexer` | Tokenisation, `Span` (offset+length) |
| `ndc_parser` | AST (`Expression`, `ExpressionLocation`), parser |
| `ndc_core` | `StaticType`, `FunctionRegistry`, `NodeId` |
| `ndc_resolver` | Name resolution: `NameResolver`, `SymbolId`, per-use candidate symbols and closure captures |
| `ndc_analysis` | What the analyser works out for the compiler and LSP: `Analysis` (tables keyed by `NodeId`), `ResolvedVar`, `Binding`, `Candidate`, `CaptureSource` |
| `ndc_layout` | Storage layout: each resolver symbol's global or local slot, each function's captures and frame size; `Layout::lay_out` fills the location tables of `Analysis` from the analyser's `Picks` |
| `ndc_analyser` | Semantic analyser (types, overload bindings by symbol), runs the resolver first and the layout pass last |
| `ndc_interpreter` | `Interpreter` facade (analyse + compile + run via VM) |
| `ndc_vm` | Bytecode `Compiler`, stack-based `Vm`, and the runtime `Value` (including `Number`) |
| `ndc_stdlib` | Built-in functions registered via `FunctionRegistry` |
| `ndc_lsp` | LSP backend (hover, inlay hints) |
| `ndc_diagnostics` | Renders `InterpreterError`s as codespan reports, for the CLI (coloured, `color` feature) and the browser (plain text) |
| `ndc_bin` | CLI entry point, REPL, syntax highlighting |
| `ndc_wasm` | The interpreter for the browser via wasm-bindgen, and the playground page in `ndc_wasm/web/` |

### Key Concepts

**Single execution path** — The bytecode VM in `ndc_vm` is the only execution path. `ndc_interpreter` acts as a facade: it runs the semantic analyser, compiles to bytecode via `ndc_vm::Compiler`, and executes via `ndc_vm::Vm`.

**Value type** — `ndc_vm/src/value/mod.rs` defines the one runtime `Value`. It is constrained to 16 bytes: `Int(i64)`, `Float(f64)`, `Number(Rc<AdvancedNumber>)` (BigInt/Rational/Complex), `Bool`, `None`, `Object(Rc<Object>)`, and `Cell` for captured locals.

**Function overloading** — Functions are matched by name and arity. The semantic analyser produces `Binding::Resolved` (exact compile-time match) or `Binding::Dynamic(Vec<ResolvedVar>)` (runtime dispatch among candidates). Binary operators like `+` are parsed as `Expression::Call`.

**Semantic analyser** — `ndc_analyser/src/analyser/` infers `StaticType` and resolves function bindings: `mod.rs` holds the expression match, with calls, assignments and declarations in their own modules. `StaticType::Any` is the fallback when inference fails.

**`FunctionRegistry`** — Lives in `ndc_core`. Holds all registered built-in functions as `Rc<NativeFunction>`. Replaces the old `Environment`-based function registry. At runtime, natives are passed to the VM as global slots.

**Top-level declarations are globals** — Natives take the first global slots, then every declaration in the program scope (`let`, `fn`, `struct` constructors and accessors) gets the next one, in order, via `ndc_layout::Layout::global_slot`. Functions reach them with `GetGlobal`/`SetGlobal` and never capture them; only locals of functions and of top-level blocks and loops are captured, through cells.

**Persistent REPL** — Each evaluation compiles into its own top-level function and runs on a fresh VM. A resumable `Interpreter` keeps the analyser and the globals vector between evaluations, which is how a REPL line sees what earlier lines declared. A resumable `Interpreter` starts the session over after an evaluation that fails past parsing, as the failed code may have done only part of its work; there is no in-process rollback. On Unix, `ndc_bin/src/isolation.rs` instead runs each REPL line in a forked copy of the REPL process, so a line that fails (error, panic, stack overflow, Ctrl-C) leaves the session exactly as it was. `ndc_bin/tests/repl.rs` drives the built binary to test this.

### Test Infrastructure

Test crates live under `tests/`:

- `tests/functional/` (`functional_tests`) — `.ndc` integration tests and REPL tests. `build.rs` auto-generates one Rust `#[test]` per `.ndc` file under `tests/functional/programs/`, runs via `Interpreter::eval` (VM). Test directives in `.ndc` files:
  ```ndc
  // expect-output: 42      ← assert stdout equals this
  // expect-error: divide   ← assert error message contains this substring
  ```
- `tests/compiler/` (`compiler_tests`) — validates the bytecode compiler by asserting exact `OpCode` sequences. Use when adding new VM instructions.
- `ndc_bin/tests/repl.rs` — feeds lines to the built `ndc` binary's REPL (Unix only) and checks what it prints, e.g. that a failed line leaves no trace.
- `tests/proptest/` (`proptest_tests`) — property-based panic fuzzer over random token streams. The fresh-fuzz test is `#[ignore]`d; regression replay (from `panic.regressions`) runs by default. See [CONTRIBUTING.md](CONTRIBUTING.md) for run commands.
