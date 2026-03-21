# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Before commit
- Run `cargo fmt`
- Run `cargo clippy` and fix all warnings
- Ensure all tests pass (`cargo test`)
- Do not leave `TODO` comments in code — either fix the issue immediately or open a GitHub issue and record it in `TODO.md`

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
Source → [Lexer] → Tokens → [Parser] → AST → [Analyser] → Annotated AST
                                                                  ↓
                                                            [Compiler]
                                                                  ↓
                                                          [Bytecode VM] → Value
```

### Git Workflow
- Prefer short commit messages, only use multiple lines in case of unrelated changes
- Pull request titles must start with an emoji
- Branch names use category prefixes: `feature/`, `bugfix/`, `housekeeping/`, etc.

### Crate Layout

| Crate | Role |
|---|---|
| `ndc_lexer` | Tokenisation, `Span` (offset+length) |
| `ndc_parser` | AST (`Expression`, `ExpressionLocation`), parser |
| `ndc_core` | `Number` (BigInt/Rational/Complex), `StaticType`, `FunctionRegistry`, ordering, hashing |
| `ndc_interpreter` | Semantic analyser, `Interpreter` facade (compile + run via VM) |
| `ndc_vm` | Bytecode `Compiler` and stack-based `Vm` |
| `ndc_stdlib` | Built-in functions registered via `FunctionRegistry` |
| `ndc_lsp` | LSP backend (hover, inlay hints) |
| `ndc_bin` | CLI entry point, REPL, syntax highlighting |

### Key Concepts

**Single execution path** — The bytecode VM in `ndc_vm` is the only execution path. `ndc_interpreter` acts as a facade: it runs the semantic analyser, compiles to bytecode via `ndc_vm::Compiler`, and executes via `ndc_vm::Vm`. `vm_bridge.rs` handles value conversion between `ndc_interpreter::Value` and `ndc_vm::Value`.

**Value types** — `ndc_interpreter/src/value.rs` and `ndc_vm/src/value.rs` are separate enums. The VM `Value` is constrained to 16 bytes (`Int(i64)`, `Float(f64)`, `Bool`, `None`, `Object(Box<Object>)`).

**Function overloading** — Functions are matched by name and arity. The semantic analyser produces `Binding::Resolved` (exact compile-time match) or `Binding::Dynamic(Vec<ResolvedVar>)` (runtime dispatch among candidates). Binary operators like `+` are parsed as `Expression::Call`.

**Semantic analyser** — `ndc_interpreter/src/semantic/analyser.rs` infers `StaticType` and resolves function bindings. `StaticType::Any` is the fallback when inference fails.

**`FunctionRegistry`** — Lives in `ndc_core`. Holds all registered built-in functions as `Rc<NativeFunction>`. Replaces the old `Environment`-based function registry. At runtime, natives are passed to the VM as global slots.

**Persistent REPL** — The `Interpreter` keeps `repl_state: Option<(Vm, Compiler)>` so variables declared on one REPL line are visible on subsequent lines (resume-from-halt pattern).

### Test Infrastructure

The `tests` crate auto-generates one test function per `.ndc` file at build time via `tests/build.rs`. For every `.ndc` file under `tests/programs/`, a single Rust test function is generated:
- `test_<path>` — runs via `Interpreter::run_str` (VM)

Test directives are comments inside `.ndc` files:
```ndc
// expect-output: 42      ← assert stdout equals this
// expect-error: divide   ← assert error message contains this substring
```

### Compiler Tests

`compiler_tests/` validates the bytecode compiler by asserting exact `OpCode` sequences. Use these when adding new VM instructions.
