# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Before commit
- Run `cargo fmt`
- Run `cargo check` and fix warnings and errors
- Ensure all non-ignored tests pass
- Check if any ignored tests pass, if they do mark them with `// vm-ready` first

## Common Commands

```bash
# Build
cargo build

# Run all tests
cargo test

# Run a single test by name (substring match on the filename)
cargo test test_001_math_001_addition

# Run VM tests (ignored by default)
cargo test -- --ignored
cargo test test_vm_001_math -- --ignored

# Run benchmarks
cargo bench -p benches

# Start REPL
cargo run --bin ndc

# Run a .ndc script (tree-walk)
cargo run --bin ndc -- script.ndc

# Run a .ndc script (bytecode VM)
cargo run --bin ndc -- --vm script.ndc

# Disassemble bytecode
cargo run --bin ndc -- disassemble script.ndc
```

## Architecture

This is a custom language interpreter ("Andy C++") with two execution backends:

```
Source → [Lexer] → Tokens → [Parser] → AST → [Analyser] → Annotated AST
                                                    ↓                ↓
                                          [Tree-Walk Interpreter]  [Compiler]
                                                    ↓                ↓
                                                 Value           [Bytecode VM] → Value
```

### Crate Layout

| Crate | Role |
|---|---|
| `ndc_lexer` | Tokenisation, `Span` (offset+length) |
| `ndc_parser` | AST (`Expression`, `ExpressionLocation`), parser, `StaticType` |
| `ndc_core` | `Number` (BigInt/Rational/Complex), ordering, hashing |
| `ndc_interpreter` | Tree-walk evaluator, semantic analyser, function dispatch |
| `ndc_vm` | Bytecode `Compiler` and stack-based `Vm` |
| `ndc_stdlib` | Built-in functions registered in `Environment` |
| `ndc_lsp` | LSP backend (hover, inlay hints) |
| `ndc_bin` | CLI entry point, REPL, syntax highlighting |

### Key Concepts

**Two execution modes** — The tree-walk interpreter in `ndc_interpreter` is the reference implementation. The bytecode VM in `ndc_vm` is the work-in-progress performance path. Tests with `// vm-ready` are known to pass on both.

**Value types** — `ndc_interpreter/src/value.rs` and `ndc_vm/src/value.rs` are separate enums. The VM `Value` is constrained to 16 bytes (`Int(i64)`, `Float(f64)`, `Bool`, `None`, `Object(Box<Object>)`).

**Function overloading** — Functions are matched by name and arity. The semantic analyser produces `Binding::Resolved` (exact compile-time match) or `Binding::Dynamic(Vec<ResolvedVar>)` (runtime dispatch among candidates). Binary operators like `+` are parsed as `Expression::Call`.

**Semantic analyser** — `ndc_interpreter/src/semantic/analyser.rs` infers `StaticType` and resolves function bindings. `StaticType::Any` is the fallback when inference fails.

**`Environment`** — Holds global variable slots and all registered built-in functions. Passed through evaluation and compiled into global slot indices in the VM.

### Test Infrastructure

The `tests` crate auto-generates test functions at build time via `tests/build.rs`. For every `.ndc` file under `tests/programs/`, two Rust test functions are generated:
- `test_<path>` — runs with tree-walk interpreter
- `test_vm_<path>` — runs with VM (marked `#[ignore]` unless the file contains `// vm-ready`)

Test directives are comments inside `.ndc` files:
```ndc
// vm-ready               ← enable test_vm_* function
// expect-output: 42      ← assert stdout equals this
// expect-error: divide   ← assert error message contains this substring
```

### Compiler Tests

`compiler_tests/` validates the bytecode compiler by asserting exact `OpCode` sequences. Use these when adding new VM instructions.
