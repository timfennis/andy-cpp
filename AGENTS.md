# Repository Guidelines

Guidance for coding agents and contributors working in this repository: a Rust workspace for the Andy C++ language. The crates live at the root (see [Crate Layout](#crate-layout)), the tests under `tests/`, the benchmarks in `benches/`, the end-user manual in `manual/`, and the VS Code extension, which is its own npm project, in `ext/andy-cpp/`.

## Before commit
- Run `cargo fmt`
- Run `cargo clippy` and fix all warnings
- Ensure all tests pass (`cargo test`)
- Do not leave `TODO` comments in code — either fix the issue immediately or open a GitHub issue

## Coding Style
Follow standard Rust formatting: 4-space indentation, `snake_case` for functions, modules and files, `PascalCase` for types. Crates that are part of the language are named `ndc_…`. Prefer small, focused modules over large multi-purpose files. In the extension, keep TypeScript files under `src/` and rely on its `eslint` setup.

## Git Workflow
- Use Conventional Commit subjects for commits and pull request titles: `feat`, `fix`, `refactor`, `perf`, `style`, `test`, `docs`, `build`, `ops`, `chore`
- Scopes are optional; when a scope helps, prefer a crate or subsystem such as `lexer`, `parser`, `vm`, `compiler`, or `lsp`, for example `fix(parser): handle trailing commas`
- This repository uses squash merging, so every pull request title must already be a valid Conventional Commit subject because it becomes the merged commit message
- Include an emoji in a commit or pull request title. Place it at the end of the title, for example `perf(lexer): make token scanning faster 🐌`
- Branch names use category prefixes: `feature/`, `bugfix/`, `housekeeping/`, etc.
- A pull request describes the change in behaviour, links the relevant issue or pull request, and says where it was written with AI, as `CONTRIBUTING.md` asks

## Common Commands

```bash
# Build
cargo build

# Run all tests
cargo test

# Run a single test by name (substring match on the filename)
cargo test test_001_math_001_addition

# Check the standard library without its default features, as CI does
cargo build -p ndc_stdlib --no-default-features

# Install the ndc CLI
cargo install --path ndc_bin

# Run benchmarks
cargo bench -p benches

# Benchmark one program against the installed ndc (needs hyperfine)
./bench.sh benches/programs/fibonacci.ndc

# Compare two ndc binaries on whole programs (benches/programs and, when it's
# checked out, Advent of Brian): CPU time and peak memory per program, totals
python3 benches/compare.py path/to/baseline/ndc path/to/candidate/ndc

# Start REPL
cargo run --bin ndc

# Run a .ndc script
cargo run --bin ndc -- script.ndc

# Disassemble bytecode
cargo run --bin ndc -- disassemble script.ndc

# Build the interpreter for the browser into manual/src/playground/pkg (needs the
# wasm32-unknown-unknown target and wasm-bindgen-cli at the version in Cargo.lock),
# then serve the manual with its playground and runnable examples
./ndc_wasm/build.sh
mdbook serve manual

# Bundle the playground's code editor (CodeMirror) into manual/src/playground/editor.js.
# Without it the playground falls back to a plain editor. npm never runs on the host:
# the script runs it in a Docker image pinned by digest, from manual/editor's lockfile
# (exact versions, install scripts off). Add or upgrade packages with
# `./manual/editor/build.sh add <package>`, which only takes versions over 30 days old.
./manual/editor/build.sh

# Format .ndc files in place (--check lists unformatted files instead)
cargo run --bin ndc -- fmt script.ndc

# Build and test the VS Code extension
cd ext/andy-cpp && npm run compile
cd ext/andy-cpp && npm test

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

The manual introduces people to the language and teaches the most common ways to use it, with examples and plain English. It does not document every edge case of the compiler or analyser: leave out corner cases, internal rules and exact error conditions that a newcomer won't run into. Edge cases belong in tests, and in the error messages and help texts that explain them when someone does run into one.

When making changes that affect language behaviour or runtime semantics, update the relevant manual page.

The published manual highlights ` ```ndc ` code blocks and gives them a Run button, running them on the interpreter compiled to WebAssembly (`manual/theme/ndc.js`). Mark a block that can't run on its own, such as a fragment that uses names from an earlier block, as ` ```ndc,norun `. Write examples so running them shows the result: `print(-7 / 2);  // -3` rather than `assert_eq(-7 / 2, -3);`, with the exact printed output in the comment. A block that demonstrates a runtime error ends with it, as nothing after the first error runs, and is marked ` ```ndc,fails `. `tests/functional/tests/manual.rs` runs every block that isn't `norun` and checks all of this: blocks fail exactly when marked `fails`, the others show something, and print comments match the output (`cargo test -p functional_tests --test manual`). The playground page and the code it shares with the manual live in `manual/src/playground/`.

## Architecture

This is a custom language interpreter ("Andy C++") with a register-based bytecode VM:

```
Source → [Lexer] → Tokens → [Parser] → AST → [Analyser] → Analysis
                                        │                     │
                                        └───→ [Compiler] ←────┘
                                                   ↓
                                             [Bytecode VM] → Value
```

The AST is syntax only. The analyser runs name resolution (`ndc_resolver`) on each statement before typing it, and storage layout (`ndc_layout`) once at the end of each batch, and records what it works out in an `Analysis` keyed by each node's `NodeId`. The analyser itself only deals in symbols and types; the layout pass turns the symbols it picked into slots.

### Crate Layout

| Crate | Role |
|---|---|
| `ndc_lexer` | Tokenisation, `Span` (source id, offset, length) |
| `ndc_parser` | AST (`Expression`, `ExpressionLocation`), parser |
| `ndc_core` | `StaticType`, `FunctionRegistry`, `NodeId` |
| `ndc_resolver` | Name resolution: `NameResolver`, `SymbolId`, per-use candidate symbols and closure captures |
| `ndc_analysis` | What the analyser works out for the compiler and LSP: `Analysis` (tables keyed by `NodeId`), `ResolvedVar`, `Binding`, `Candidate`, `CaptureSource` |
| `ndc_layout` | Storage layout: each resolver symbol's global or local slot, each function's captures and frame size; `Layout::lay_out` fills the location tables of `Analysis` from the analyser's `Picks` |
| `ndc_analyser` | Semantic analyser (types, overload bindings by symbol), runs the resolver first and the layout pass last |
| `ndc_interpreter` | `Interpreter` facade (analyse + compile + run via VM) |
| `ndc_vm` | The bytecode `Compiler` (`compiler.rs`), its `Instr`s (`instr.rs`) and `Chunk`s (`chunk.rs`), the register-based `Vm` (`vm.rs`), the disassembler, the runtime `Value` (including `Number`), and `Natives`, which makes each native on first use |
| `ndc_stdlib` | Built-in functions registered via `FunctionRegistry` |
| `ndc_lsp` | Language features (completion, hover, diagnostics, inlay hints, symbols, go-to-definition) over a `Document`; with the default `server` feature, the LSP server that serves them over stdio |
| `ndc_diagnostics` | Renders `InterpreterError`s as codespan reports, for the CLI (coloured, `color` feature) and the browser (plain text) |
| `ndc_highlight` | Classifies the pieces of a program (keyword, function, comment, …) for syntax highlighting, from the lexer and parser |
| `ndc_fmt` | Code formatter (`ndc fmt`): lays the AST out as a Wadler/Prettier-style document and prints it within 120 columns, keeping the source's tokens, parentheses and comments; re-parses its output to check the program is unchanged |
| `ndc_bin` | CLI entry point, REPL, terminal colours for the highlighting |
| `ndc_wasm` | The interpreter, highlighter and language features (`ndc_lsp` without its server) for the browser via wasm-bindgen, used by the manual's playground; its editor is bundled from `manual/editor/` |

### Key Concepts

**Single execution path** — The bytecode VM in `ndc_vm` is the only execution path. `ndc_interpreter` acts as a facade: it runs the semantic analyser, compiles to bytecode via `ndc_vm::compiler::Compiler`, and executes via `ndc_vm::Vm`.

**Value type** — `ndc_vm/src/value/mod.rs` defines the one runtime `Value`. It is constrained to 16 bytes: `Int(i64)`, `Float(f64)`, `Number(Rc<AdvancedNumber>)` (BigInt/Rational/Complex), `Bool`, `None`, `Object(Rc<Object>)`, and `Cell` for captured locals.

**Function overloading** — Functions are matched by name and by the number and types of their arguments. The semantic analyser picks one overload for every call before the program runs: `Binding::Resolved`, or `Binding::PerPosition` for an operator on tuples whose positions take different overloads. There is no dispatch at runtime. Binary operators like `+` are parsed as `Expression::OperatorCall`.

**Semantic analyser** — `ndc_analyser/src/analyser/` infers `StaticType` and resolves function bindings: `mod.rs` holds the expression match, with calls, assignments and declarations in their own modules. `StaticType::Unknown` is what nothing settles the type of; only a type parameter or `Any` takes it.

**Natives are made on first use** — The stdlib registers each native as a `NativeSpec` in a `FunctionRegistry` (`ndc_core`): its name, documentation, and closures that make its type and its function. `ndc_vm::Natives` keeps them, with the intrinsics, and makes a native's type or function the first time something asks, so a program pays only for the natives it uses. The analyser reads native types through the `Builtins` trait. Native `i` is global slot `i`; the compiler lists the native slots its code reads on the top-level `CompiledFunction`, and `Vm::new` puts those natives in the globals before the code runs.

**A register VM** — `ndc_vm::Vm` runs `Instr`s (`ndc_vm/src/instr.rs`), 8-byte instructions that name the registers they read and write: `i = i + 1` is one instruction, `AddIntI r0, r0, #1`. A function's locals are its registers, parameters first (`ResolvedVar::Local { slot }` is register `slot`), then the temporaries its expressions need, which the compiler allocates above the locals and frees when the expression is done; `CompiledFunction::frame_size` is how many registers a call takes. A frame is a window of the one value stack, `stack[base..base + frame_size]`, and nothing is pushed or popped while code runs. `Return` sets its frame's registers back to `None`; a temporary that's no longer needed may keep its last value until its register is used again or the function returns. A call doesn't clear the registers of the frame it makes, so they may hold what the caller left in its temporaries: every local is written before it's read, a parameter by the call and any other local by its declaration. A declaration (`let`, a named `fn`, `struct`) is always a statement, which runs before anything after it in its block, and that is all that can read the name; a loop or comprehension variable is written before each run of what it's in.

**Calls** — `Call { base, args }` calls the function in register `base` with the `args` registers after it, which become the callee's first registers: its frame starts at `base + 1`, so the arguments are its parameters without a copy, and `Return` puts the result in `base`. A native's arguments are a slice of the registers (`NativeFunc::Simple`), or are moved out into a `Vec` for a native that takes the VM (`NativeFunc::WithVm`), which may call back into the program (`Vm::call_callback`) in a frame above the running one. A call of a `pure fn` (`Function::Memoized`) or of a `Function::Checked`, and the limit on how deep calls go, are handled in `Vm::dispatch_call` and `Vm::enter_frame`.

**An argument register is the call's own copy** — The compiler never hands a variable's own register to a call: a local passed as an argument is copied into the argument register, and a temporary is computed there. This matters because natives look at reference counts: `a ++ b` appends to `a`'s list in place when nothing else holds it, so passing `a`'s register itself would change `a`. A cast relies on the same count: it gives its value containers of its own, keeping a list, map, deque or heap nothing else holds and copying one something else holds (`Value::unshare`), so its operand register has to be its own copy too, or a variable and the cast's result could hold one list under two types. Instructions either borrow their operands (the native ops, jumps, `TupleIndex`, `IterNext`) or take them, leaving `None` behind (everything that stores a value somewhere else: `MakeList`, `SetGlobal`, `SetCell`, `ListPush`, `ListSet`'s value, `Return`, calls); the compiler gives an instruction that takes a value only a temporary, copying a local into one first.

**Two loops** — `Vm::run_to_depth` is two nested loops. The inner one runs the instructions of one frame with what it needs in locals: the code, the instruction pointer and the frame's registers, borrowed as `&mut [Value; WINDOW]` so a `Reg` (a `u16`) indexes them without a bounds check. It never calls a `&mut self` method; anything that enters or leaves a frame or needs the whole VM (a call, a native that takes the VM, `Return`, `Halt`, an error) breaks out with an `Exit`, and the outer loop writes the instruction pointer back to the frame, does it, and enters the inner loop again for the frame on top. The inner loop never reads a span; the outer one reads the span of the instruction that failed. The loop's generated code is sensitive to small changes (a new arm, a helper that gets inlined), so measure `benches/compare.py` and the integer loop and `fibonacci` with pinned `perf stat` after changing it.

**Calls of natives skip the callee** — The compiler writes a call the analyser resolved to one native as `CallNative { dst, slot, base, args }`, with no function value in a register. A native registered with `NativeSpec::operator` does an `ndc_vm::NativeOp` (Int and Float `+ - *`, Float `/`, comparisons, `==`, unary `-`, `!`, and a list's element at an `Int`, read and written: `xs[i]`, `xs[i] = v`); `Natives::ops` lists them by slot for the compiler, and a call or augmented assignment resolved to one is the op's own instruction (`AddInt r2, r0, r1`, `Less`, …), with an immediate form when the right operand is a small `Int` written in the code (`AddIntI r0, r0, #1`) and a fused compare-and-branch when it's the condition of an `if` or `while` (`JumpIfNotLess r0, r2, +5`). The native, called as a value, runs the same `ndc_vm` code as the instruction.

**Tuple indexes are read in place** — The analyser resolves a tuple indexed by a number written in the code (`t[1]`) to the intrinsic `Intrinsic::TupleIndex` instead of the `[any]` native, and the compiler writes it as `TupleIndex { dst, src, index }`, which reads the element straight from the tuple's register.

**Top-level declarations are globals** — Natives take the first global slots, then every declaration in the program scope (`let`, `fn`, `struct` constructors and accessors) gets the next one, in order, via `ndc_layout::Layout::global_slot`. Functions reach them with `GetGlobal`/`SetGlobal` and never capture them; only locals of functions and of top-level blocks and loops are captured, through cells: the first capture moves the local's value into a `Value::Cell` in its register, which `GetCell`/`SetCell` read and write through, and a closure reaches it as an upvalue (`GetUpvalue`/`SetUpvalue`).

**Persistent REPL** — Each evaluation compiles into its own top-level function and runs on a fresh VM; the value of its last expression goes to a register that `Halt` names. A resumable `Interpreter` keeps the analyser and the globals vector between evaluations, which is how a REPL line sees what earlier lines declared. A resumable `Interpreter` starts the session over after an evaluation that fails past parsing, as the failed code may have done only part of its work; there is no in-process rollback. On Unix, `ndc_bin/src/isolation.rs` instead runs each REPL line in a forked copy of the REPL process, so a line that fails (error, panic, stack overflow, Ctrl-C) leaves the session exactly as it was. `ndc_bin/tests/repl.rs` drives the built binary to test this.

### Test Infrastructure

Add a test for a change in the language's behaviour as an `.ndc` program under `tests/functional/programs/<category>/`, with the directory's numeric prefix, and for a change in what the compiler writes as a program under `tests/compiler/programs/<topic>/`.

Test crates live under `tests/`:

- `tests/functional/` (`functional_tests`) — `.ndc` integration tests and REPL tests. `build.rs` auto-generates one Rust `#[test]` per `.ndc` file under `tests/functional/programs/`, runs via `Interpreter::eval` (VM). Test directives in `.ndc` files:
  ```ndc
  // expect-output: 42      ← assert stdout equals this
  // expect-error: divide   ← assert error message contains this substring
  ```
  It also generates one test per ` ```ndc ` block in the manual (`manual_<page>_<line>`), run by `tests/manual.rs`; see [Manual](#manual).
  `tests/vm.rs` runs the few programs whose cases no program under `programs/` pins (every compare-and-branch, a declaration in a later operand, `x = a and x`, a long map literal's keys) and checks what each prints.
  `tests/stdlib_signatures.rs` calls every native with sample values of the types its signature takes and fails on a runtime error about types, as the type checker trusts the signatures: a native whose signature promises more than it handles fails it.
- `tests/compiler/` (`compiler_tests`) — disassembly snapshots of the bytecode compiler. Each `.ndc` program under `tests/compiler/programs/` starts with a comment saying what it shows and a `// compile:` directive (`parsed`, `analysed` or `stdlib`; several setups separated by commas give a section each), and its expected listing sits next to it as `<name>.txt`, in the format `ndc disassemble` prints without source excerpts. `build.rs` generates one `#[test]` per program. When a compiler change alters a listing on purpose, re-bless with `BLESS=1 cargo test -p compiler_tests` and review the diff of the `.txt` files. Add a program when adding a VM instruction or changing what the compiler emits.
- `ndc_bin/tests/repl.rs` — feeds lines to the built `ndc` binary's REPL (Unix only) and checks what it prints, e.g. that a failed line leaves no trace.
- `tests/proptest/` (`proptest_tests`) — property-based fuzzers. `panic.rs` runs random token streams through the whole pipeline and fails on a panic or a hang; its fresh-fuzz test is `#[ignore]`d, and regression replay (from `panic.regressions`) runs by default. `generated_programs.rs` generates programs that run (bounded loops, functions, closures, lists, …), runs each at the top level, where its variables are globals, and inside a function `main`, where they are registers, and requires the same output, value and error with its span from both: a short run of a few hundred programs runs by default, the long run is `#[ignore]`d. `overloads.rs` stresses the analyser's overload choices. See [CONTRIBUTING.md](CONTRIBUTING.md) for run commands.
