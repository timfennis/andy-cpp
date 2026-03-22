# Tracing

Andy C++ has built-in tracing support for inspecting how the bytecode VM executes your program. Tracing is available when the binary is compiled with the `trace` feature flag and is controlled via command-line flags on the `run` subcommand.

```sh
cargo run --features trace -- run --trace-print program.ndc
```

Multiple trace flags can be combined in a single invocation.

## Trace modes

### `--trace-print`

Prints every VM instruction to stderr as it is dispatched, along with the corresponding source excerpt:

```
[VM] 0000 GetGlobal(2)                    assert_eq
[VM] 0001 GetGlobal(87)                   +
[VM] 0002 Constant(0)                     15
[VM] 0003 Constant(1)                     3
[VM] 0004 Call(2)                         +
```

This is useful for understanding the exact sequence of bytecode operations your program produces.

### `--trace-histogram`

Prints a summary table of how many times each instruction type was dispatched:

```
--------------------------------------------------
Instruction histogram (179 total)
--------------------------------------------------
  Call                         43  ( 24.0%)
  Constant                     44  ( 24.6%)
  GetLocal                     32  ( 17.9%)
  ...
```

### `--trace-time`

Measures cumulative wall-clock time spent per instruction type and prints a summary:

```
------------------------------------------------------------
Instruction timing (total: 184µs)
------------------------------------------------------------
  Call                        69µs  ( 37.4%)
  Constant                    33µs  ( 18.1%)
  ...
```

The time for an instruction is measured from when it starts executing until the next instruction begins.

### `--trace-span`

Renders the source code as a heat map, coloring regions from green (cold) to red (hot) based on how much execution time was spent on the bytecode instructions associated with each source span.

```sh
cargo run --features trace -- run --trace-span program.ndc
```

This gives a visual overview of where your program spends its time. The heat is additive: if a character is covered by multiple overlapping spans (e.g. an expression inside a loop body), all their durations contribute, so hot inner code within a hot loop shows as hotter than the surrounding syntax.

> **Note:** The span-based heat map is a rough profiling aid, not a precise profiler. In particular, recursive function calls can produce misleading results because the function body spans overlap with themselves across call depths, and the timing of `Call` instructions only measures call-setup overhead rather than the total time spent in the callee.

## Building with tracing

Tracing is behind a Cargo feature flag so it has zero cost when not compiled in:

```sh
# Without tracing (default) — no overhead
cargo build

# With tracing support
cargo build --features trace
```

The trace flags only appear in `--help` when compiled with the feature enabled.
