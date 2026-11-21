# Tracing

Andy C++ has built-in tracing support for inspecting how the bytecode VM executes your program. Tracing is available when the binary is compiled with the `trace` feature flag and is controlled via command-line flags on the `run` subcommand.

```sh
cargo run --features trace -- run --trace-print program.ndc
```

Multiple trace flags can be combined in a single invocation.

## Trace modes

### `--trace-print`

Prints every VM instruction to stderr as it is dispatched, with the registers it reads and writes and the corresponding source excerpt. For a loop that adds up `square(i)`:

```
[VM] 0007 ForRangeNext r2, r0, +7         for i in 1..=3 {↵    total += square(i);↵}
[VM] 0008 GetGlobal r5, g639              total
[VM] 0009 GetGlobal r6, g640              square
[VM] 0010 Copy r7, r0                     i
[VM] 0011 Call r6, 1                      square
[VM] 0000 MulInt r1, r0, r0               *
[VM] 0001 Return r1
[VM] 0012 AddInt r4, r5, r6               total += square(i)
[VM] 0013 SetGlobal g639, r4              total
[VM] 0014 Jump -8                         for i in 1..=3 {↵    total += square(i);↵}
```

`r0` is a register of the running function, `g639` a global and `+7` how far a jump goes. This is useful for understanding the exact sequence of bytecode operations your program produces; `ndc disassemble` lists the same instructions without running them.

### `--trace-histogram`

Prints a summary table of how many times each instruction type was dispatched:

```
--------------------------------------------------
Instruction histogram (42 total)
--------------------------------------------------
  GetGlobal                     7  ( 16.7%)
  SetGlobal                     5  ( 11.9%)
  ForRangeNext                  4  (  9.5%)
  LoadK                         4  (  9.5%)
  ...
```

### `--trace-time`

Measures cumulative wall-clock time spent per instruction type and prints a summary:

```
------------------------------------------------------------
Instruction timing (total: 198us)
------------------------------------------------------------
  Call                      125us  ( 63.1%)
  CallNative                 30us  ( 15.3%)
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
