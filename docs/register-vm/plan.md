# Register VM migration plan

Status: plan, written 2026-11-19 against master `8b6437be`. Work started 2026-11-19 on `feat/register-vm`.

Progress:

- Step 0 is done (#494 benchmark script, #495 cast check removed on the branch, #496 characterisation tests, #497 compiler snapshots).
- Steps 1 and 2 are written (`ndc_vm::register`) and **the first go/no-go passed**: on the same binary, the integer loop of appendix A takes 2,660 ms on the stack VM and 279 ms on the register VM (9.5x; the gate was 3x), and `fib(32)` 795 ms and 295 ms (2.7x; the gate was 1.3x). Without immediates and fused branches the figures are 6.3x and 2.3x. Safe Rust, `u16` registers, about 2 ns per instruction, as the prototype predicted.
- Creating a VM costs 61 µs with the 65,536-register window against 3 µs on the stack VM, which is lost in a process's 2 ms start. `u16` registers stay.

This is a guideline for the sessions that do the work. It records what was measured, what other interpreters learned, the design that follows from both, and the order to build it in. Where the plan says "measure", the number decides, not the plan.

## 1. Goal and decisions already made

Replace the stack bytecode and its VM in `ndc_vm` with a register bytecode and VM. The goal is run time. Language behaviour, error messages and error spans must not change, with the two exceptions in section 4.6.

Tim decided these on 2026-11-19:

| Question | Decision |
|---|---|
| How it lands | Long-lived branch `feat/register-vm` with small stacked PRs. Both VMs exist only on the branch, so every test runs on both and the results are compared. The stack VM is deleted before the merge, so master always has one execution path. |
| `unsafe` | Safe Rust by default (`-Dunsafe_code` stays). A small isolated unsafe module is allowed later only if a measurement shows at least 10% on the benchmark set. |
| Compiler tests | `tests/compiler`'s exact-opcode assertions become disassembly snapshots that are re-blessed with one command. |
| Merge gate | Advent of Brian 2025 total and `benches/programs`, each against master: totals at least 1.3x faster, no single program more than 5% slower. |
| Reference counts | The VM doesn't have to reproduce today's counts. The cast check that reads them (`CheckUnshared`) is removed **on the branch, never on master**. The aliasing problem it guards against gets a proper fix on the same branch, after the register VM is done and has passed its gates, so master never loses the check without getting the fix. `++` keeps appending in place. |
| Leftover values in registers | Undecided. Build without clearing them (section 4.6) and adjust if it turns out badly. |

## 2. What was measured

Machine: Tim's, load about 2, rustc 1.99.0, `perf stat -e task-clock`, three runs each. The programs are in the appendix.

### 2.1 Where ndc stands

| Program | ndc master | Python 3.14 | Lua 5.4 | LuaJIT `-joff` |
|---|---|---|---|---|
| Int loop, 20 million iterations | 2,630 ms | 1,245 ms | 254 ms | 134 ms |
| `fib(32)` | 750 ms | 186 ms | 137 ms | 96 ms |

Lua 5.4 and LuaJIT's interpreter are register VMs. Python is a stack VM with specialised instructions.

### 2.2 Where the time goes

The loop body is 18 stack instructions per iteration (360 million in total). `perf stat` on the loop:

- 20.8 billion machine instructions and 9.9 billion cycles: **58 machine instructions and 27 cycles per bytecode instruction**.
- 33,000 branch misses in total. Dispatch prediction is not the problem, so threaded or tail-call dispatch is not the lever.
- The head of the dispatch loop is about 27 machine instructions: it reloads the frame, the prototype and the code pointer, does two bounds checks, copies the 24-byte `Span` of the instruction, and writes `ip` back to the frame.
- The handlers spend their time in `Vec<Value>` push and pop: the length is loaded and stored in memory each time, and each 16-byte `Value` travels through a stack temporary as two overlapping unaligned 8-byte moves (the payload starts at byte 1). 24% of the samples sit on the store of a push, 19% in pop.
- `#[repr(C, u8)]` on `Value`, which aligns the payload, changed nothing (2,634 ms against 2,632 ms). The layout is not the cause.

`fib(32)` makes about 7 million calls at about 17 instructions each, which is again about 6 ns per instruction. Calls are not unusually slow; every instruction is.

### 2.3 Prototype: the same loop on stack and register VMs

`docs/register-vm/prototype.rs` runs the loop on small VMs written in safe Rust, with a 16-byte `Value` that has `Rc` variants, checked arithmetic, and type checks that return errors. All instructions are 8 bytes.

| Variant | Instructions per iteration | Time | Per instruction |
|---|---|---|---|
| Stack, `Vec` push/pop, ndc's exact sequence | 18 | 1,820–2,800 ms | 5.1–7.8 ns |
| Stack, stack pointer in a local, stack as a slice | 18 | 1,530 ms | 4.2 ns |
| Register, plain three-address, registers as a slice | 11 | 1,170 ms | 5.3 ns |
| Register, plain, registers as a fixed window | 11 | 430 ms | 2.0 ns |
| Register, constants kept in registers, window | 7 | 225–250 ms | 1.6–1.8 ns |
| Register, immediates and fused compare-and-branch, slice | 6 | 530 ms | 4.4 ns |
| Register, immediates and fused compare-and-branch, window | 6 | 165–215 ms | 1.4–1.8 ns |

"Window" means the registers of a frame are borrowed as `&mut [Value; N]` and indexed by an operand whose type can't exceed `N` (`u8` with 256, `u16` with 65,536), so the compiler drops the bounds checks. `u8` and `u16` operands measured the same.

What this shows:

1. A tight stack VM with `Vec` push/pop costs what ndc costs today. The dispatch loop's own overhead is not the main cost, which is why the earlier `perf/dispatch-locals-v2` branch gained only 1.02x on this loop.
2. The register form gains twice: about 3x from fewer instructions (18 to 6), and about 3x from each instruction being cheaper (operands read in place, results written in place, no stack length to maintain, no bounds checks).
3. Safe Rust is enough to pass Lua 5.4 on this loop.
4. The first row's range comes from two builds of the same source (cargo with debug info, and plain `rustc`). `loop { match }` codegen is sensitive to small things; wasmi's author reports the same. Always compare binaries built the same way.

Limits of the prototype: one loop, integers only, no calls, no frames, no reference counting traffic. Treat it as the ceiling for arithmetic loops, not a prediction for whole programs.

## 3. What other interpreters learned

Sources are listed in the appendix. Facts marked (reported) come from the project's own write-up.

**How much it gains.** Shi, Gregg, Beatty and Ertl (2005, JVM): 47% fewer executed instructions, 25% larger code, 26–32% less time. Lua 4 to 5 (VM change only): a sum loop took 44% of the time, but `fibo` 72% and library-heavy programs 92–98%. wasmi 0.32 (Rust, safe, `loop { match }`): 80–100% faster overall, **no significant gain on recursive fib**, and compilation 30% slower. Plush (Rust, 2026): 1.55x geometric mean right after the conversion, 1.94x after tuning. Ruby's RTL experiment: twice as fast on a while loop, about 10% on typical programs. Expect 1.5–3x on loops and arithmetic and close to nothing on code that lives in the standard library.

**What everyone does.**

- Fixed-width instructions with the operand kind in the opcode (`ADDI`, `ADDK`, `ADDVN`).
- Immediate operands and fused compare-and-branch. Plush's gains after conversion came from exactly these.
- A dedicated numeric `for` loop instruction pair (Lua `FORPREP`/`FORLOOP`).
- Calls put the arguments in consecutive registers, which become the callee's first registers, so nothing is copied.
- One-pass register allocation with a watermark for temporaries, and the destination of an expression chosen late, so temporaries cost no moves.
- Register indexes, not pointers, or a re-read of the base after anything that can grow the stack. Luau has a stress mode that reallocates the stack aggressively.
- Differential testing against the old engine (wasmi fuzzes against its previous version).

**What went wrong elsewhere.**

- **Reference counts and registers** (Victor Stinner's CPython `registervm`, never merged): values in registers lived until the function returned, which he called the major drawback. Clearing registers with an extra instruction made the code slower than the stack bytecode. Koto (Rust, `Rc` values, register VM) solves it per instruction: operands are borrowed in place, and there are separate "clone this register" and "move out of this register" operations. PHP specialises handlers by operand kind and frees only temporaries.
- **Mixing the conversion with new optimisations** (Stinner): he judged it a mistake. Convert first, optimise after.
- **A long-lived branch** (Ruby RTL): it fell almost a year behind trunk.
- **Constants as registers** (wasmi): constants sat in front of the parameters, so frames could not overlap and parameters were copied. They removed it in 2025. Lua, LuaJIT and Luau use immediate and constant operand opcodes.
- **Tests tied to the compiler's exact output** (wasmi): not sustainable through the conversion.
- **Dispatch**: `become` is still unstable (stabilisation hoped for in 2027) and is slower when compiled to wasm, which the playground needs. CPython's tail-call interpreter gain was corrected from 9–15% to 3–5%. Stay with `loop { match }`.

## 4. Design

### 4.1 What does not change

`Value`, `Object`, `Function`, `NativeOp`, the natives and how they're registered, the analyser, the resolver, `ndc_layout`, `Analysis`, the globals vector, the REPL's one-function-per-line model. The compiler keeps everything it does to decide *what* a piece of code calls (bindings, instances of generic functions, shared prototypes, the REPL `Session`). Only what it emits changes.

### 4.2 Frames and registers

The value stack stays one `Vec<Value>`, but it's a register file: nothing is pushed or popped while code runs.

- A frame is `stack[base .. base + frame_size]`. Register `r` is `stack[base + r]`.
- `ResolvedVar::Local { slot }` is register `slot`. The layout pass already numbers parameters first, then locals, and reuses a block's slots when it ends. Temporaries come after the locals. `CompiledFunction::frame_size` replaces `num_locals` and includes the most temporaries the function ever has at once.
- A register nothing lives in holds `Value::None`. It costs nothing to write and nothing to drop, unlike today's `Value::unit()`, which is an `Rc` clone for every local of every call.
- `Return` sets the registers of the frame it leaves to `Value::None`, as `truncate` drops them today, so a call never has to initialise the callee's frame: it may hold temporaries the caller left there, which is harmless, as every local is written before it's read. Inside a running frame, a temporary that's no longer needed may keep its last value until the register is used again or the function returns (section 4.6).
- The stack only grows. Before a frame runs, the VM makes sure `stack.len() >= base + WINDOW`, filling with `Value::None`.

### 4.3 The dispatch loop

Two nested loops. The inner one runs straight-line code with everything it needs in locals. It never calls a `&mut self` method. The outer one handles anything that changes frames.

```rust
loop {
    // Per frame entry: borrow the frame's code and its register window.
    let frame = self.frames.last().expect("a frame");
    let code: &[Instr] = frame.closure.prototype.body.code();
    let mut ip = frame.ip;
    let regs: &mut [Value; WINDOW] = window(&mut self.stack, frame.base);

    let exit = loop {
        let instr = code[ip];
        ip += 1;
        match instr {
            Instr::AddInt { dst, left, right } => { /* regs only */ }
            Instr::Call { base, args } => break Exit::Call { base, args },
            Instr::Return { src } => break Exit::Return { src },
            // ...
        }
    };

    // The one place the instruction pointer is written back.
    self.frames.last_mut().expect("a frame").ip = ip;
    match exit { /* push or pop a frame, or return the error with its span */ }
}
```

- `code` borrows `self.frames`, `regs` borrows `self.stack`, and `GetGlobal`/`SetGlobal` use `self.globals`: three separate fields, so the borrow checker accepts it, and it rejects any attempt to push a frame while the window is held. The stale-`ip` bug of `perf/dispatch-locals-v2` (a missed `cursor.save`, an endless loop, 27 GB of memory) can't be written in this shape.
- Spans are read only when an error is returned: `body.span(ip - 1)`. The hot loop never touches them.
- `NativeFunc::Simple` natives run inside the inner loop on `&regs[base..base + args]`. `NativeFunc::WithVm` natives leave it.
- The `trace` feature's hook goes in the inner loop under `cfg`, as now.

### 4.4 Instructions

`#[derive(Clone, Copy)] enum Instr`, 8 bytes, with a test that asserts the size (as `ndc_vm/src/lib.rs` does for `OpCode` today).

- Operands: `Reg(u16)`, a constant index, or an `i16` immediate. An opcode byte and three `u16`s fit in 8 bytes.
- What makes `OpCode` 32 bytes and not `Copy` moves to tables on the chunk: `Rc<StaticType>` (casts) and `Rc<[CaptureSource]>` (closures) become indexes.
- The builder keeps symbolic labels and lowers them to offsets at the end, as `OptimizerIr::into_chunk` does.
- Spans stay a parallel `Vec<Span>`.

`u16` registers and a 65,536-slot window (1 MB of `None` beyond the top frame). It avoids a 255-register limit and measured the same as `u8`; step 2 measured what creating that stack costs (see Progress) and kept it.

What step 2 settled about the encoding:

- `CallNative { dst, slot, base, args }` has `args: u8`, as four `u16`s don't fit beside the opcode. A call with more than 255 arguments, or a native slot past `u16`, calls the native as a value with `Call`.
- `Jump` and `JumpIfFalse` take an `i32` offset and never need a wider form. The fused compare-and-branch instructions take an `i16`; when a target is further, the offset is a marker and the target is in a table on the chunk, read only when that jump is taken.
- An immediate is only ever the right operand, so an overflow message names the operands in the order the stack VM does.

**Instruction set for parity (step 3).** One register instruction per thing the stack VM does, nothing clever:

| Group | Instructions |
|---|---|
| Moving values | `Copy { dst, src }` (clones), `Move { dst, src }` (leaves `None` in `src`), `LoadK { dst, k }`, `LoadUnit { dst }`, `MakeString { dst, k }` |
| Variables | `GetGlobal`, `SetGlobal`, `GetUpvalue`, `SetUpvalue`, `GetCell`, `SetCell`, `NewCell`. `GetLocal` and `SetLocal` are gone: a local is its register. |
| Native ops | `AddInt { dst, left, right }` and the other 15 `NativeOp`s |
| Control | `Jump`, `JumpIfFalse { cond, offset }`, `JumpIfTrue { cond, offset }`, `Halt`. They read the condition; there's no `Pop`. |
| Calls | `Call { base, args }`, `CallVec { base, args }`, `CallNative { dst, slot, base, args }`, `Return { src }` |
| Building values | `MakeList { dst, base, len }`, `MakeTuple`, `MakeMap`, `MakeRange`, `Closure { dst, k, captures }`, `Memoize` |
| Iterating | `GetIterator { dst, src }`, `IterNext { iter, dst, offset }`, `ListPush { list, src }`, `MapInsert { map, key, value }` |
| Taking apart | `Unpack { src, base, len }`, `TupleIndex { dst, src, index }` |
| Casts | `CheckType { reg, typ }`, `TryCast { reg, typ }` |

`TupleIndex` on a local reads the local's register in place, so `GetLocalTupleIndex` isn't needed. A list or map literal with more elements than fit in consecutive registers is built with `MakeList` of the first chunk followed by `ListPush`es.

**Added after parity (step 5), one PR each, each measured:**

1. Immediates: `AddIntI { dst, left, imm }`, and the same for `-`, `*` and the comparisons.
2. Conditions compile to jumps and compare-and-branch is fused: `JumpIfNotLess { left, right, offset }` and its immediate forms. `if a < b` never makes a `Bool`.
3. A call of a function known at compile time: `CallGlobal { slot, base, args }`, with no function value in a register.
4. `for i in a..b` over integers without an iterator object: a `ForRangePrep`/`ForRangeLoop` pair. Today each range allocates an `Rc<RefCell<dyn VmIterator>>` and each step is a dynamic call.
5. `IterNext` and `Unpack` write straight into the loop variable's or the pattern's registers.
6. Operand-kind variants of the consuming instructions (section 4.6) where a profile shows the extra `Copy`.

### 4.5 The compiler

One pass over the AST as now, with the destination passed down:

```rust
enum Dest {
    /// The value isn't used: `f(x);`
    Discard,
    /// Put it in this register: an argument, or `let x = ...`
    Reg(Reg),
    /// Anywhere: returns where it is, which for a local is the local's own register.
    Any,
}

fn compile_expr(&mut self, expression: ExpressionLocation, dest: Dest) -> Result<Operand, CompileError>;
```

`Operand` says which register holds the value and whether it's a local (the instruction must borrow or clone it) or a temporary (the instruction may take it).

- `a + b` on two locals is one instruction, `AddInt t, a, b`. Nothing is copied.
- A `;`-statement compiles its expression with `Dest::Discard`. No unit is made and thrown away, so the peephole pass (`Constant; Pop`) and the "every expression leaves one value" rule have nothing left to do. `OpCode::stack_delta` and the depth simulation go too.
- A block gives its last expression the block's own destination. `if`/`else` gives both branches the same destination.
- Temporaries are a watermark above the locals: allocate upward, free back to a mark when the expression is done, remember the highest point for `frame_size`. The `TempSlot`s that are patched in afterwards (`layout_temporaries`) become ordinary temporaries.
- Call arguments are compiled with `Dest::Reg(base + i)`.

### 4.6 Reference counts: one rule, and what may change

Some code looks at how many `Rc` holders a value has:

- `++` appends in place when its left list has one holder, and `++=` moves out of its right list when that has one (`ndc_stdlib/src/list.rs`). An optimisation Tim wants to keep.
- `CheckUnshared`: a cast of a container fails when something else holds it. **Removed in step 0.4, on the branch only.** Tim's decision of 2026-11-19: the reference-count check isn't worth carrying into the new VM, the soundness gap it leaves around `Any` is one no real program hits today, and step 6 fixes the aliasing properly before the branch merges.
- `ref_count(x)`, a debugging function. No test and no manual page uses it.
- `Rc::try_unwrap` and `Rc::unwrap_or_clone` on values a native or instruction owns (unpacking, iterating). Speed only.

The stack VM makes "one holder" mean "a temporary nothing else can see", because reading a local clones it onto the stack. A register VM that hands a local's register straight to `++` would see one holder and **change the local**:

```ndc,norun
let a = [1, 2];
let b = a ++ [3];
print(a);  // must stay [1, 2]
```

**The rule that must hold: an argument register is the call's own copy.** A local passed to a function or native is `Copy`'d into its argument register (two holders); a temporary is computed there (one holder). Never pass a local's register as part of a native's argument slice. This costs nothing: a call needs its arguments in consecutive registers anyway.

Two habits that follow from the instruction set and keep `++` chains appending in place:

- Instructions either borrow or consume. The native ops, jumps, `TupleIndex` and `IterNext` borrow their operands and can't observe a count. Everything that stores a value somewhere else (`MakeList`, `MakeTuple`, `MakeMap`, `SetGlobal`, `SetUpvalue`, `SetCell`, `ListPush`, `MapInsert`, `Return`, calls) moves a temporary out and leaves `None`. For parity, the compiler gives a consuming instruction only temporaries, copying a local first, which is what `GetLocal` does today.
- A native call empties its argument registers when it returns.

**What may change (open: Tim hasn't settled this; build it this way first and adjust if it turns out badly).** A temporary that an instruction only borrowed, as both sides of `f(x) == g(y)`, stays in its register until the register is used again or the function returns. The plan does not clear it: Stinner's CPython experiment found the clearing instructions cost more than they saved. The consequences are accepted:

1. Such a value lives a little longer, so memory is freed later. At the top level, "until the function returns" is the end of the program, though later statements reuse the registers.
2. `ref_count` can report one more than today for a value a leftover register still holds.

`++` is not affected: a leftover register holds either a value nothing else can reach, or one that already had a second holder and would have been copied anyway.

If a benchmark shows memory held too long, add a `Clear { reg }` instruction for that case then, not before.

### 4.7 Calls, natives and callbacks

- `Call { base, args }`: the function value is in `R[base]`, the arguments in `R[base + 1 ..= base + args]`. The callee's frame starts at the caller's `base + 1`, so the arguments are its parameters without a copy. `Return` puts the result in the caller's `R[base]`. This is today's layout (callee slot under the frame), so `dispatch_call`, memoisation, `Function::Checked` and `MAX_CALL_DEPTH` carry over.
- `CallNative { dst, slot, base, args }`: runs the native on `&regs[base..base + args]`, clears those registers, writes the result to `dst`.
- `WithVm` natives get their arguments moved into a `Vec`, as now. Avoiding that allocation is a later optimisation.
- Natives take `&mut ndc_vm::Vm`, which on the branch is the stack VM's type, and `print` is one of them. Step 2 gives them a stack `Vm` without code that holds the output and input. Step 3 has to let a native call back into register code through that same type (`Vm::call_callback`, `VmCallable`), so the register VM's state has to be reachable from what the natives are given. Prefer the arrangement that leaves the least to undo in step 6, when the register VM becomes `ndc_vm::Vm`.
- `Vm::call_callback` puts the callback's frame above the running frame: at `base + frame_size` of the top frame, with a callee slot first. It runs with `run_to_depth` as now.
- The value of the last top-level expression goes to a known register that `Halt` names, which replaces `Vm::last_value`'s "is there something above the locals".
- Keep `CallFrame` small: `memo` and `returns` are rare, so they can share one boxed field.

## 5. Pitfalls, by class

Name the class before patching anything that looks like one of these.

| Class | What goes wrong | Guard |
|---|---|---|
| Lingering temporaries | A dead temporary keeps a big value alive, so memory is freed later | Accepted (section 4.6). Watch peak memory in the benchmark script; add `Clear` only for a measured case. |
| Borrowed local given to a count-sensitive native | `a ++ b` changes `a` | The rule of 4.6; a test per native that reads `strong_count` |
| A local read late | In `x + { x = 5; x }` the left operand is `x`'s register, and the block changes it before the add runs. The stack VM read `x` first. | When a later operand of the same instruction can assign a local (it contains an assignment, an op-assignment or a declaration), copy earlier local operands to temporaries. A declaration counts because a block's registers are reused once it ends: `{ let a = 1; a } + { let b = 2; b }` would otherwise add `b` to itself. A captured local is read through its cell into a temporary anyway, as a call can change it. Tests for binary ops, call arguments, list literals, index expressions. |
| Destination is also an operand | `Move { dst, src }` with both the same, `MakeList { dst, base, .. }` with `dst == base`, a call whose result register is an argument | Read all operands before writing the destination; a unit test per instruction with aliased registers |
| Stale window or `ip` | The loop keeps running the old frame's code after a call | The two-loop shape of 4.3. Run every VM test under `ulimit -v`. |
| Stack too short for the window | A frame near the end of the stack panics on window creation | Reserve on every frame push, including callbacks. A test mode with a tiny initial stack. |
| Offset or register doesn't fit | A huge function, a 300-argument call, a 5,000-element literal | Wide jump fallback, chunked literals, a compile error with a clear message otherwise. One test each. |
| A dead register holds a real value | A compiler bug that reads a dead register gets `None` or a leftover value and carries on | Differential tests against the stack VM. Don't add a `Value` variant for it. |
| Different error text or span | The message names a different operand, or the span is the statement's and not the operator's | Differential tests compare the rendered error, not only success |
| Compile time | wasmi's compiler got 30% slower | Track `Phase::Compiling` from `ExecutionTimings` and the test suite's wall time |
| Calls don't get faster | `fib` gains little while loops gain a lot | Expected. `CallGlobal` (4.4, item 3) and a smaller `CallFrame` (4.7) target it. |
| Codegen swings | A rustc upgrade or an added match arm moves numbers by 10–50% | Build baseline and candidate with the same toolchain and profile, interleave runs |

## 6. Steps

Everything lands on `feat/register-vm`, including step 0, so nothing waits on a merge to master and master keeps the cast check. Every PR follows CLAUDE.md (fmt, clippy, tests, Conventional Commit title with an emoji at the end).

One PR at a time into the branch: open it with the `create-pr` skill, let Codex review it, fix what it finds, squash-merge, then open the next. Don't chain open PRs onto each other (GitHub treats them as a stack and its stack merge is dangerous, see memory `reference-github-stacked-prs`), and don't open a PR from `feat/register-vm` to master until step 6.

While the branch is open, avoid new stack-VM instructions on master: each one has to be ported. Merge master into the branch at least weekly.

### Step 0: groundwork, first on the branch

- **0.1 Benchmark script.** `benches/compare.py BASELINE CANDIDATE` runs `benches/programs/*.ndc` and Advent of Brian 2025 (on a copy) on two binaries: one run of each to check they print the same, then five interleaved runs, the fastest counting. It reports CPU time and peak memory per program, the totals and the merge gate.
- **0.2 Characterisation tests.** `.ndc` tests under `tests/functional/programs` that pin what the migration could change, written and passing on the stack VM first:
  - evaluation order when an operand assigns a variable an earlier operand read (binary ops, call arguments, list, tuple and map literals, index and member targets, op-assignment);
  - `a ++ b` leaves `a` alone; `a ++ b ++ c` and `xs ++= make()` still work;
  - `break`, `continue` and `return` in operand position (extend `005_functions/012_double_return.ndc`);
  - errors raised by the second operand, by a native, inside a callback, and inside a vectorised call, each with its span;
  - recursion to the depth limit.
- **0.3 Snapshot tests.** Replace `tests/compiler/tests/{compiler,optimizer}.rs` with small `.ndc` programs and their expected disassembly as text files next to them, generated into tests by a `build.rs` like `tests/functional` has, and rewritten when an environment variable is set. No new dependency. Do this with today's disassembly before any bytecode changes, so the change to each snapshot is a reviewable diff.
- **0.4 Remove the cast's sharing check.** Delete `OpCode::CheckUnshared`, where `emit_cast` writes it, `Value::shared_container`, and `StaticType::holds_a_mutable_container` if nothing else uses it. The four tests that expect "also held by something else" (`016_casts/034`, `035`, `036`, `039`) move to `998_not_desired` with what the program now does. Update `manual/src/features/casts.md`. The sound replacement is step 6 of this plan; no `TODO` in the code.

### Step 1: skeleton, in a new module beside the old code

`ndc_vm/src/register/` (the name is for the branch; it becomes the only VM in step 6): `Instr` with its size test, the chunk with its side tables, the builder with labels, the disassembler. No behaviour yet.

### Step 2: vertical slice, and the first go/no-go

The VM loop of 4.3 and the compiler of 4.5 for just enough to run the two appendix programs: integer literals, locals, `let`, assignment, native ops, `if`, `while`, function declarations, calls, `return`, `print`.

Measure against master. **Continue only if the loop is at least 3x faster and `fib(32)` at least 1.3x.** If the loop is far from the prototype's numbers, read the assembly of the inner loop (`perf annotate`) before going on: the usual causes are a bounds check that came back, a `Value` copied through the stack, or state reloaded from `self`. Decide `u8` or `u16` registers here.

### Step 3: the rest of the language

Port in this order, a PR each. One test already runs every program under `tests/functional/programs` that the register compiler accepts on both VMs and requires the same outcome, so each step shows up as more programs compared (413 of 927 after step 2).

1. Natives and calls of every kind of function: natives that take the VM and call back into the program, `CallVec`, memoised and checked functions, calling a value that isn't a global.
2. Closures, cells, upvalues, named functions that capture themselves, `pure fn`.
3. All literals, strings, tuples, lists, maps, ranges, logical operators.
4. `for` loops, iterators, guards, list and map comprehensions.
5. Every assignment target: index, member, op-assignment, destructuring, and `Unpack`.
6. Structs, casts, intrinsics.
7. Instances of generic functions and the REPL `Session`.
8. The `trace` feature and `ndc disassemble`.

Helpers that take "the top of the stack" (`convert_top`, `try_cast`, `tuple_element`, `run_native_op`) are changed to take `&mut Value` or `&Value`, so both VMs call the same code.

### Step 4: both backends under every test

- `Interpreter` gets a backend choice (branch only).
- `tests/functional` runs every program on both and compares output, the rendered error and its span. The manual's examples, the REPL tests, `stdlib_signatures` and the proptest regressions run on the register backend.
- The proptest fuzzer gets a differential mode: a generated program must do the same on both.
- **Parity gate: everything green, then the benchmark script.** Fix any program more than 5% slower before optimising anything.

### Step 5: optimisations, only as far as the gate needs

In the order of 4.4's list. Stop when the merge gate is met (totals at least 1.3x, nothing more than 5% slower); the rest go to master after the merge as ordinary PRs. A change that doesn't measure at least 2% on the totals or 10% on some program isn't worth its instructions.

### Step 6: switch and delete

- The register backend becomes the only one. Delete `OpCode`, `OptimizerIr`'s peephole and depth simulation, the stack `Compiler` emission and `Vm` loop, and the backend choice.
- Move `register/` up so the module layout reads as if it had always been this way.
- Update CLAUDE.md and AGENTS.md (architecture, key concepts, test infrastructure), CONTRIBUTING.md, `manual/src/features/tracing.md`, and `bench.sh` if it still refers to anything removed.
- Build the wasm playground and run the manual's examples in it.
- **Fix cast aliasing**, in its own PRs, with the design agreed with Tim first. The four tests moved to `998_not_desired` in step 0.4 say what has to be refused again.
- Run the merge gate one last time against current master, then open the PR to master. Tim merges it.

### Step 7: optional, after the merge

An unsafe experiment (unchecked `code[ip]`, or a raw register base) in one module with `#![allow(unsafe_code)]`, as `ndc_bin/src/isolation.rs` does. Keep it only for at least 10% on the benchmark totals, and only with a stress mode that reallocates the stack on every call.

## 7. Rules for agents doing this work

- Every `cargo` or `ndc` run that executes VM code: `(ulimit -v 16000000; timeout 900 cargo test ...)`. A VM bug can loop and allocate until the machine dies.
- At most two agents building at once. Benchmarks are run serially by the coordinating session after agents finish.
- `perf record` works only with `-e task-clock` and without `--call-graph=dwarf` on Tim's machine. No hyperfine.
- Worktree agents start from master: pass the branch's commit explicitly. The branch moves while an agent works; an agent that rebases can't force-push, so it pushes the rebased work under a new branch name and says which one to use.
- PRs through the `create-pr` skill, short bodies, no test sections.
- If a second special case appears around one cause, stop and propose the design change instead.

## Appendix A: benchmark programs

```ndc,norun
fn run() {
    let i = 0;
    let total = 0;
    while i < 20000000 {
        total = total + i * 2 - 1;
        i = i + 1;
    }
    total
}
print(run());  // 399999960000000
```

```ndc,norun
fn fib(n) {
    if n <= 1 { 1 } else { fib(n - 2) + fib(n - 1) }
}
print(fib(32));  // 3524578
```

Today's bytecode for the loop body (18 instructions per iteration):

```text
0004  GetLocal(0)      0009  GetLocal(1)     0014  Constant(6)    0019  AddInt
0005  Constant(4)      0010  GetLocal(0)     0015  SubInt         0020  SetLocal(0)
0006  Less             0011  Constant(5)     0016  SetLocal(1)    0021  Jump(-18)
0007  JumpIfFalse(14)  0012  MulInt          0017  GetLocal(0)
0008  Pop              0013  AddInt          0018  Constant(8)
```

The same loop after step 5 (6 per iteration; `r0` is `i`, `r1` is `total`, `r2` a temporary, `r5` holds 20,000,000):

```text
loop:  JumpIfNotLess r0, r5, end
       MulIntI       r2, r0, 2
       AddInt        r2, r1, r2
       SubIntI       r1, r2, 1
       AddIntI       r0, r0, 1
       Jump          loop
end:
```

## Appendix B: sources

- wasmi 0.32 (stack to register, Rust): https://wasmi-labs.github.io/blog/posts/wasmi-v0.32 and https://github.com/wasmi-labs/wasmi/issues/1598 (constants as registers)
- The Implementation of Lua 5.0: https://www.lua.org/doc/jucs05.pdf
- Lua 5.4 opcodes and code generator: https://www.lua.org/source/5.4/lopcodes.h.html, https://www.lua.org/source/5.4/lcode.c.html
- Luau performance notes: https://luau.org/performance
- Shi, Gregg, Beatty, Ertl, "Virtual Machine Showdown: Stack Versus Registers": https://www.usenix.org/legacy/events/vee05/full_papers/p153-yunhe.pdf
- Plush's register interpreter (Rust, 2026): https://pointersgonewild.com/2026-09-02-plushs-new-register-based-interpreter/
- Stinner's CPython registervm: https://mail.python.org/pipermail/python-dev/2012-November/122777.html
- Ruby RTL: https://bugs.ruby-lang.org/issues/12589
- Koto's VM (Rust, `Rc` values, register): https://github.com/koto-lang/koto/blob/main/crates/runtime/src/vm.rs
- PHP 7 VM (reference counted, operand kinds): https://www.npopov.com/2017/04/14/PHP-7-Virtual-machine.html
- Rust tail calls: https://trifectatech.org/blog/tail-calls-project-goal/, https://www.mattkeeter.com/blog/2026-04-05-tailcall/
- CPython tail-call correction: https://blog.nelhage.com/post/cpython-tail-call/
