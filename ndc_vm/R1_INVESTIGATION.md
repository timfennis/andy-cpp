# R1 — `SetUpvalue` stack-effect mismatch with `SetLocal`

## Summary

`SetUpvalue` **peeks** the top of the stack (clone, no pop), while `SetLocal` **pops** it.
The compiler treats them identically via `emit_set_var`, assuming both consume the top
value. This causes a stack leak: one phantom value per upvalue assignment. The leak is
currently masked by `Return`, which truncates the entire frame's stack, so it doesn't
produce wrong results — but it does cause unbounded stack growth inside loops that
reassign captured variables.

---

## Context: how this VM works

This is a stack-based bytecode VM for a custom language. Functions are closures. Variables
live either in **local stack slots** (within the current call frame) or in **upvalue cells**
(captured variables from an enclosing scope). Upvalue cells are `Rc<RefCell<UpvalueCell>>`,
where `UpvalueCell` is either `Open(stack_slot_index)` or `Closed(Value)`.

The compiler emits `SetLocal` for local variable writes and `SetUpvalue` for captured
variable writes. Both are emitted from the same helper:

```rust
// compiler.rs:532
fn emit_set_var(&mut self, var: ResolvedVar, span: Span) {
    match var {
        ResolvedVar::Local { slot } => self.chunk.write(OpCode::SetLocal(slot), span),
        ResolvedVar::Upvalue { slot } => self.chunk.write(OpCode::SetUpvalue(slot), span),
        ResolvedVar::Global { .. } => unreachable!("globals are native, never assigned"),
    };
}
```

The compiler **assumes both instructions have the same stack effect**: consume (pop) the
top-of-stack value and store it.

---

## The bug

### `SetLocal` (vm.rs:195) — pops

```rust
OpCode::SetLocal(slot) => {
    let slot = *slot;
    let value = self.stack.pop().expect("stack underflow");
    if frame.slot(slot) < self.stack.len() {
        // Reassignment: write to existing slot, stack shrinks by 1
        self.stack[frame.slot(slot)] = value;
    } else {
        // Declaration: slot IS the top, pop+push is a no-op
        self.stack.push(value);
    }
}
```

For **reassignment** (the case relevant here — declarations go through a separate compiler
path that only ever emits `SetLocal`), `SetLocal` pops the value and writes it into an
existing lower slot. **Net stack effect: -1.**

### `SetUpvalue` (vm.rs:312) — peeks

```rust
OpCode::SetUpvalue(slot) => {
    let slot = *slot;
    let value = self.stack.last().expect("stack underflow").clone();
    let mut cell = frame.closure.upvalues[slot].borrow_mut();
    match &mut *cell {
        UpvalueCell::Open(stack_slot) => self.stack[*stack_slot] = value,
        UpvalueCell::Closed(stored) => *stored = value,
    }
}
```

Clones the top value and writes it into the upvalue cell. The original value **remains on
the stack**. **Net stack effect: 0.**

### Compiled assignment pattern

A simple assignment `x = expr` where `x` is an upvalue compiles to:

```
compile_expr(expr)          — pushes result          [... result]
SetUpvalue(slot)            — peeks, writes to cell  [... result]     ← value stays!
Constant(unit)              — pushes ()              [... result, ()]
```

The surrounding code (statement in a block) then pops the expression result:

```
Pop                         — pops ()                [... result]     ← leaked!
```

Compare to the local case:

```
compile_expr(expr)          — pushes result          [... result]
SetLocal(slot)              — pops, writes to slot   [...]
Constant(unit)              — pushes ()              [... ()]
Pop                         — pops ()                [...]            ← clean
```

### Evidence from disassembly

`add_one` function from `tests/programs/005_functions/004_closures.ndc`:

```
== add_one ==
0000  GetGlobal(81)        +
0001  GetUpvalue(0)        n
0002  Constant(0)          1
0003  Call(2)              n + 1            stack: [result]
0004  SetUpvalue(0)        n = ...          stack: [result]       ← leaked!
0005  Constant(1)          ()               stack: [result, ()]
0006  Pop                                   stack: [result]       ← leaked value remains
0007  GetGlobal(81)        +
0008  GetUpvalue(1)        invoked
0009  Constant(2)          1
0010  Call(2)              invoked + 1      stack: [result, result2]
0011  SetUpvalue(1)        invoked = ...    stack: [result, result2]   ← leaked!
0012  Constant(3)          ()               stack: [result, result2, ()]
0013  Pop                                   stack: [result, result2]   ← two leaked values
0014  GetUpvalue(0)        n                stack: [result, result2, n]
0015  Return                                ← truncates entire frame, hiding the leak
```

### Why it doesn't produce wrong results (yet)

`Return` (vm.rs:175) does `self.stack.truncate(frame_pointer - 1)`, which wipes the
entire frame's stack slots regardless of how many phantom values accumulated. So the
leaked values are cleaned up and never observed by the caller.

### Why it's still a real bug

1. **Unbounded stack growth in loops.** A `for` loop inside a closure that reassigns a
   captured variable leaks one value per iteration. For long-running loops this is a
   memory leak proportional to iteration count.

2. **Fragile invariant.** The compiler's stack-depth tracking is wrong for upvalue
   assignments. Any future optimisation that relies on knowing the stack depth (e.g.
   stack-slot reuse, register allocation, or stack-depth assertions) will break.

3. **Unnecessary clone.** `SetUpvalue` clones the top value even though the compiler
   doesn't use the original afterward. The clone is wasted work (and an `Rc` refcount
   bump for heap-allocated values).

---

## The question: what should the fix be?

There are two possible approaches. I'd like your opinion on which is cleaner.

### Option A: Make `SetUpvalue` pop (match `SetLocal`)

Change `SetUpvalue` to pop instead of peek:

```rust
OpCode::SetUpvalue(slot) => {
    let slot = *slot;
    let value = self.stack.pop().expect("stack underflow");
    let mut cell = frame.closure.upvalues[slot].borrow_mut();
    match &mut *cell {
        UpvalueCell::Open(stack_slot) => self.stack[*stack_slot] = value,
        UpvalueCell::Closed(stored) => *stored = value,
    }
}
```

**No compiler changes needed.** The compiler already assumes both instructions pop. This
is the minimal fix.

**Concern:** When the upvalue cell is `Open(stack_slot)`, we pop from the top and write
to a *different* stack position. This is fine — we're moving the value, not duplicating
it. But it means `SetUpvalue` for an open upvalue does: pop top → write to stack[slot].
Is that clean, or is it weird that we pop from one stack position and write to another?
(Note: `SetLocal` does the exact same thing for reassignment — pops top, writes to a
lower slot — so this is consistent.)

### Option B: Keep `SetUpvalue` as peek, emit `Pop` in the compiler

Add `OpCode::Pop` after every `SetUpvalue` emission in `emit_set_var`:

```rust
fn emit_set_var(&mut self, var: ResolvedVar, span: Span) {
    match var {
        ResolvedVar::Local { slot } => self.chunk.write(OpCode::SetLocal(slot), span),
        ResolvedVar::Upvalue { slot } => {
            self.chunk.write(OpCode::SetUpvalue(slot), span);
            self.chunk.write(OpCode::Pop, span);
        }
        ResolvedVar::Global { .. } => unreachable!("globals are native, never assigned"),
    };
}
```

**No VM changes needed.** The peek semantics stay, but the compiler compensates.

**Concern:** This adds an extra instruction to every upvalue assignment. It also means the
two instructions have *different* documented stack effects, which is a footgun for anyone
reading the opcode definitions. Every call site of `emit_set_var` would need to be
audited to confirm the extra `Pop` is correct in context (it should be, since the
compiler already assumes the value is consumed).

### Option C: Something else?

Is there a third approach I'm missing? For example, should assignments be
expression-valued (returning the assigned value) rather than statement-valued (returning
unit)? That would change the semantics but might simplify the compiler. Currently NDC
assignments always produce `()` as their expression value.

---

## Call sites that emit `SetUpvalue` (via `emit_set_var`)

For completeness, here are all the compiler locations where `emit_set_var` can emit
`SetUpvalue`:

1. **Simple assignment** (`x = expr`, compiler.rs:174-178):
   `compile_expr → SetUpvalue → Constant(unit)`

2. **Op-assignment with dynamic binding** (`x += expr`, compiler.rs:237):
   `compile_binding → GetUpvalue → compile_expr → Call(2) → SetUpvalue`
   followed by `Constant(unit)` at line 293.

3. **Op-assignment with no in-place op** (`x += expr`, compiler.rs:244):
   Same pattern as (2).

In all three cases, the compiler expects `SetUpvalue` to consume the top value, then
pushes `Constant(unit)` as the expression result.
