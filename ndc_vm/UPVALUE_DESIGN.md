# Upvalue Design: Options for Fixing B5

We have a bug in the VM's upvalue implementation and several meaningfully different ways
to fix it. Looking for input on which direction to take.

---

## Background: how upvalues work today

The VM uses a **Lua-style open/closed upvalue** system. When a closure captures a
variable from an enclosing function, an upvalue cell is created. While the enclosing
frame is still alive the cell is *open* — it holds a stack slot index and reads/writes
go directly to the stack. When the frame exits, `close_upvalues` copies the stack value
into the cell and marks it *closed* (heap-resident). Closures that outlive the frame
then read from their own copy.

```rust
enum UpvalueCell {
    Open(usize),          // stack slot index — cheap, no allocation
    Closed(Value),        // heap copy — frame has exited
}
```

The outer function accesses its own locals via `GetLocal`/`SetLocal` (direct stack
indexing). Inner closures access captured variables via `GetUpvalue`/`SetUpvalue`
(through the cell). While the cell is open, both paths ultimately touch the same stack
slot and stay in sync.

---

## The bug (B5)

Some stdlib higher-order functions (`map`, `filter`, `any`, …) are implemented as
`NativeFunc::WithVm`: they receive `&mut Vm` and call closures back inline. Before
dispatching these, the VM **drains** the arguments off the stack:

```rust
NativeFunc::WithVm(f) => {
    let call_args: Vec<Value> = self.stack.drain(start..).collect();
    self.stack.pop(); // callee slot
    f(&call_args, self)?
}
```

The drain is forced by Rust's borrow checker — you can't hold `&vm.stack[start..]`
(immutable borrow) and `&mut vm` (mutable borrow) simultaneously. (Note: `Simple`
natives, which don't need `&mut Vm`, already avoid the drain and pass a slice directly.)

`materialize_upvalues_in_args` was added to compensate: before the drain it snapshots
any open upvalue cells in the argument closures into `Closed` copies, so the HOF can
call them back safely. Its comment reads: *"Native functions bridge to the tree-walk
interpreter, which may call closures back via `Vm::call_function` in a fresh VM
context."*

That comment is the key, as we'll see below.

**The problem:** when two closures capture the same variable, they share a single
`Rc<RefCell<UpvalueCell>>`. If one of them is passed to a HOF and gets materialized,
the shared cell flips to `Closed` — taking the *other* closure's cell with it. The
outer frame is still alive and continues to use `GetLocal`/`SetLocal` on the original
stack slot. After the HOF returns, the stack slot and the closed cell have diverged:
mutations made by the HOF-called closure went to the closed copy, while the outer
frame's stack slot was never updated.

### Reproducer

```
fn make_counter() {
    let x = 0;
    let inc = fn(v) { x = x + 1 };  // captures x
    let get = fn() { x };            // captures x — shares the same upvalue cell as inc

    [1, 2, 3].map(inc);
    // inc was called 3 times via SetUpvalue -> closed cell now holds 3
    // outer frame's stack slot for x was never updated -> still 0

    print(get())  // prints 3  (reads closed cell)
    print(x)      // prints 0  (reads stack slot)  <- BUG: should also be 3
}
make_counter()
```

The two `print` calls should produce the same value but don't.

---

## Option C — Gemini's suggestion: pass args as a slice, don't drain

Keep args on the stack while the HOF executes. Open upvalue cells remain valid because
the slots they reference are still present.

```rust
// Instead of draining into a Vec...
let result = native_func(vm, &vm.stack[start..]);
vm.stack.truncate(start - 1); // clean up after
```

**Why this is attractive:** `materialize_upvalues_in_args` disappears entirely. No
premature closing, no divergence.

**The problem:** it doesn't compile. The same borrow-checker conflict that forced the
drain in the first place makes this impossible: `&vm.stack[start..]` borrows the stack
immutably, while `vm` is passed mutably. This is exactly the same constraint `Simple`
natives face — and they solve it by *not* taking `&mut Vm` at all. For `WithVm`, which
*needs* `&mut Vm` to call closures back, there is no safe way to also hold a live slice
into the stack at the same time. (Even if you worked around the borrow checker with
`unsafe`, the stack `Vec` can reallocate when the callback pushes frames, invalidating
the raw pointer.)

---

## Option D — Delete `materialize_upvalues_in_args` entirely

This option emerges from reading `call_function` and `call_callback` side by side:

```rust
// Spawns a FRESH VM with an empty stack — used by the interpreter bridge
pub fn call_function(func, args, globals) -> Result<Value, VmError> {
    let mut vm = Self { stack: Vec::new(), ... };
    vm.call_callback(func, args)
}

// Runs INLINE on the same VM — used by VmCallable (all stdlib HOFs)
pub fn call_callback(&mut self, func, args) -> Result<Value, VmError> {
    self.stack.push(Value::unit());
    self.stack.extend(args);
    self.dispatch_call_with_memo(func, ...)?;
    self.run_to_depth(depth)?;
    Ok(self.stack.pop()...)
}
```

All stdlib HOFs use `VmCallable::call`, which delegates to `call_callback`. That runs
inline on the **same VM with the same stack**. After the drain, the outer frame's locals
— including `x` at slot S — are still on the stack. An `Open(S)` upvalue cell is
perfectly valid: when the HOF calls `inc` back via `call_callback`, `inc` pushes its
own frame, accesses `self.stack[S]` through the open cell, and everything stays in sync
without any materialization.

`materialize_upvalues_in_args` is only legitimately needed when callbacks go through
`call_function` (fresh VM, empty stack) — that's the interpreter bridge path, not the
stdlib HOF path. It appears the function was written for the bridge case but is being
applied to all `WithVm` calls, including the stdlib HOFs where it's both unnecessary
and harmful.

**The fix:** remove the `materialize_upvalues_in_args` call (and possibly the function
itself, if no other caller needs it). The open/closed mechanism continues to work
correctly for its intended purpose (frame teardown).

**Scope:** delete ~5 lines at the call site. Verify that no `WithVm` function in the
codebase calls `call_function` internally (rather than `call_callback`/`VmCallable`),
since those would lose their safety net.

**Remaining question:** is there any code path where a `WithVm` HOF ends up calling
back a closure via `call_function` rather than `VmCallable`? If yes, materialization is
still needed on that path (and should be scoped to it). If no, it can be deleted
outright.

---

## Option A — Targeted fix: sync-back after the HOF call

Keep the current architecture. Fix the divergence by writing the closed value back to
the stack slot — and reopening the cell — after the HOF returns.

**How it works:**

1. `materialize_upvalues_in_args` returns the `(cell, stack_slot)` pairs it closed.
2. A new `sync_back_materialized_upvalues` method, called after `dispatch_call`,
   writes `cell.value → stack[slot]`, converts the cell back to `Open(slot)`, and
   re-adds it to `open_upvalues`.

```rust
// call site (simplified)
let synced = if func.needs_arg_materialization() {
    self.materialize_upvalues_in_args(args)
} else {
    vec![]
};
let result = self.dispatch_call(func, args);
self.sync_back_materialized_upvalues(synced);  // restore stack + reopen cells
if let Err(mut e) = result { ... }
```

After sync-back the stack slot has the value the HOF-called closure left in the cell,
the cell is open again, and all subsequent accesses — via `GetLocal` in the outer frame
or `GetUpvalue` in any sharing closure — are back in sync.

**Scope:** ~30–40 lines changed, all inside `vm.rs`. No changes to the compiler,
analyser, or opcode set. Low risk.

**Remaining complexity:** `materialize_upvalues_in_args`, `close_upvalues`,
`open_upvalues`, `UpvalueCell::Open`, and `CloseUpvalue` all remain. The open/closed
dance still exists and could produce similar bugs in future edge cases.

---

## Option B — Architectural fix: box captured variables at declaration

Mark captured variables during the analysis/compilation pass. Instead of starting life
as stack slots and migrating to the heap on frame exit, captured variables are
heap-allocated (`Rc<RefCell<Value>>`) from the moment of declaration. The outer frame
and all inner closures hold a reference to the same heap cell from the start. There is
no open→closed transition.

Non-captured locals are unaffected — they remain cheap stack slots.

**How it works:**

- The analyser already tracks which variables are captured by inner closures. That
  information would be used to tag declarations.
- The compiler emits `GetUpvalue`/`SetUpvalue` for captured variables in the **outer**
  function too, not just in inner closures. Both sides always go through the heap cell.
- The `Open` variant of `UpvalueCell` is deleted. Cells always hold a `Value`.
- `open_upvalues`, `capture_upvalue`, `close_upvalues`, and
  `materialize_upvalues_in_args` are all removed from the VM.
- Loop iteration isolation (currently handled by `CloseUpvalue`) is replaced by the
  compiler emitting "allocate a fresh cell, copy current value" at the top of each loop
  body for any captured loop variable.

**Scope:** touches the analyser, compiler, and VM. Moderate refactor — probably
200–400 lines net, spread across several files.

**Benefit:** B5 and the entire class of open/closed divergence bugs become impossible.
The VM's frame teardown is simpler (no upvalue scanning). `materialize_upvalues_in_args`
disappears completely.

**Cost:** Every access to a captured variable in the outer scope pays a heap
dereference instead of a stack index. In practice this only affects variables that are
actually captured (a small fraction of all locals), but it is a regression on those
paths.

---

## Summary table

|                          | Option D (delete materialize) | Option A (sync-back)         | Option B (box at declaration)        | Option C (slice, no drain)   |
|--------------------------|-------------------------------|------------------------------|--------------------------------------|------------------------------|
| Lines changed            | ~5, one file                  | ~30–40, one file             | ~200–400, multiple files             | Not implementable as stated  |
| Risk                     | Low (needs audit)             | Low                          | Medium                               | —                            |
| Fixes B5?                | Yes                           | Yes                          | Yes                                  | Yes (if implementable)       |
| Eliminates bug class?    | Removes the cause directly    | No — same mechanism remains  | Yes — open/closed dance gone         | Removes the cause directly   |
| Runtime cost             | None                          | None                         | Heap deref for captured vars         | —                            |
| Removes VM complexity    | Partially                     | No                           | Yes (open_upvalues, close_upvalues…) | Partially                    |
| Key prerequisite         | Audit all WithVm HOFs         | None                         | Analyser + compiler changes          | Rust borrow rules            |

---

## Questions for reviewers

1. Is Option D safe? Are there any `WithVm` HOFs that call back closures via
   `call_function` (fresh VM) rather than `VmCallable`/`call_callback` (inline)?
2. If Option D is safe, does it fully fix B5, or are there other paths that still
   require materialization?
3. Is Option B's architectural simplification worth the refactor cost and the
   performance regression on captured-variable access?
4. Are there other edge cases in the current open/closed mechanism (beyond B5) that
   make Option B more urgent regardless of the B5 fix chosen?
