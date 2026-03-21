# ndc_vm Code Review

Findings from a review of the `ndc_vm` crate. Tackle these one by one.
Severity: **critical** > **major** > **minor** > **nitpick**

---

## 🔴 Major

### ~~P1 — `OpCode` cloned on every dispatch iteration~~ ✅ Fixed
**File:** `chunk.rs:164`, `vm.rs:142` | **Axes:** Performance

`Chunk::opcode()` returns `OpCode` by value, causing a 32-byte `memcpy` (and an
`Rc` refcount bump on `Closure` opcodes) on every single bytecode dispatch — the
hottest path in the VM.

**Fix:** Return `&OpCode` from `Chunk::opcode` and pattern-match by reference in
`run_to_depth`; clone only in the rare `Closure` arm that actually needs ownership.

---

### ~~P2 — `capture_upvalue` is O(n) linear scan~~ ⏭ Skipped
**File:** `vm.rs:499` | **Axes:** Performance

Every closure capture scans `open_upvalues` linearly. In a loop body that creates
many closures this compounds per captured variable.

**Fix:** Keep `open_upvalues` sorted by slot and use `binary_search`. Apply the
same idea to `close_upvalues` (`partition_point`) and `materialize_upvalues_in_args`.

**Decision:** Benchmarked against AoC solutions and the existing bench suite — no
measurable difference. `open_upvalues` is small enough in practice that the linear
scan is faster and clearer to read than the sorted-insert approach. Not worth the
added complexity.

---

### ~~B1 — Integer overflow in `RangeInclusiveIter::size_hint`~~ ✅ Fixed
**File:** `iterator.rs:119` | **Axes:** Bug

```rust
let remaining = (self.end - self.current + 1) as usize;
```

Overflows when `self.end == i64::MAX`. Panics in debug, wraps silently in release.

**Fix:** `(self.end - self.current).saturating_add(1) as usize`

---

### ~~B2 — `frame_pointer - 1` underflows when `frame_pointer == 0`~~ ✅ Fixed
**File:** `vm.rs:176` | **Axes:** Bug

A `Return` from top-level code causes a `usize` underflow (panic in debug, wrap in
release). Currently guarded only by the convention that the compiler always emits
`Halt` rather than `Return` at top level — a convention not enforced at runtime.

**Fix:** Add an explicit guard or use saturating subtraction with an assertion.

---

### ~~R4 — `OverloadSet` hashing is broken~~ ✅ Fixed
**File:** `value/mod.rs:838` | **Axes:** Bug, Idiomatic Rust

All `OverloadSet` values hash to the discriminant byte `10` regardless of contents,
while `PartialEq` compares contents. This violates the `Hash`/`Eq` contract and
would silently corrupt any `HashMap<Value, _>` keyed on an overload set.

**Investigation:** `OverloadSet` is only ever emitted as a transient callee constant
consumed immediately by `Call` — `get_binding_any` always returns a single `Resolved`
binding, so OverloadSets cannot currently reach map keys through normal NDC code.

**Fix:** Both `Hash` and `PartialEq` now panic with `"OverloadSet cannot be used as a
map key"`, making any future regression loudly visible rather than silently corrupting.

---

## 🟡 Minor

### ~~P3 — `close_upvalues` makes two passes~~ ✅ Fixed
**File:** `vm.rs:514` | **Axes:** Performance

Close-then-`retain` traverses `open_upvalues` twice and borrows every cell twice.

**Fix:** Single `retain_mut` pass that both closes and filters simultaneously.
**Result:** ~12% speedup on closure-heavy benchmarks.

---

### ~~P4 — `vectorization_pairs` eagerly allocates even when vectorization fails~~ ✅ Fixed
**File:** `vm.rs:1003` | **Axes:** Performance

Builds a `Vec<(Value, Value)>` with full element clones before the type/shape check
can fail. Every `Rc` clone bumps a refcount even if the vec is immediately dropped.

**Fix:** Inline shape check in `try_vectorized_call`: build a two-element probe to
confirm overload match, then call `vectorization_pairs` only once confirmed.

---

### ~~P5 — `candidates` cloned on every 2-arg dynamic dispatch~~ ✅ Fixed
**File:** `vm.rs:836` | **Axes:** Performance

```rust
Object::OverloadSet(candidates) => candidates.clone(),
```

Clones `Vec<ResolvedVar>` unconditionally for every dynamic binary call.

**Fix:** Scoped the shared borrows so `candidates: &[ResolvedVar]` is borrowed from
the stack instead; all borrows drop before the `&mut self` calls below.

---

### ~~P6 — `StringIter::size_hint` is O(n)~~ ✅ Fixed
**File:** `iterator.rs:411` | **Axes:** Performance

`remaining.chars().count()` scans the entire remaining string on every `size_hint`
call (used by `len()` in the `Display` impl).

**Fix:** Return `(0, None)` or a byte-length lower bound; exact UTF-8 code-point
count cannot be O(1).

---

### ~~P7 — `MapIter` snapshots the full map at creation~~ ✅ Fixed
**File:** `iterator.rs:239` | **Axes:** Performance

Eagerly clones all entries into a `Vec` even when the caller only needs the first few.

**Fix:** Hold `Rc<Object>` and iterate lazily (collect keys on creation, clone values
on demand), matching the approach used by `SeqIter`.

---

### P8 — `MinHeapIter` iterates then sorts separately
**File:** `iterator.rs:274` | **Axes:** Performance

Clones all elements into a `Vec` then sorts. Draining a cloned `BinaryHeap` via
repeated `pop()` yields elements in sorted order with no extra sort pass.

---

### ~~B3 — User-reachable panics in `exec_unpack`~~ ✅ Fixed
**File:** `vm.rs:875, 959` | **Axes:** Bug

```rust
panic!("expected a tuple or list to unpack")
```

This code path is reachable from user input (any value passed to the `Unpack`
opcode that the analyser failed to reject). Should return `VmError` instead.

---

### ~~B4 — `UnboundedRangeIter` wraps silently at `i64::MAX`~~ ✅ Fixed
**File:** `iterator.rs:149` | **Axes:** Bug

`self.current += 1` overflows in release builds, silently producing negative values.

**Fix:** Use `checked_add(1)` and return `None` (or error) on overflow.

---

### ~~B5 — `materialize_upvalues_in_args` can close a still-live upvalue cell~~ ✅ Fixed
**File:** `vm.rs:533` | **Axes:** Bug

Closes upvalue cells referenced by argument closures while the originating frame is
still live. If another closure in that frame shares the same cell, subsequent
`SetUpvalue` calls write to the closed copy while the stack slot drifts out of sync.

---

### ~~B6 — `resolve_callee` panics on unexpected `Object` variants~~ ✅ Fixed
**File:** `vm.rs:766` | **Axes:** Bug

```rust
obj => panic!("callee is unexpected object type: {:?}", std::mem::discriminant(obj)),
```

A non-function `Object` in the callee slot (e.g. from a miscompiled `OverloadSet`)
produces an uncontrolled panic rather than a `VmError`.

**Fix:** Return `Err(format!(...))` — the surrounding function already returns
`Result<Option<Function>, String>`.

---

### R1 — `SetUpvalue` clones top-of-stack but never pops
**File:** `vm.rs:307` | **Axes:** Idiomatic Rust

`SetLocal` pops the value after storing; `SetUpvalue` uses `last()` + clone and
leaves the value on the stack. The asymmetry should be documented or resolved.

---

### R5 — `VmCallable` wraps `&mut Vm` in `RefCell` unnecessarily
**File:** `vm.rs:1042` | **Axes:** Idiomatic Rust

`RefCell<&mut Vm>` is used to get interior mutability for `call(&self, ...)`.
This can panic if `borrow_mut()` is called re-entrantly. `call(&mut self, ...)` is
the idiomatic alternative if the macro codegen allows it.

---

### ~~R6 — `Function::static_type()` deep-clones `StaticType` on every call~~ ✅ Fixed
**File:** `value/function.rs:96` | **Axes:** Performance, Idiomatic Rust

`StaticType` is a recursive enum with heap allocations. Every call clones it.

**Fix:** Added `Function::matches_value_args(&[Value])` which skips `static_type()`
entirely for `Any` parameters (the common case for stdlib functions). Changed
`find_overload` to take `&[Value]` directly, eliminating the `Vec<StaticType>`
allocation from every overload-set dispatch. ~6-8% speedup on call-heavy benchmarks
(ackermann, fibonacci, closures).

---

### M1 — `compile_for_block/list/map` have ~180 lines of duplicated scaffolding
**File:** `compiler.rs:748–930` | **Axes:** Modularity

The loop scaffolding (GetIterator, new_loop_context, IterNext, CloseUpvalue,
jump-back, patch breaks, Pop) is copy-pasted across all three variants.

**Fix:** Extract into a shared helper that takes a closure/callback for the inner body.

---

### M2 — `VmIterator::deep_copy` default silently shares mutable state
**File:** `iterator.rs:34` | **Axes:** Modularity

The default `deep_copy` returns `None`, so forgetting to implement it on a new
iterator type causes silent shared mutable state when values are deep-copied
(e.g. passed to a `pure fn`).

**Fix:** Remove the default; make `deep_copy` a required method so new iterator
types fail to compile until they implement it.

---

## 🔵 Nitpick

### RD1 — `compile_if` no-else branch pushes synthetic unit with no comment
**File:** `compiler.rs:564` | **Axes:** Readability

The no-else arm pushes `Value::unit()` but the two branches look structurally
identical; the asymmetry is not explained.

---

### RD2 — Dead `else { None }` arm in `dispatch_call`
**File:** `vm.rs:594` | **Axes:** Readability

The `else` path of the memoization check always sets `memo = None`, making the
`let memo = ...` binding misleading. Simplify to `dispatch_call_with_memo(func, args, None)`.

---

### RD3 — `"invalid type"` / `"invalid type 2"` panic messages
**File:** `vm.rs:434` | **Axes:** Readability

Give no diagnostic context. Use descriptive messages like
`"Closure opcode: constant at index {idx} is not a Compiled function"`.

---

### RD4 — Field `max_local` vs method `num_locals()`
**File:** `compiler.rs:15` | **Axes:** Readability

The backing field and its public accessor use different names for the same concept.

---

### M3 — `OutputSink` enum is closed to extension
**File:** `vm.rs:20` | **Axes:** Modularity

Adding a third output destination requires changing the enum and all match arms.
(Low priority — benchmark results show this doesn't matter for performance; noted
for completeness.)
