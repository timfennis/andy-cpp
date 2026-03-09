# SetUpvalue Implementation Plan

## Problem
The VM cannot mutate upvalues (closed-over variables). Test `005_functions/004_closures.ndc` fails because `add_one` assigns to `n` and `invoked` which are upvalues.

## What's Done (in working tree, uncommitted)

### 1. Added `SetUpvalue { slot, depth }` opcode (`chunk.rs`)
Mirrors `GetUpvalue`. Added to `OpCode` enum and `Debug` impl.

### 2. Compiler emits `SetUpvalue` for upvalue assignments (`compiler.rs:132`)
Was `todo!("?")`, now emits `OpCode::SetUpvalue { slot, depth }`.

### 3. VM handler for `SetUpvalue` (`vm.rs`)
Reads top of stack (without popping -- matches `SetLocal` semantics), then:
- `UpvalueCell::Open(stack_slot)` -> writes to the stack slot
- `UpvalueCell::Closed(stored)` -> writes to the stored value

### 4. `capture_upvalues` processes both Get and Set (`chunk.rs`)
Changed the opcode filter from matching only `GetUpvalue` to matching both `GetUpvalue | SetUpvalue`.

### 5. `is_closure` detection (`compiler.rs:164-167`)
Was only checking for `GetUpvalue`. Will be replaced entirely (see plan below).

## Remaining Problem: Multi-Level Upvalue Hoisting

The simple SetUpvalue works for depth=1 (capturing from immediate parent). The test fails at **runtime** because of depth=2 captures:

```
let invoked = 0;        // top-level local, slot 0
fn counter() {          // depth 1
  let n = 0;
  fn add_one() {        // depth 2
    n = n + 1;          // Upvalue { depth: 1, slot: ? } -- works fine
    invoked = invoked + 1; // Upvalue { depth: 2, slot: 0 } -- PROBLEM
  }
}
```

### The bug
For `add_one`, `invoked` at depth=2 becomes `CaptureFrom::Upvalue(0)`, meaning "get upvalue index 0 from parent (`counter`)". But `counter` itself never captures `invoked` -- it has no upvalues at all! So `counter`'s closure has an empty upvalue array, causing an index-out-of-bounds panic.

### Why the old approach (compiler post-pass hoisting) is bad
The original plan was to modify `capture_upvalues` to accept a `&mut Vec<CaptureFrom>` for the parent and hoist during the post-pass. This requires threading `extra_captures` through the compiler, seeding `capture_upvalues` with initial values, and passing parent capture lists down. It's brittle and hard to reason about because information flows backwards (child -> parent) through state bags.

## Chosen Approach: Analyser-Side Hoisting

Move all upvalue chain resolution into the analyser/scope tree. The analyser already knows about scoping and walks intermediate function boundaries. By hoisting there, the compiler never sees `depth > 1` and becomes much simpler.

### Design overview
- The analyser builds upvalue chains when resolving variables at depth > 1
- Each intermediate function scope gets an `upvalues` list recording what it captures
- `ResolvedVar::Upvalue` simplifies to just `{ index }` (into the function's captures list)
- `FunctionDeclaration` gains a `captures: Vec<CaptureSource>` field in the AST
- The compiler reads captures directly from the AST -- no post-pass needed
- `capture_upvalues()` and `depth` on opcodes go away entirely

### Step 1: Change `ResolvedVar` and add `CaptureSource` (`ndc_parser/src/expression.rs`)

This is the best starting point -- it will cause compiler errors everywhere `ResolvedVar::Upvalue` is pattern-matched, giving you a checklist of call sites to update.

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaptureSource {
    Local(usize),        // capture parent's local at this slot
    Upvalue(usize),      // capture parent's upvalue at this index
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ResolvedVar {
    Local { slot: usize },
    Upvalue { index: usize },   // index into this function's captures list
    Global { slot: usize },
}
```

Add `captures: Vec<CaptureSource>` to `Expression::FunctionDeclaration`.

### Step 2: Add upvalue tracking to `Scope` (`ndc_interpreter/src/semantic/scope.rs`)

Add an `upvalues` field to `Scope` (only meaningful for function scopes):

```rust
struct Scope {
    // ... existing fields ...
    upvalues: Vec<(String, CaptureSource)>,
}
```

With a dedup-aware helper:

```rust
fn add_upvalue(&mut self, name: &str, source: CaptureSource) -> usize {
    if let Some(idx) = self.upvalues.iter().position(|(n, _)| n == name) {
        return idx;
    }
    self.upvalues.push((name.to_string(), source));
    self.upvalues.len() - 1
}

fn find_upvalue(&self, name: &str) -> Option<usize> {
    self.upvalues.iter().position(|(n, _)| n == name)
}
```

### Step 3: Update resolution functions to hoist (`scope.rs`)

All 4 resolution functions follow the same pattern. Here's `get_binding_any` as the template:

```rust
fn get_binding_any(&mut self, ident: &str) -> Option<ResolvedVar> {
    let mut scope_ptr = self.current_scope_idx;
    let mut env_scopes: Vec<usize> = vec![];  // function scopes crossed (inner to outer)

    loop {
        if let Some(slot) = self.scopes[scope_ptr].find_slot_by_name(ident) {
            if env_scopes.is_empty() {
                return Some(ResolvedVar::Local { slot });
            }
            let index = self.hoist_upvalue(ident, slot, &env_scopes);
            return Some(ResolvedVar::Upvalue { index });
        }
        // Check if this scope already has an upvalue for it
        // (could have been registered by a sibling closure's resolution)
        if let Some(uv_idx) = self.scopes[scope_ptr].find_upvalue(ident) {
            if env_scopes.is_empty() {
                return Some(ResolvedVar::Upvalue { index: uv_idx });
            }
            let index = self.hoist_from_upvalue(ident, uv_idx, &env_scopes);
            return Some(ResolvedVar::Upvalue { index });
        }

        if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
            if self.scopes[scope_ptr].creates_environment {
                env_scopes.push(scope_ptr);
            }
            scope_ptr = parent_idx;
        } else {
            return Some(ResolvedVar::Global {
                slot: self.global_scope.find_slot_by_name(ident)?,
            });
        }
    }
}
```

The 4 functions to update:
- `get_binding_any` -- general variable lookup
- `resolve_function` -- typed function lookup
- `resolve_function_dynamic` -- returns multiple candidates
- `get_all_bindings_by_name` -- returns all same-named bindings

The key helper that builds the chain:

```rust
fn hoist_upvalue(&mut self, name: &str, local_slot: usize, env_scopes: &[usize]) -> usize {
    // env_scopes is [innermost, ..., outermost]
    // Walk from outermost to innermost, building the capture chain
    let mut capture_idx = local_slot;
    let mut is_local = true;

    for &scope_idx in env_scopes.iter().rev() {
        let source = if is_local {
            CaptureSource::Local(capture_idx)
        } else {
            CaptureSource::Upvalue(capture_idx)
        };
        capture_idx = self.scopes[scope_idx].add_upvalue(name, source);
        is_local = false;  // subsequent scopes capture from parent's upvalue, not local
    }
    capture_idx
}
```

Concrete example for the test case:

```
env_scopes = [add_one_scope, counter_scope]  (inner to outer)
Iterating reversed (outer to inner):
  counter:  add_upvalue("invoked", Local(0))    -> counter.upvalues[0]
  add_one:  add_upvalue("invoked", Upvalue(0))  -> add_one.upvalues[1]
                                                    (assuming n was already at index 0)
Returns index 1 -> ResolvedVar::Upvalue { index: 1 }
```

### Step 4: Populate `captures` in the AST (`analyser.rs`)

When the analyser finishes analysing a `FunctionDeclaration` and is about to destroy the function scope, extract the scope's upvalues and store them in the AST node's `captures` field. The analyser already mutates the AST to fill in `resolved` bindings, so this is the same pattern.

### Step 5: Simplify the compiler (`compiler.rs`, `chunk.rs`)

The compiler becomes straightforward:

**Opcodes simplify** -- drop `depth` entirely:
```rust
GetUpvalue(usize),   // index into closure's upvalue array
SetUpvalue(usize),   // index into closure's upvalue array
```

**Reading upvalues:**
```rust
Binding::Resolved(ResolvedVar::Upvalue { index }) => {
    self.chunk.write(OpCode::GetUpvalue(index), span);
}
```

**FunctionDeclaration handler:**
```rust
// captures comes directly from the AST, mapped CaptureSource -> CaptureFrom
let captures: Vec<CaptureFrom> = ast_captures.iter().map(|c| match c {
    CaptureSource::Local(slot) => CaptureFrom::Local(*slot),
    CaptureSource::Upvalue(idx) => CaptureFrom::Upvalue(*idx),
}).collect();
let is_closure = !captures.is_empty();
```

**Delete `capture_upvalues()`** from `chunk.rs` -- it's no longer needed.

### Step 6: Update the VM (`vm.rs`)

`GetUpvalue` and `SetUpvalue` handlers already work with index-based lookup (depth was rewritten to 0 by the old post-pass). They should just work with the new simpler opcodes. Verify they index into `closure.upvalues[slot]` correctly.

## Summary of files to change

| File | Change |
|------|--------|
| `ndc_parser/src/expression.rs` | Add `CaptureSource`, simplify `ResolvedVar::Upvalue`, add `captures` to `FunctionDeclaration` |
| `ndc_interpreter/src/semantic/scope.rs` | Add `upvalues` to `Scope`, `add_upvalue`/`find_upvalue` helpers, update 4 resolution functions, add `hoist_upvalue` |
| `ndc_interpreter/src/semantic/analyser.rs` | Extract scope upvalues into AST when leaving function scope |
| `ndc_vm/src/chunk.rs` | Simplify opcodes (drop `depth`), delete `capture_upvalues()` |
| `ndc_vm/src/compiler.rs` | Use captures from AST, simplify upvalue emission, remove post-pass |
| `ndc_vm/src/vm.rs` | Update opcode handlers for simplified opcodes (likely minimal) |

## Implementation order

1. `ResolvedVar` + `CaptureSource` (step 1) -- compiler errors guide remaining work
2. `Scope` upvalue tracking (step 2)
3. Resolution function updates (step 3) -- the core logic
4. Analyser AST population (step 4)
5. Compiler + chunk simplification (step 5)
6. VM cleanup (step 6)
