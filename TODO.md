# TODO

Tracked open issues across the codebase. All items here must be resolved before merging — see `CLAUDE.md`.

---

## ndc_analyser

### `src/analyser.rs`

- **Option type inference** (`analyser.rs:42`): `None` literals default to `Option<Any>`. Needs Hindley-Milner or similar to infer the inner type; consider requiring explicit type annotations in the interim.

- **Boolean type checking in Logical expressions** (`analyser.rs:58–59`): Operands of `&&`/`||` are not checked to be `Bool`. Emit an error or warning when a non-Bool operand is detected.

- **Never type for VariableDeclaration** (`analyser.rs:66`): `VariableDeclaration` currently returns `StaticType::unit()`. It should return a bottom/never type since a declaration is not an expression that yields a value.

- **Conflicting function bindings** (`analyser.rs:141`): Function hoisting always creates a new binding; it should error when a binding already exists in the same scope to catch accidental shadowing.

- **Missing-semicolon warning in if-expression** (`analyser.rs:174`): Emit a diagnostic when an if-expression without a semicolon is used in statement position.

- **Branch type mismatch warning** (`analyser.rs:178`): When the true and false branches of an if-expression have different static types, emit a warning.

- **Empty list type inference** (`analyser.rs:225`): `[]` defaults to `List<Any>`. A better solution is needed — either infer from usage context or require a type annotation.

- **Empty map type inference** (`analyser.rs:260`): Empty map literals default to `Map<Any, unit>` for the same reason as empty lists above.

- **Dynamic binding type accuracy** (`analyser.rs:314`): `Binding::Dynamic` returns `Function { parameters: None, return_type: Any }` which is a lie. Ideally the type captures the union of all candidate signatures.

- **For-loop variable type with untyped sequences** (`analyser.rs:342`): When the sequence has no type parameters the loop variable defaults to `Any`. This should improve once all sequence types carry element type parameters.

- **Function parameter type inference** (`analyser.rs:503`): Parameter types in anonymous/lambda functions always default to `Any`. Requires an HM-like inference pass to do better.

### `src/scope.rs`

- **Variadic branch in `find_function_candidates`** (`scope.rs:99`): The variadic branch inside `find_function_candidates` has a `debug_assert!(false, ...)` and an early return. Determine whether this is truly unreachable and replace with `unreachable!()` or propagate a proper error.

---

## ndc_core

### `src/num.rs`

- **Bitwise NOT of non-integers** (`num.rs:181`): Bitwise negation of floats, rationals, and complex numbers silently produces `NaN`, matching Noulith. Decide whether this is the intended behaviour for Andy C++ or whether it should be an error.

- **Division performance** (`num.rs:323`): `Div<&Number>` always converts both operands to rational before dividing, even for simple integer cases. Add an integer fast-path to avoid the allocation.

- **BigInt rounding not downcasting to small int** (`num.rs:584`): The `round_impl!` macro converts `Rational` results to `Int::BigInt` unconditionally. It should attempt to downcast to `Int::Small` first when the value fits.

---

## ndc_lexer

### `src/lib.rs`

- **`consume()` internal error handling** (`lib.rs:202`): `consume()` calls `.expect()` on an empty iterator, which panics. Decide whether to use `unreachable!()` (if this is guaranteed impossible) or propagate a recoverable error.

### `src/number.rs`

- **Error interception after base-2 literals** (`number.rs:48`): The error-interception block after binary literal lexing may be unnecessary since the language has no numeric suffixes. Evaluate and remove or consolidate.

- **Error interception after hex literals** (`number.rs:85`): Same question as above for hexadecimal literals.

- **Error interception after octal literals** (`number.rs:104`): Same question as above for octal literals.

- **Underscore after decimal point** (`number.rs:130`): `_` separators are silently ignored even after a `.` (e.g. `1._5`). Consider making this a lexer error.

- **`validator_for_radix` performance** (`number.rs:231`): Uses string slicing and `str::contains` for each character. Replace with a direct range/char comparison for clarity and speed.

### `src/string.rs`

- **Unicode escape sequences** (`string.rs:72`): `\uXXXX` Unicode escapes are not yet supported in string literals.

- **Byte escape sequences** (`string.rs:73`): `\xFF` byte escapes are unimplemented. Likely intentional (strings are UTF-8) but should be an explicit decision and documented error.

---

## ndc_parser

### `src/parser.rs`

- **Parser ambiguity: if-expression followed by `[`** (`parser.rs:738`): When an if-expression is followed by `[`, the parser cannot distinguish indexing from a list comprehension and emits a generic error. The error message should explain the ambiguity and suggest adding a semicolon.

- **"Expected an expression" error quality** (`parser.rs:1001`): The fallback error message when no expression is found may not describe the situation accurately. Investigate and improve.

---

## ndc_stdlib

### Documentation (`file.rs`, `index.rs`, `list.rs`, `math.rs`, `string.rs`)

- **Missing documentation for stdlib functions**: Many `NativeFunction` registrations have `documentation: None`. The infrastructure for attaching human-readable documentation to stdlib functions needs to be completed so that `docs` queries return useful output. Affected files: `file.rs` (×2), `index.rs` (×2), `list.rs` (×2), `math.rs` (×14), `string.rs` (×1).

### `src/value.rs`

- **`docs()` helper commented out** (`value.rs:11`): The `docs()` function is stubbed and commented out. Complete the implementation once the `NativeFunction` documentation infrastructure is in place.

---

## ndc_vm

Items from the VM code review (`ndc_vm/REVIEW.md`). Severity: **bug** > **performance** > **modularity** > **readability**.

### Bugs

- **`UnboundedRangeIter` wraps at `i64::MAX`** (`iterator.rs:149`, Minor): `self.current += 1` overflows silently in release builds, producing negative values. Use `checked_add(1)` and return `None` on overflow.

- **`resolve_callee` panics on unexpected `Object` variants** (`vm.rs:766`, Minor): An unrecognised `Object` variant in the callee slot triggers an uncontrolled `panic!`. Return `Err(VmError)` instead, consistent with the surrounding `Result`-returning function.

### Performance

- **`MinHeapIter` clones then sorts** (`iterator.rs:274`, Minor): Clones all elements into a `Vec` then sorts. Draining the cloned `BinaryHeap` via repeated `pop()` yields elements in sorted order without the extra sort pass.

### Modularity

- **`compile_for_*` duplicated scaffolding** (`compiler.rs:748–930`, Minor): `compile_for_block`, `compile_for_list`, and `compile_for_map` each duplicate ~180 lines of loop scaffolding (GetIterator → new_loop_context → IterNext → CloseUpvalue → jump-back → patch breaks → Pop). Extract a shared helper that accepts a closure for the loop body.

- **`VmIterator::deep_copy` has a silent default** (`iterator.rs:34`, Minor): The default implementation returns `None`, so any new iterator type that forgets to override `deep_copy` silently shares mutable state when values pass through `pure fn` memoization. Remove the default and make the method required.

- **`OutputSink` closed to extension** (`vm.rs:20`, Low priority): Adding a third output destination requires touching the enum and every match arm. Low urgency; note for future refactors.

### Idiomatic Rust

- **`SetUpvalue` does not pop** (`vm.rs:307`, Minor): `SetLocal` pops the stack after storing; `SetUpvalue` uses `last()` + clone and leaves the value on the stack. The asymmetry should be documented or reconciled.

- **`VmCallable` uses `RefCell<&mut Vm>`** (`vm.rs:1042`, Minor): Interior mutability via `RefCell<&mut Vm>` can panic on re-entrant borrow. Prefer `call(&mut self, ...)` if the macro codegen allows it.

### Readability

- **`compile_if` no-else unit push is unexplained** (`compiler.rs:564`, Nitpick): The no-else arm pushes `Value::unit()` without a comment, making both branches look structurally equivalent when they are not. Add a brief comment.

- **Dead `else { None }` in `dispatch_call`** (`vm.rs:594`, Nitpick): The `else` path unconditionally sets `memo = None`, making the `let memo = ...` binding misleading. Simplify by passing `None` directly.

- **Uninformative panic messages** (`vm.rs:434`, Nitpick): `"invalid type"` and `"invalid type 2"` provide no diagnostic context. Replace with descriptive messages including the opcode name and index.

- **`max_local` vs `num_locals()`** (`compiler.rs:15`, Nitpick): The backing field and its public accessor use different names for the same concept. Rename one to match the other.
