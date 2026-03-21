# ndc_analyser code review (2026-03-21)

## Medium: Scope resolution correctness

- **Upvalue resolution skips type checking** (`src/scope.rs` ~line 425)
  `resolve_function` and `resolve_function_dynamic` return upvalues without checking if their type
  signature matches the requested argument types. A function `f(Int)` captured as an upvalue would
  be returned even when calling `f("hello")`, potentially resolving to the wrong overload.

- **`resolve_function_dynamic` stops at first scope with candidates** (`src/scope.rs` ~line 299)
  If an inner scope has one overload `f(Int)` and an outer scope has `f(String)`, the inner scope's
  candidates are returned and the outer scope is never checked. This may be intentional (shadowing)
  but differs from `get_all_bindings_by_name` which collects across all scopes. Document whether
  this is by design or should collect across scope boundaries.

- **Index operators resolved with empty arg types** (`src/analyser.rs` ~line 427)
  `[]` and `[]=` are resolved with `&[]` (no argument types), so the analyser can never produce
  `Binding::Resolved` for index operations — it always falls through to `Binding::Dynamic`. Add a
  comment explaining this is intentional, or pass actual types for better static resolution.

## Medium: Performance

- **Triple scope walk for function resolution** (`src/scope.rs` ~line 338)
  `resolve_function_binding` tries `resolve_function`, then `resolve_function_dynamic`, then
  `get_all_bindings_by_name` — each performing a full scope walk. For dynamic dispatch cases (e.g.,
  operators with `Any` types), this means 2–3 redundant traversals. Merging into a single pass that
  collects exact matches, loose matches, and all-by-name simultaneously would improve compile-time
  performance for programs with many overloads.

## Medium: Code duplication

- **Scope-walking loop repeated 5 times** (`src/scope.rs`)
  `get_binding_any`, `resolve_function_dynamic`, `get_all_bindings_by_name`, `resolve_function`,
  and partially `find_type_by_slot` / `find_scope_owning_slot` all share the same structure: walk
  up the scope chain, check each scope, track `env_scopes` for upvalue hoisting, fall through to
  globals. Extracting a generic scope-walk iterator or visitor would eliminate ~100 lines.

- **Duplicated lub-fold pattern** (`src/analyser.rs`)
  The pattern of folding types via `lub` appears in `analyse_multiple_expression_with_same_type`
  (list elements), map key accumulation (~line 228), and map value accumulation (~line 234). A small
  helper `fn analyse_lub(&mut self, exprs) -> Option<StaticType>` could unify these.

- **`resolve_lvalue` and `resolve_single_lvalue` overlap** (`src/analyser.rs` ~line 397)
  Both resolve an lvalue's identifier binding. The `Identifier` and `Index` arms are near-identical;
  they differ only in that `resolve_single_lvalue` returns the type and errors on `Sequence`. Consider
  having `resolve_lvalue` return `Option<StaticType>` to avoid the duplication.

## Small: Cleanup

- **`debug_assert!(false)` → `unreachable!`** (`src/scope.rs` ~line 98)
  A variadic function match should be impossible at this call-site. Replace with `unreachable!`
  once confident the invariant holds.

- **`new_iteration_scope` identical to `new_block_scope`** (`src/scope.rs` ~line 46)
  Both constructors produce identical `Scope` values. Merge them, or add a comment explaining why
  the distinction exists for future work (e.g., break/continue scoping).

- **Anonymous slot uses magic string `"\x00"`** (`src/scope.rs` ~line 456)
  `reserve_anonymous_slot` uses `"\x00"` as a sentinel name. An `Option<String>` name field would
  be more explicit, though this works since the lexer never produces null bytes.

- **Stale comment on `Return` analysis** (`src/analyser.rs` ~line 255)
  Remove the "Actually it doesn't seem to make it any easier" comment.

- **Commented-out debug println** (`src/analyser.rs` ~line 287)
  Remove `// println!("resolve fn {name} {}", ...)`.

- **Unnecessary clone of `StaticType::Any`** (`src/analyser.rs` ~line 490)
  `resolved_type.clone()` on a unit variant — just push `StaticType::Any` directly.

- **Missing comment on `from_global_scope` dual-root design** (`src/scope.rs` ~line 145)
  `global_scope` is separate from `scopes[0]` — add a comment explaining why there are two root
  scopes (one for natives, one for user top-level code).
