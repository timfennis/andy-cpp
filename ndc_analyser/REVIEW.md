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

