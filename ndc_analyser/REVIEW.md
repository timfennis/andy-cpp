# ndc_analyser code review (2026-03-21)

## Medium: Scope resolution correctness

- **Index operators resolved with empty arg types** (`src/analyser.rs` ~line 427)
  `[]` and `[]=` are resolved with `&[]` (no argument types), so the analyser can never produce
  `Binding::Resolved` for index operations — it always falls through to `Binding::Dynamic`. Add a
  comment explaining this is intentional, or pass actual types for better static resolution.

- **Method call resolution should be type-directed** (future work)
  `.foo(args)` is currently sugar for `foo(value, args)` and uses the same name-based resolution.
  A `let` binding like `let len = 300` shadows all `len` function overloads, breaking method calls
  like `"test".len`. Method calls should resolve against function-typed bindings only, or be
  resolved on the receiver's type.

