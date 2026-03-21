# ndc_analyser code review (2026-03-21)

## Future work

- **Method call resolution should be type-directed**
  `.foo(args)` is currently sugar for `foo(value, args)` and uses the same name-based resolution.
  A `let` binding like `let len = 300` shadows all `len` function overloads, breaking method calls
  like `"test".len`. Method calls should resolve against function-typed bindings only, or be
  resolved on the receiver's type.

