# Return type inference problem and solution

## The problem

The analyser infers function return types from the body expression's type. The body is usually a `Block`, and a block's type is the type of its last expression.

The issue: when a function uses an explicit `return` statement with a semicolon (which is common), the parser wraps it as `Statement(Return { value })`. The `Statement` handler discards the inner type and returns `unit`:

```rust
Expression::Statement(inner) => {
    self.analyse(inner)?;
    Ok(StaticType::unit()) // ← return type lost
}
```

This means a function like:

```ndc
fn make_fn() {
    fn inner() { 42 }
    return inner;    // ← semicolon makes this a Statement(Return)
}
```

...has its return type inferred as `Tuple([])` (unit) instead of `Function([] -> Int)`.

### Why this matters now

Previously this was masked because the function resolution fallback (`find_all_slots_by_name`) included ALL bindings regardless of type. A variable like `let f = make_fn()` with type `unit` would still be included in the dynamic overload set, and the VM would resolve it correctly at runtime.

When we changed function resolution to skip known non-callable types (so that `let foo = 300; foo(42)` produces a compile error instead of a confusing runtime error), the mis-typed `unit` bindings got filtered out, breaking legitimate code.

## The solution: `return_type_stack`

Added a `return_type_stack: Vec<Option<StaticType>>` to the `Analyser` struct. It tracks explicit `return` types for each nested function scope:

1. **On function entry**: push `None` onto the stack
2. **On `return expr`**: analyse `expr`, then `fold_lub` its type into the top of the stack
3. **On function exit**: pop the stack, combine with the block's implicit return type:

```rust
let implicit_return = self.analyse(body)?;
let explicit_return = self.return_type_stack.pop().unwrap();

let return_type = match explicit_return {
    Some(ret) => ret.lub(&implicit_return),
    None => implicit_return,
};
```

This correctly handles:
- Functions with only implicit returns (no `return` keyword) → `explicit_return` is `None`, uses block type
- Functions with only explicit returns → combines explicit type with block's unit type via `lub`
- Functions with both early returns and an implicit final value → `lub` of all paths
- Nested functions → stack isolates each function's return types

## Alternatives considered

1. **Change `Statement(Return)` to preserve type**: Could special-case returns in the Statement handler, but this breaks the semantic of semicolons (which always discard) and doesn't handle early returns before the last statement.

2. **Use a `Never`/`NoReturn` type for return statements**: The `return` expression could return a bottom type, and the block would then use `lub(Never, unit) = unit` for the block type, with the actual return type tracked separately. This would be more principled but requires introducing a bottom type throughout the type system.

3. **Track returns at the Block level**: Instead of a stack, each block could propagate return types upward. More complex and doesn't clearly improve on the stack approach.

## Limitations

- `break` and `continue` have a similar issue (they discard the block's implicit type) but are less impactful since they don't produce callable values
- The `lub` combination means `fn foo() { if cond { return 42; } "hello" }` has return type `Any` (lub of Int and String), which is correct but imprecise — a union type would be more accurate
