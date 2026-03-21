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

## The solution: `Never` bottom type + `return_type_stack`

Two complementary changes:

### 1. `StaticType::Never` — bottom type for diverging expressions

`return`, `break`, and `continue` now produce `StaticType::Never` instead of the value's type or unit. `Never` is a subtype of every type, so `lub(T, Never) = T`. The `Statement` handler propagates `Never` instead of converting it to unit:

```rust
Expression::Statement(inner) => {
    let typ = self.analyse(inner)?;
    if typ == StaticType::Never {
        Ok(StaticType::Never)  // diverging — don't convert to unit
    } else {
        Ok(StaticType::unit())
    }
}
```

This means `return 1;` (a `Statement(Return)`) has type `Never`, not unit. When the block computes its implicit return type, `lub(previous_type, Never) = previous_type` — the dead code after a return doesn't pollute the type.

Without this, `return 1;` would contribute both `Int` (via the return stack) and `unit` (via the block's implicit type), and `lub(Int, unit) = Any` — making every function with explicit returns infer `Any`.

### 2. `return_type_stack` — tracking explicit return types

Added a `return_type_stack: Vec<Option<StaticType>>` to the `Analyser` struct. It tracks explicit `return` types for each nested function scope:

1. **On function entry**: push `None` onto the stack
2. **On `return expr`**: analyse `expr`, `fold_lub` its type into the top of the stack, return `Never`
3. **On function exit**: pop the stack, combine with the block's implicit return type:

```rust
let implicit_return = self.analyse(body)?;
let explicit_return = self.return_type_stack.pop().unwrap();

let return_type = match explicit_return {
    Some(ret) => ret.lub(&implicit_return),
    None => implicit_return,
};
```

### How they work together

For `fn foo() { return 1; }`:
- `return 1` → pushes `Int` to stack, returns `Never`
- `Statement(Return)` → propagates `Never`
- Block implicit type: `Never`
- Stack: `Some(Int)`
- Combined: `lub(Int, Never) = Int` ✓

For `fn foo(x) { if x > 0 { return x; } "default" }`:
- `return x` → pushes `Int` to stack, returns `Never`
- `Statement(Return)` → `Never`
- If-true branch: `Never`, if-false (implicit): unit
- `lub(Never, unit) = unit` → if expression type is unit
- Statement wrapping if → unit
- `"default"` → `String`
- Block implicit type: `String`
- Stack: `Some(Int)`
- Combined: `lub(Int, String) = Any` ✓

For `fn foo() { 1 }`:
- No explicit return
- Block implicit type: `Int`
- Stack: `None`
- Combined: `Int` ✓
