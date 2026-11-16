# Control flow

Control flow in Andy C++ uses expressions and keywords instead of punctuation-heavy syntax. This section covers branching, loops, and the lazy logical operators that often appear in conditions.

## Statements that end at a brace

A statement that starts with `if`, `while`, `for` or a block `{ }` ends at its closing brace, as in Rust: what follows starts the next statement. A list or a tuple on the next line isn't an index into the loop's value or a call of it:

```ndc
fn path_to(last) {
    let path = [1];
    for x in 2..=last { path.push(x); }
    [path]
}
print(path_to(3));  // [[1,2,3]]
```

Inside an expression they still take an index, a call or an operator, as in `print(if c { [1] } else { [2] }[0])`. At the start of a statement, parentheses do the same: `(if c { xs } else { ys })[0] = 1;`.
