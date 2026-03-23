In Andy C++, you can call a function as a *method* on its first argument. You get method-style syntax without
defining member functions.

```ndc
fn add(x, y) {
  return x + y;
}

// Normal function call
print(add(3, 5));

// Method-like syntax
print(3.add(5));
```

Both forms do the same thing.

### Implicit call

You can also omit `()` when you call a 0-ary function.

```ndc
let input = "some text\nsome more text";
let lines = input.lines;
```
