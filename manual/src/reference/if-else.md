# If/else

If-statements are very similar to how they are in Go and Rust. You don't need parentheses
around the conditions but braces around the body are required.

```ndc
let x = 3;
let y = 4;

if x == 3 {
  print("x is three");
} else if y == 4 {
  print("y is four");
} else {
  print("neither x is three or y is four");
}
```

## Expressions

Just like in Rust they are expressions and can be used to return a value when you omit the semicolon at the end of the last statement in the block.

```ndc
let n = 5;
let x = if n > 0 {
  1
} else if n == 0 {
  0
} else {
  -1
};
print(x);  // 1

```
