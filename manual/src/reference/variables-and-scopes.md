# Variables and Scopes

In Andy C++, you declare new variables with `let`, like you do in Rust. Use `=` to reassign an existing variable.

```ndc
let x = 1;
print(x); // 1
```

## Shadowing

Andy C++ follows Rust-style shadowing. If you declare a variable with an existing name, the new binding hides the old one in that scope.
```ndc
let x = 1;
let x = 2;
print(x); // 2
```

You can create a new scope with curly braces. A binding inside that scope can shadow an outer binding. When the scope ends, the outer binding stays available.
```ndc
let x = 1;
{
  let x = 2;
  print(x); // 2
}
print(x); // 1
```

## Scopes

In Andy C++, every block is an expression. End the block with an expression, not a semicolon, and that expression becomes the block's value.

```ndc
let x = {
  let a = 1;
  let b = 2;
  a + b
};

print(x); // 3
```

## Reassignment

The `=` operator can be used to reassign a value to an existing variable. When you reassign a variable to a value of a different type, the variable's type is widened to the least upper bound (LUB) of the old and new types.

```ndc
let x = 1;     // type is Int
x = 2;         // type is still Int
x = 3.14;      // type widens to Number (LUB of Int and Float)
```

```ndc
let pos = ();        // type is ()
pos = (1, 2);        // type widens to Sequence<Any>
pos = ("a", "b");    // type is still Sequence<Any>
```

> **Tip:** For the best type inference, initialize variables with a value that matches the intended type. For example, use `let pos = (0, 0);` instead of `let pos = ();` if you intend to store a 2-tuple of numbers.

## Destructuring

Destructuring works more like Python than Rust. Commas matter more than the delimiters, so `[]` and `()` both work.

The statements below are all equivalent:

```ndc
let a, b = 3, 4;
let [a, b] = 3, 4;
let (a, b) = [3, 4];
let [a, b] = (3, 4);
```

You can also destructure nested patterns:

```
let [a, (b, c)] = (1, [2, 3]);
```
