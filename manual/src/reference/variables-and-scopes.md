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

The `=` operator assigns a new value to an existing variable. A variable keeps the type of the value it starts with, so a new value has to have that type too:

```ndc,fails
let n = 1;   // n is an Int
n = 2;       // fine
n = 3.14;    // error: mismatched types: found Float but expected Int
```

So to keep a running total of Floats, start it at `0.0` rather than `0`. The same goes for [augmented assignment](../features/augmented-assignment.md), like `n += 0.5`.

An empty list or map gets its type from the first thing you put in it:

```ndc,fails
let counts = %{};
counts["a"] = 1;      // counts is a %{String: Int} from here on
counts["b"] = "two";  // error: mismatched types: found String but expected Int
```

A variable that only gets its real value later can hold an `Option`:

```ndc
let pos = None;
pos = Some((1, 2));
print(pos);    // Some((1,2))
```

To store values of different types in one variable, annotate it with `Any`:

```ndc
let x: Any = 1;
x = "text";    // fine
print(x);      // text
```

## Type annotations

You can give a variable a type by adding `: Type` after its name. The value has to fit it:

```ndc
let count: Int = 0;
let name: String = "world";
let xs: [Int] = [1, 2, 3];
print(count, name, xs);  // 0 world [1,2,3]
```

```ndc,fails
let x: Int = "hello";   // ERROR: mismatched types: found String but expected Int
```

An annotation is the way to say what an empty list or map will hold:

```ndc
let xs: [Int] = [];
let m: %{String: Int} = %{};
print(xs, m);          // [] %{}
```

The same syntax works on function parameters and return types (see [Function](./types/function.md)). See [Types](./types.md) for the names you can use.

## Destructuring

Destructuring works more like Python than Rust. Commas matter more than the delimiters, so `[]` and `()` both work.

The statements below are all equivalent:

```ndc
let a, b = 3, 4;
let [a, b] = 3, 4;
let (a, b) = [3, 4];
let [a, b] = (3, 4);
print(a, b);  // 3 4
```

You can also destructure nested patterns:

```ndc
let [a, (b, c)] = (1, [2, 3]);
print(a, b, c);  // 1 2 3
```

Ranges and deques destructure too, as long as they have exactly as many elements as there are names:

```ndc
let low, high = 0..2;
print(low, high);  // 0 1
```
