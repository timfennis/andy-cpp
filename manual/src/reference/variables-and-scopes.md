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

The `=` operator assigns a new value to an existing variable. A variable keeps the type its first value gave it, so a new value has to fit that type:

```ndc,fails
let n = "abc".len();  // n is Int
n = 2;                // fine
n = 3.14;             // error: mismatched types: found Float but expected Int
```

A number literal takes the type its uses need, so a variable that starts at `1` and is later given `3.14` is a `Float` from the start:

```ndc
let x = 1;
x = 3.14;
print(x);  // 3.14
```

Augmented assignment is checked the same way, against the result of the operation:

```ndc,fails
let total = "abc".len();
total += 0.5;  // error: Int + Float is a Float, which doesn't fit in Int
```

What an empty list or map holds, or what `None` will hold, isn't known yet. The first assignment that stores something settles it, and later ones have to fit:

```ndc,fails
let counts = %{};     // key and value types not known yet
counts["a"] = 1;      // counts is Map<String, Int> from here on
counts["b"] = "two";  // error: mismatched types: found String but expected Int
```

Calls settle them too, through the types the called function takes. `push` takes a `List<T>` and a `T`, so pushing an `Int` makes the list a `List<Int>`, and later pushes have to fit:

```ndc,fails
let xs = [];
xs.push(1);       // xs is List<Int> from here on
xs.push("two");   // error: No function called 'push' found that matches the arguments 'List<Int>, String'
```

When nothing settles them, they stay `Any`, which takes a [cast](../features/casts.md) to be used as anything else.

A variable that only gets its real value later holds an `Option`:

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

You can pin a variable's type by adding `: Type` after the name. The initialiser still has to fit, the analyser just checks it for you up front.

```ndc
let count: Int = 0;
let name: String = "world";
let xs: List<Int> = [1, 2, 3];
print(count, name, xs);  // 0 world [1,2,3]
```

Every value fits where `Any` is requested:

```ndc
let n: Number = 3n;
let x: Any = "anything";  // every value fits Any
print(n, x);              // 3 anything
```

A mismatch is rejected with a `mismatched types` error:

```ndc,fails
let x: Int = "hello";   // ERROR: mismatched types: found String but expected Int
```

An empty container literal adopts the annotated type — there are no elements to infer
from, so the annotation decides:

```ndc
let xs: List<Int> = [];        // xs is List<Int>, not List<Any>
let m: Map<Int, Int> = %{};    // m is Map<Int, Int>
let nested: List<List<Int>> = [[]];
print(xs, m, nested);          // [] %{} [[]]
```

An annotated variable keeps its annotated type, just as an inferred one keeps the type of its first value:

```ndc,fails
let x: Int = 5;
x = "test";   // ERROR: mismatched types
```

```ndc,fails
let x: Int = 5;
x += 0.5;     // ERROR: the Float result doesn't fit in Int
```

The same syntax shows up on function parameters and return types — see the [Function](./types/function.md) page.

See [Types](./types.md) for the full list of names you can use, including generics like `List<T>`, `Map<K, V>`, and tuple shorthand `(Int, String)`.

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
