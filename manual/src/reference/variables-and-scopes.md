# Variables and Scopes

In Andy C++ new variables must be declared explicitly using `let`-statements just like in rust. The `=` operator is used to reassign a value to an existing variable.

```ndc
let x = 1;
print(x); // 1
```

## Shadowing

The language follows shadowing rules similar to Rust, allowing you to declare a variable with an existing name; the new variable will temporarily shadow the previous one.
```ndc
let x = 1;
let x = 2;
print(x); // 2
```

Similar to Rust, you can create a scope in Andy C++ using curly brackets, allowing variables to be shadowed within that scope. Once the scope ends, the original shadowed variable remains accessible.
```ndc
let x = 1;
{
  let x = 2;
  print(x); // 2
}
print(x); // 1
```

## Scopes

In Andy C++, everything is an expression—even a scope. Similar to Rust, you can conclude a scope with an expression, which then becomes the value of the scope itself. Notice that the final expression in a block does not end with a semicolon; this signals that its value should be used as the result of the entire expression.

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

Destructuring is more similar to how it works in python and cares mostly about where commas are and not so much about the delimiters (`[]`, `()`) used.

The statements below are all equivalent:

```ndc
let a, b = 3, 4;
let [a, b] = 3, 4;
let (a, b) = [3, 4];
let [a, b] = (3, 4);
```

It's also possible to destructure deeper patterns:

```
let [a, (b, c)] = (1, [2, 3]);
```
