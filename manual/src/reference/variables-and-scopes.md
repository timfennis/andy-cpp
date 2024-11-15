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

## Destructuring

Destructuring is more similar to how it works in python and cares mostly about where comma's are and not so much about the delimiters (`[]`, `()`) used.

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
