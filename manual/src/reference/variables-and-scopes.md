# Variables and Scopes

In Andy C++ new variables must be declared explicitly with the `:=` operator just like in Go. The `=` operator is used to reassign a value to an existing variable.

```ndc
x := 1;
print(x); // 1
```

## Shadowing

The language follows shadowing rules similar to Rust, allowing you to declare a variable with an existing name; the new variable will temporarily shadow the previous one.
```ndc
x := 1;
x := 2;
print(x); // 2
```

Similar to Rust, you can create a scope in Andy C++ using curly brackets, allowing variables to be shadowed within that scope. Once the scope ends, the original shadowed variable remains accessible.
```ndc
x := 1;
{
  x := 2;
  print(x); // 2
}
print(x); // 1
```

## Scopes

In Andy C++, everything is an expression—even a scope. Similar to Rust, you can conclude a scope with an expression, which then becomes the value of the scope itself. Notice that the final expression in a block does not end with a semicolon; this signals that its value should be used as the result of the entire expression.

```ndc
x := {
  a := 1;
  b := 2;
  a + b
};

print(x); // 3
```
