# Functions

You can declare custom functions (with a name) and arguments.

```ndc
fn my_function(input) {
  if input == something {
    return foo;
  }

  // do some more stuff

  // implicitly return the last expression in the block
  bar
}
```

Currently function declarations are not hoisted so you need to define them before you can call them. The example below will fail:

```ndc
foo(); // ERROR: no function called foo exists

fn foo() {
  print("Hello, World!");
}
```

Functions do close over their environment so the example below is perfectly valid:

```ndc
let x = [];

fn my_function(n) {
  // add argument n to x
  x.push(n);

  // return the sum of x
  x.sum()
}

assert_eq(10, my_function(10));
assert_eq(15, my_function(5));
assert_eq([10, 5], x);
```
## Function overload

Functions can be overloaded, allowing multiple definitions with different parameter counts ~or types~.

```ndc
fn add(n) { n + 1 };
fn add(x, y) { x + y };

assert_eq(10, add(9));
assert_eq(12, add(8, 4));
```

> **Note:** The engine also supports overloading functions based on their argument types, and the standard
> library takes advantage of this feature in some cases. However, the current syntax of the language does
> not allow users to specify argument types, so this capability is not yet available for custom functions.

## Anonymous functions

Anonymous functions allow you to define inline functionality and use them as values. This feature is key to unlocking
the full power of functional programming.

```ndc
let my_list = [x for x in 1..10];
let out = my_list.map(fn(x) { x * 10 });
```

In this example, the anonymous function fn(x) { x * 10 } is passed directly to the map method, applying the
transformation to each element in my_list.

## Method call syntax

{{#include ../snippets/method-call-syntax.md }}
