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
## Function overloading

Functions can be overloaded by declaring multiple `fn` definitions with the same name but different parameter counts.

```ndc
fn add(n) { n + 1 };
fn add(x, y) { x + y };

assert_eq(10, add(9));
assert_eq(12, add(8, 4));
```

Declaring two `fn` definitions with the same name **and** the same number of parameters in the same scope is an error:

```ndc
fn foo(a) { a + 1 }
fn foo(a) { a + 2 } // ERROR: redefinition of 'foo' with 1 parameter
```

> **Note:** The engine also supports overloading functions based on their argument types, and the standard
> library takes advantage of this feature in some cases. However, the current syntax of the language does
> not allow users to specify argument types, so this capability is not yet available for custom functions.

## Function shadowing

A `fn` declaration in a nested scope shadows only the overload with the same parameter count from outer scopes. Other overloads remain accessible:

```ndc
fn foo(a) { "outer-one" }
fn foo(a, b) { "outer-two" }

{
    fn foo(a) { "inner-one" }
    foo("x");        // "inner-one" — inner 1-arg shadows outer 1-arg
    foo("x", "y");   // "outer-two" — outer 2-arg still reachable
}

foo("x");  // "outer-one" — shadow is gone after the block
```

A `let` binding with a function value completely replaces all previous bindings with the same name — it does not participate in function overloading:

```ndc
fn foo(a) { "one" }
fn foo(a, b) { "two" }

let foo = fn(a) => "let";
foo("x");      // "let" — both fn overloads are shadowed
```

A non-function `let` binding shadows the name for value access, but function calls still resolve to the underlying function:

```ndc
let len = 300;
len;           // 300 — the value
len("test");   // 4 — calls the stdlib function, skipping the non-function binding
"test".len;    // 4 — method call also resolves to the function
```

## Anonymous functions

Anonymous functions allow you to define inline functionality and use them as values. This feature is key to unlocking
the full power of functional programming.

```ndc
let my_list = [x for x in 1..10];
let out = my_list.map(fn(x) => x * 10);
```

In this example, the anonymous function `fn(x) => x * 10` is passed directly to the map method, applying the
transformation to each element in my_list.

## Method call syntax

{{#include ../snippets/method-call-syntax.md }}
