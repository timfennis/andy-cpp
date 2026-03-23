# Function

Functions are first-class values in Andy C++. You can store them in variables, pass them as arguments, and call them like any other value.

## Named functions

Declare named functions with `fn`.

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

Function declarations are not hoisted. Define a function before you call it.

```ndc
foo(); // ERROR: no function called foo exists

fn foo() {
  print("Hello, World!");
}
```

Functions close over their environment.

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

## Anonymous functions

Anonymous functions let you write inline function values.

```ndc
// An anonymous function that adds its operands together
let my_function = fn(a, b) { a + b };

assert_eq(my_function(5, 3), 8);

fn foo() {
    // whatever
}

let my_variable = foo; // Store the foo-function in a variable
```

`map` applies an anonymous function to each element in the list:

```ndc
let my_list = [x for x in 1..10];
let out = my_list.map(fn(x) => x * 10);
```

## Arrow syntax

Use the fat arrow `=>` for functions that return a single expression.

```ndc
// Works for anonymous functions
let my_function = fn(a, b) => a + b;

// Also works for named functions
fn foo() => "whatever";

// Note that commas have a very low precedence in this example x is a tuple (function, 3)
let x = fn(y) => y, 3;

// If you want to return a tuple from a function written in this way you must use parentheses
let x = fn(y) => (y, 3);
```

## Function overloading

You can overload functions by declaring multiple `fn` definitions with the same name and different parameter counts.

```ndc
fn add(n) { n + 1 };
fn add(x, y) { x + y };

assert_eq(10, add(9));
assert_eq(12, add(8, 4));
```

Declaring two `fn` definitions with the same name and the same number of parameters in the same scope is an error:

```ndc
fn foo(a) { a + 1 }
fn foo(a) { a + 2 } // ERROR: redefinition of 'foo' with 1 parameter
```

> **Note:** The engine can also overload functions by argument type, and the standard library uses that support in a few places. You cannot write those overloads in user code yet because the language does not let you declare argument types in function signatures.

## Function shadowing

A `fn` declaration in a nested scope shadows only the overload with the same parameter count from outer scopes. Other overloads remain available:

```ndc
fn foo(a) { "outer-one" }
fn foo(a, b) { "outer-two" }

{
    fn foo(a) { "inner-one" }
    foo("x");        // "inner-one": inner 1-arg shadows outer 1-arg
    foo("x", "y");   // "outer-two": outer 2-arg still reachable
}

foo("x");  // "outer-one": shadow is gone after the block
```

A `let` binding with a function value replaces all previous bindings with the same name. It does not participate in function overloading:

```ndc
fn foo(a) { "one" }
fn foo(a, b) { "two" }

let foo = fn(a) => "let";
foo("x");      // "let": both fn overloads are shadowed
```

A non-function `let` binding shadows the name for value access, but function calls still resolve to the underlying function:

```ndc
let len = 300;
len;           // 300: the value
len("test");   // 4: calls the stdlib function, skipping the non-function binding
"test".len;    // 4: method call also resolves to the function
```

## Method call syntax

{{#include ../../snippets/method-call-syntax.md }}
