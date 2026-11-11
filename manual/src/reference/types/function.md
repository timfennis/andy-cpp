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

A function declared with `fn` can't be replaced. To have a variable that holds a function, declare it with `let`:

```ndc
fn double(x) { x * 2 }
double = fn(x) { x * 10 };     // ERROR: Can't assign to the function 'double'

let scale = fn(x) { x * 2 };
scale = fn(x) { x * 10 };      // fine
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

A function captures the variable itself, not a copy of its value. It sees later assignments, and its own assignments are seen outside:

```ndc
let count = 0;
let increment = fn() { count += 1; };
increment();
increment();
assert_eq(count, 2);
```

Each block and each loop iteration has its own variables, so a function keeps the one from the iteration that created it:

```ndc
let getters = [];
let i = 0;
while i < 3 {
    let j = i;
    getters.push(fn() { j });
    i += 1;
}
assert_eq([g() for g in getters], [0, 1, 2]);
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

## Type annotations

Parameters and return values can carry type annotations, just like `let` bindings:

```ndc
fn greet(name: String) -> String => "hello " <> name;

fn add(x: Int, y: Int) -> Int {
    x + y
}
```

Annotations are optional — leave them off and the parameter is treated as `Any`. Mix and match as you like:

```ndc
fn first(xs: List<Int>) => xs[0];   // params annotated, return inferred
fn count(xs) -> Int => len(xs);      // return annotated, params inferred
```

If the body produces a value that doesn't fit the declared return type, the analyser flags it:

```ndc
fn bad() -> Int { "hello" }   // ERROR: mismatched types
```

A return-type annotation also helps the analyser understand recursive calls — without it, a recursive call resolves against an unknown return type and you can lose precision.

### Function types

The type of a function is written `Fn(A, B) -> C`: it takes an `A` and a `B` and returns a `C`. Without `-> …` the function returns `()`:

```ndc
let twice: Fn(Int) -> Int = fn(x: Int) => x * 2;
let log: Fn(String) = fn(s: String) { print(s); };

fn apply(f: Fn(Int) -> Int, x: Int) -> Int { f(x) }
fn make_adder(n: Int) -> Fn(Int) -> Int { fn(x: Int) => x + n }

assert_eq(apply(twice, 4), 8);
assert_eq(apply(make_adder(10), 1), 11);
```

A function passed for a function parameter has to take what the parameter passes and return what it returns. Parts whose type isn't known, like the parameter of an unannotated lambda, are checked when the program runs:

```ndc
fn apply(f: Fn(Int) -> Int, x: Int) -> Int { f(x) }
apply(fn(x) => x - 1, 1);         // fine: x isn't annotated
apply(fn(s: String) => 1, 4);     // ERROR: the function takes a String, but apply passes an Int
```

## Function overloading

A name can refer to several functions, called overloads, as long as they differ in their number of parameters, their parameter types, or both:

```ndc
fn add(n) { n + 1 }
fn add(x, y) { x + y }

assert_eq(10, add(9));
assert_eq(12, add(8, 4));
```

A call uses the overload whose parameters accept its arguments. When several accept them, the most specific one wins:

```ndc
fn describe(x: Int) { "int" }
fn describe(x: String) { "string" }
fn describe(x: Any) { "something else" }

assert_eq("int", describe(1));
assert_eq("string", describe("a"));
assert_eq("something else", describe([1]));
```

Two overloads in the same scope can't have the same parameter types. Parameter names don't count, and a parameter without a type annotation has type `Any`:

```ndc
fn foo(a) { a + 1 }
fn foo(b) { b + 2 }   // ERROR: Illegal redefinition of function 'foo' with parameters (Any)
```

## How a call finds its function

A call such as `foo(x)` looks for `foo` in the scope it's written in first, then in each enclosing scope in turn, and finally among the built-in functions.

**Functions in a nested scope** take over only the calls they accept. Other calls still reach the overloads further out:

```ndc
fn foo(a) { "outer-one" }
fn foo(a, b) { "outer-two" }

{
    fn foo(a) { "inner-one" }
    foo("x");        // "inner-one"
    foo("x", "y");   // "outer-two": the inner foo doesn't take two arguments
}
```

**A variable**, such as a `let`, a parameter or a loop variable, shadows the functions with its name, whatever it holds. It doesn't join their overloads, so the call uses the variable, and it's an error when the variable can't take the call, even when a function further out would:

```ndc
fn foo(a) { "one" }
fn foo(a, b) { "two" }

let foo = fn(a) => "let";
foo("x");        // "let"
foo("x", "y");   // ERROR: The variable 'foo' holds a function that doesn't accept the arguments 'String, String'

let len = 300;
len("test");     // ERROR: The variable 'len' is Int, which can't be called
```

**A method call `x.foo()` and a property read `x.foo`** are the call `foo(x)`, but they only look among functions: variables named `foo` are skipped. A variable never gets in the way of a method call, and a function stored in a variable is called the plain way:

```ndc
let len = 300;
len;               // 300
"test".len();      // 4: the built-in len
"test".len;        // 4

let double = fn(x) => x * 2;
double(5);         // 10
5.double();        // ERROR: No function named 'double'
```

**Within one scope**, a name belongs either to `let` bindings or to functions, not both. A `fn` after a `let` with the same name in the same scope is an error, while in a nested scope it's fine. The same applies to a struct's constructor:

```ndc
let foo = fn(a) => "let";
fn foo(a, b) { "fn" }    // ERROR: Illegal redefinition of variable foo as a function in the same scope

let bar = 1;
{
    fn bar(a) { a }      // fine: a different scope
}
```

## Method call syntax

{{#include ../../snippets/method-call-syntax.md }}
