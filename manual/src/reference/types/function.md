# Function

Functions are first-class values in Andy C++. You can store them in variables, pass them as arguments, and call them like any other value.

## Named functions

Declare named functions with `fn`.

```ndc,norun
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

```ndc,fails
foo(); // ERROR: no function called foo exists

fn foo() {
  print("Hello, World!");
}
```

A function declared with `fn` can't be replaced:

```ndc,fails
fn double(x) { x * 2 }
double = fn(x) { x * 10 };     // ERROR: Can't assign to the function 'double'
```

To have a variable that holds a function, declare it with `let`:

```ndc
let scale = fn(x) { x * 2 };
scale = fn(x) { x * 10 };      // fine
print(scale(3));               // 30
```

Functions close over their environment.

```ndc
let x: List<Int> = [];

fn my_function(n) {
  // add argument n to x
  x.push(n);

  // return the sum of x
  x.sum()
}

print(my_function(10));  // 10
print(my_function(5));   // 15
print(x);                // [10,5]
```

A function captures the variable itself, not a copy of its value. It sees later assignments, and its own assignments are seen outside:

```ndc
let count = 0;
let increment = fn() { count += 1; };
increment();
increment();
print(count);  // 2
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
print([g() for g in getters]);  // [0,1,2]
```

## Anonymous functions

Anonymous functions let you write inline function values.

```ndc
// An anonymous function that adds its operands together
let my_function = fn(a, b) { a + b };

print(my_function(5, 3));  // 8

fn foo() {
    "whatever"
}

let my_variable = foo; // Store the foo-function in a variable
print(my_variable());      // whatever
```

`map` applies an anonymous function to each element in the list:

```ndc
let my_list = [x for x in 1..10];
let out = my_list.map(fn(x) => x * 10);
print(out);  // [10,20,30,40,50,60,70,80,90]
```

## Arrow syntax

Use the fat arrow `=>` for functions that return a single expression.

```ndc
// Works for anonymous functions
let my_function = fn(a, b) => a + b;
print(my_function(1, 2));  // 3

// Also works for named functions
fn foo() => "whatever";
print(foo());              // whatever

// Note that commas have a very low precedence in this example x is a tuple (function, 3)
let x = fn(y) => y, 3;
print(x[1]);               // 3

// If you want to return a tuple from a function written in this way you must use parentheses
let x = fn(y) => (y, 3);
print(x(1));               // (1,3)
```

## Type annotations

Parameters and return values can carry type annotations, just like `let` bindings:

```ndc
fn greet(name: String) -> String => "hello " <> name;

fn add(x: Int, y: Int) -> Int {
    x + y
}

print(greet("world"));  // hello world
print(add(1, 2));       // 3
```

Annotations are optional. A parameter without one gets its type from how the function is used: each call of a function declared in the program gives it the types of that call's arguments (see [Generic functions](#generic-functions)), and a lambda gets the types of how it's used (see below). Mix and match as you like:

```ndc
fn first(xs: List<Int>) => xs[0];   // params annotated, return inferred
fn count(xs) -> Int => len(xs);      // return annotated, params inferred

print(first([7, 8]));      // 7
print(count([1, 2, 3]));   // 3
```

If the body produces a value that doesn't fit the declared return type, the analyser flags it:

```ndc,fails
fn bad() -> Int { "hello" }   // error: mismatched types
```

Without an annotation, a function returns the type its `return`s and its body's value share. When they share none, say that it returns `Any` (see [Values of different types](../types.md#values-of-different-types)):

```ndc,fails
fn size(n: Int) {
  if n > 9 { return "big"; }
  n  // error: values of different types: String and Int share no type
}
```

```ndc
fn size(n: Int) -> Any {
  if n > 9 { return "big"; }
  n
}
print(size(3), size(10));  // 3 big
```

### Function types

The type of a function is written `Fn(A, B) -> C`: it takes an `A` and a `B` and returns a `C`. Without `-> …` the function returns `()`:

```ndc
let twice: Fn(Int) -> Int = fn(x: Int) => x * 2;
let log: Fn(String) = fn(s: String) { print(s); };

fn apply(f: Fn(Int) -> Int, x: Int) -> Int { f(x) }
fn make_adder(n: Int) -> Fn(Int) -> Int { fn(x: Int) => x + n }

print(apply(twice, 4));           // 8
print(apply(make_adder(10), 1));  // 11
```

A function passed for a function parameter has to take what the parameter passes and return what it returns:

```ndc,fails
fn apply(f: Fn(Int) -> Int, x: Int) -> Int { f(x) }
apply(fn(s: String) => 1, 4);     // error: the function takes a String, but apply passes an Int
```

A lambda's unannotated parameters get their types from how the lambda is used: the function type it's stored as, what the function it's passed to gives it, and the calls in its body. Its body is checked with those types:

```ndc
let shout: Fn(String) -> String = fn(s) => s ++ "!";   // s is a String
print(shout("hi"));  // hi!

let tens = [3, 1, 2].map(fn(x) => x * 10);             // x is an Int, so tens is a List<Int>
print(tens);         // [30,10,20]
```

```ndc,fails
let double: Fn(Int) -> Int = fn(x) => x ++ "!";        // error: `x ++ "!"` makes x a String, so the lambda is a Fn(String) -> String
```

A lambda passed to a function gets its parameter types from that function and its other arguments before its body is checked, so a mistake in the body is reported where it is:

```ndc,fails
struct Player { name: String, score: Int }
struct Team { name: String, size: Int }
let players = [Player("ann", 3), Player("bob", 5)];
players.map(fn(p) => p.size);    // error: p is a Player, which has no size
```

A lambda stored in a variable has one type, like any variable, so unlike a function declared with `fn` it can't take an `Int` in one call and a `Float` in the next. A lambda parameter that nothing gives a type is `Any`, which its body then has to work with: in `let f = fn(x) => x + 1;` with nothing calling `f`, no `+` takes `x`, so annotate it as `fn(x: Int)`.

## Generic functions

A function declared with `fn` in the program takes any argument its body can work with. Each call gets the types its body makes of that call's arguments:

```ndc
fn inc(x) => x + 1;

let a: Int = inc(1);       // + on an Int and an Int is an Int
let b: Float = inc(2.5);   // + on a Float and an Int is a Float

fn apply(f, x) => f(x);
print(apply(abs, -1));                 // 1
print(apply(fn(s) => s ++ "!", "hi"));  // hi!

fn fib(n) => if n < 2 { n } else { fib(n - 1) + fib(n - 2) };
let i: Int = fib(10);
print(a, b, i);                        // 2 3.5 55
```

The body is compiled for each set of types it's called with, so the calls in it are picked before the program runs.

A call with an argument the body can't use is an error at the call, with notes that lead to the line in the body that can't take it:

```ndc,fails
fn inc(x) => x + 1;
fn shout(s) => inc(s) ++ "!";
shout("a");    // error: No function called '+' found that matches the arguments 'String, Int'
               //   `shout` calls `inc` here, and `inc` needs `x + 1`
```

To take any value whatever its type, annotate the parameter with `Any`: the body then works with an `Any`, which takes a [cast](../../features/casts.md) to be used as anything else. A call with an `Any` argument has an unannotated parameter be an `Any` too, so its body has to work with one:

```ndc,fails
fn inc(x) => x + 1;
let a: Any = 1;
inc(a);  // error: No function called '+' found that matches the arguments 'Any, Int'
```

## Function overloading

A name can refer to several functions, called overloads, as long as they differ in their number of parameters, their parameter types, or both:

```ndc
fn add(n) { n + 1 }
fn add(x, y) { x + y }

print(add(9));     // 10
print(add(8, 4));  // 12
```

A call uses the overload whose parameters accept its arguments. When several accept them, the most specific one wins:

```ndc
fn describe(x: Int) { "int" }
fn describe(x: String) { "string" }
fn describe(x: Any) { "something else" }

print(describe(1));    // int
print(describe("a"));  // string
print(describe([1]));  // something else
```

An argument of type `Any` only goes to an overload that takes an `Any`, whatever it holds as the program runs, and a cast picks another:

```ndc
fn describe(x: Int) { "int" }
fn describe(x: Any) { "something else" }

let a: Any = 1;
print(describe(a), describe(a as Int));  // something else int
```

Two overloads in the same scope can't have the same parameter types. Parameter names don't count:

```ndc,fails
fn foo(a) { a + 1 }
fn foo(b) { b + 2 }   // error: Illegal redefinition of function 'foo' with parameters (Any)
```

A parameter without a type annotation takes any argument the body can work with, so an overload with one is a fallback, like one taking `Any`, but with a typed body. A call uses the most specific overload that fits it:

```ndc
fn show(x: Int) { "int" }
fn show(x) { "other" }

print(show(1));    // int
print(show("a"));  // other
```

## An overloaded function as a value

A function's name can be used as a value, as in `xs.map(abs)`. When the name has several overloads, the type expected where it's used picks one. `map` passes each element, so over a list of Floats `abs` is `abs(Float)`, and over strings `split` is `split(String)` rather than `split(String, String)`. A function type annotation picks one too, and so do a variable's uses:

```ndc
print([-1.5, 2.5].map(abs));     // [1.5,2.5]
print(["a b"].map(split));       // [["a","b"]]

let by_pattern: Fn(String, String) -> List<String> = split;
print(by_pattern("a:b", ":"));   // ["a","b"]

let f = split;
print(f("a b"));                 // ["a","b"]
```

Here `f` is `split(String)`, as it's called with one String. When nothing says which overload is meant, as in `let g = split;`, it's an error that asks for a function type.

In a generic function, each call's types pick the overload, as they pick the calls in its body:

```ndc
fn signs(xs) => xs.map(signum);
print(signs([3, -2]), signs([0.5, -1.5]));   // [1,-1] [1.0,-1.0]
```

## How a call finds its function

A call such as `foo(x)` looks for `foo` in the scope it's written in first, then in each enclosing scope in turn, and finally among the built-in functions.

**Functions in a nested scope** take over only the calls they accept. Other calls still reach the overloads further out:

```ndc
fn foo(a) { "outer-one" }
fn foo(a, b) { "outer-two" }

{
    fn foo(a) { "inner-one" }
    print(foo("x"));        // inner-one
    // the inner foo doesn't take two arguments
    print(foo("x", "y"));   // outer-two
}
```

**A variable**, such as a `let`, a parameter or a loop variable, shadows the functions with its name, whatever it holds. It doesn't join their overloads, so the call uses the variable, and it's an error when the variable can't take the call, even when a function further out would:

```ndc
fn foo(a) { "one" }
fn foo(a, b) { "two" }

let foo = fn(a) => "let";
print(foo("x"));  // let
```

```ndc,fails
fn foo(a, b) { "two" }

let foo = fn(a) => "let";
foo("x", "y");   // ERROR: The variable 'foo' holds a function that doesn't accept the arguments 'String, String'
```

```ndc,fails
let len = 300;
len("test");     // ERROR: The variable 'len' is Int, which can't be called
```

**A method call `x.foo()` and a property read `x.foo`** are the call `foo(x)`, but they only look among functions: variables named `foo` are skipped. A variable never gets in the way of a method call, and a function stored in a variable is called the plain way:

```ndc
let len = 300;
print(len);              // 300
// the built-in len
print("test".len());     // 4
print("test".len);       // 4

let double = fn(x) => x * 2;
print(double(5));        // 10
```

```ndc,fails
let double = fn(x) => x * 2;
5.double();        // ERROR: No function named 'double'
```

**Within one scope**, a name belongs either to `let` bindings or to functions, not both. A `fn` after a `let` with the same name in the same scope is an error, while in a nested scope it's fine. The same applies to a struct's constructor:

```ndc,fails
let foo = fn(a) => "let";
fn foo(a, b) { "fn" }    // ERROR: Illegal redefinition of variable foo as a function in the same scope
```

```ndc
let bar = 1;
{
    fn bar(a) { a }      // fine: a different scope
    print(bar(2));       // 2
}
print(bar);              // 1
```

## Method call syntax

{{#include ../../snippets/method-call-syntax.md }}
