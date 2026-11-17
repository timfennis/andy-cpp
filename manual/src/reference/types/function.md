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
foo(); // ERROR: Identifier foo has not previously been declared

fn foo() {
  print("Hello, World!");
}
```

A function declared with `fn` can't be replaced. If you want a variable that holds a function, declare it with `let`:

```ndc
let scale = fn(x) { x * 2 };
scale = fn(x) { x * 10 };      // fine
print(scale(3));               // 30
```

Functions close over their environment.

```ndc
let x: [Int] = [];

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

Each loop iteration has its own variables, so a function made in a loop keeps the values of the iteration that made it:

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

Parameters and return values can have type annotations, just like `let` bindings. They are optional, so you can annotate as much or as little as you like:

```ndc
fn greet(name: String) -> String => "hello " <> name;

fn add(x: Int, y: Int) -> Int {
    x + y
}

fn count(xs) -> Int => len(xs);

print(greet("world"));  // hello world
print(add(1, 2));       // 3
print(count([1, 2]));   // 2
```

If the body returns a value that doesn't fit the return type, you get an error before the program runs:

```ndc,fails
fn bad() -> Int { "hello" }   // error: mismatched types
```

A function that returns values of different types, like a String in one place and an Int in another, has to say that it returns `Any`:

```ndc
fn size(n: Int) -> Any {
  if n > 9 { return "big"; }
  n
}
print(size(3), size(10));  // 3 big
```

### Function types

The type of a function is written `fn(A, B) -> C`: it takes an `A` and a `B` and returns a `C`. Without `-> …` the function returns `()`:

```ndc
let twice: fn(Int) -> Int = fn(x: Int) => x * 2;
let log: fn(String) = fn(s: String) { print(s); };

fn apply(f: fn(Int) -> Int, x: Int) -> Int { f(x) }
fn make_adder(n: Int) -> fn(Int) -> Int { fn(x: Int) => x + n }

print(apply(twice, 4));           // 8
print(apply(make_adder(10), 1));  // 11
```

You rarely need to annotate the parameters of an anonymous function. They get their types from where the function is used:

```ndc
let shout: fn(String) -> String = fn(s) => s ++ "!";   // s is a String
print(shout("hi"));  // hi!

let tens = [3, 1, 2].map(fn(x) => x * 10);             // x is an Int
print(tens);         // [30,10,20]
```

When nothing says what a parameter is, annotate it, as in `fn(x: Int) => x + 1`.

## Generic functions

A function declared with `fn` takes any argument that its body can work with. You don't have to say so:

```ndc
fn inc(x) => x + 1;
print(inc(1), inc(2.5));               // 2 3.5

fn apply(f, x) => f(x);
print(apply(abs, -1));                 // 1
print(apply(fn(s) => s ++ "!", "hi"));  // hi!
```

Calling it with an argument the body can't use is an error before the program runs, and the error points at the line in the body that can't take it:

```ndc,fails
fn inc(x) => x + 1;
inc("a");  // error: No function called '+' found that matches the arguments 'String, Int'
```

To take a value of any type at all, annotate the parameter with `Any`. Inside the function you can print it or compare it, but to use it as something else you have to [cast](../../features/casts.md) it:

```ndc
fn show(x: Any) => "got " <> x;
print(show(1), show("a"));  // got 1 got a
```

### Type parameters and type arguments

To say that two values have the same type, whatever it is, name the type in angle brackets after the function's name. `T` stands for any type, and the same type everywhere `T` is written:

```ndc
fn first<T>(xs: [T], fallback: T) -> T {
    if xs.len() > 0 { xs[0] } else { fallback }
}
print(first([3, 4], 0));    // 3
print(first([], "none"));   // none
```

A call works out what `T` is from its arguments. To say it yourself, write the type after the name and `::`. These are type arguments, and they go in the order the function declares its type parameters:

```ndc
fn pair<A, B>(a: A, b: B) -> (A, B) { (a, b) }
print(pair::<Int, String>(1, "x"));   // (1,"x")

fn id<T>(x: T) -> T { x }
print(id::<Float>(1));                // 1.0
```

Built-in functions take type arguments too. `ndc docs` shows their type parameters in order, as in `map<T, U>`:

```ndc
print([1, 2].map::<Int, String>(fn(x) => x.string()));   // ["1","2"]
```

The type arguments come before the call's parentheses, so `id::<Int>` without a call is the function itself:

```ndc
fn id<T>(x: T) -> T { x }
let f = id::<String>;
print(f("hi"));   // hi
```

## Recursion

A function can call itself. Calls can go a million deep. A program that goes
deeper stops with an error, as that usually means a function calls itself
without end:

```ndc,fails
fn forever(n) => forever(n + 1);
forever(0)
// error[vm]: calls go more than 1000000 deep, which usually means a function calls itself without end
```

## Function overloading

A name can refer to several functions, called overloads, as long as they differ in their number of parameters or their parameter types:

```ndc
fn add(n) { n + 1 }
fn add(x, y) { x + y }

print(add(9));     // 10
print(add(8, 4));  // 12
```

A call uses the overload whose parameters fit its arguments. When several fit, the most specific one wins:

```ndc
fn describe(x: Int) { "int" }
fn describe(x: String) { "string" }
fn describe(x) { "something else" }

print(describe(1));    // int
print(describe("a"));  // string
print(describe([1]));  // something else
```

Two overloads with the same parameter types are an error. Parameter names don't count:

```ndc,fails
fn foo(a) { a + 1 }
fn foo(b) { b + 2 }   // error: Illegal redefinition of function 'foo' with parameters (Any)
```

### An overloaded function as a value

You can pass a function by its name, as in `xs.map(abs)`. When the name has several overloads, Andy C++ picks the one that fits where it's used. Where nothing says which one you mean, add a function type:

```ndc
print([-1.5, 2.5].map(abs));     // [1.5,2.5]

let by_pattern: fn(String, String) -> [String] = split;
print(by_pattern("a:b", ":"));   // ["a","b"]
```

## Variables with the name of a function

A variable hides the functions with the same name. A method call, like `"test".len()`, only looks at functions, so it still finds them:

```ndc
let len = 300;
print(len);              // 300
print("test".len());     // 4
```

```ndc,fails
let len = 300;
len("test");     // ERROR: The variable 'len' is Int, which can't be called
```

## Method call syntax

{{#include ../../snippets/method-call-syntax.md }}
