# Function

Functions are first class citizens in the type system and can be stored in variables and passed around as arguments.

```ndc
// An anonymous function that adds its operands together
let my_function = fn(a, b) { a + b };

assert_eq(my_function(5, 3), 8);

fn foo() {
    // whatever
}

let my_variable = foo; // Store the foo-function in a variable
```

For simpler function you can also use the fat arrow `=>` to declare functions that are just a single expression.

```ndc
// Works for anonymous functions
let my_function = fn(a, b) => a + b;

// Also works for named functions
fn foo() => "whatever";

// Note that comma's have a very low presedence in this example x is a tuple (function, 3)
let x = fn(y) => y, 3;

// If you want to return a tuple from a function written in this way you must use parentheses
let x = fn(y) => (y, 3);
```
