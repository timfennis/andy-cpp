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
