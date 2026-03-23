# Augmented assignment

Andy C++ supports augmented assignment for operations on existing variables.

This example increments a number with augmented assignment.

```
let my_number = 3;
my_number += 5;

assert_eq(my_number, 8);
```

## Optimization

You might expect `list ++= [1,2,3]` to desugar to `list = list ++ [1,2,3]`, but that would waste work. Andy C++ handles some augmented assignments directly. In this case, it appends `[1,2,3]` without creating an intermediate list.

## Flexibility

Note: I stole this feature from [Noulith](https://github.com/betaveros/noulith).

Augmented assignment also works with built-in functions and user-defined functions. For example:

```ndc
let x = 3;
let f = fn (a, b) { a + b }; // simple addition
x f= 5; // similar to: x = f(x, 5);
assert_eq(x, 8);
```

One common use case is tracking the highest or lowest value in a loop:

```ndc
let lowest, highest = Inf, -Inf;

for x in 1..100 {
  lowest min= g(x);
  highest max= g(x);
}
```
