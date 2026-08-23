# Augmented assignment

Andy C++ supports augmented assignment for operations on existing variables.

This example increments a number with augmented assignment.

```
let my_number = 3;
my_number += 5;

assert_eq(my_number, 8);
```

## Indexed targets

The target of an augmented assignment can also be an indexed location:

```ndc
let values = [1, 2, 3];
values[0] += 10;
assert_eq(values, [11, 2, 3]);
```

This works even when reading the location produces a new value rather than a
reference, such as a character of a string or a slice of a list. The updated
value is always stored back into the container:

```ndc
let text = "ab";
text[0] ++= "x";
assert_eq(text, "axb");

let items = [1, 2];
items[0..1] ++= [3];
assert_eq(items, [1, 3, 2]);
```

The target, index, and right-hand side are each evaluated exactly once, in
source order. The augmented assignment expression itself evaluates to `()`.

## Type checking

An augmented assignment with an in-place operator such as `++=` never changes
the type of its target. If the right-hand side would force a type change the
program is rejected:

```ndc
let values = [1];
values ++= ["two"]; // error: mismatched types: found List<String> but expected List<Int>
```

Annotate the target with `Any` to opt into heterogeneous contents:

```ndc
let mixed: List<Any> = [1];
mixed ++= ["two"];
assert_eq(mixed, [1, "two"]);
```

Operators without an in-place implementation behave like `target = target op value`
and may widen the inferred type of the target, just like a regular assignment:

```ndc
let numbers = [1];
numbers[0] += 0.5; // fine: the element type widens from Int to Number
assert_eq(numbers, [1.5]);
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
