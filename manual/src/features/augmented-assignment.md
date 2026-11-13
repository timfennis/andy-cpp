# Augmented assignment

Andy C++ supports augmented assignment for operations on existing variables.

This example increments a number with augmented assignment.

```ndc
let my_number = 3;
my_number += 5;
print(my_number);  // 8
```

## Indexed targets

The target of an augmented assignment can also be an indexed location:

```ndc
let values = [1, 2, 3];
values[0] += 10;
print(values);  // [11,2,3]
```

This works even when reading the location produces a new value rather than a
reference, such as a character of a string or a slice of a list. The updated
value is always stored back into the container:

```ndc
let text = "ab";
text[0] ++= "x";
print(text);   // axb

let items = [1, 2];
items[0..1] ++= [3];
print(items);  // [1,3,2]
```

The target, index, and right-hand side are each evaluated exactly once, in
source order. The augmented assignment expression itself evaluates to `()`.

## Type checking

An augmented assignment with an in-place operator such as `++=` never changes
the type of its target. If the right-hand side would force a type change the
program is rejected:

```ndc,fails
let values = [1];
values ++= ["two"]; // error: mismatched types: found List<String> but expected List<Int>
```

The right-hand side has to *provably* fit. A wider element type is not enough,
because the operator copies the values across without checking them:

```ndc,fails
let values: List<Int> = [1];
let rhs: List<Number> = [0.5n];
values ++= rhs; // error: mismatched types: found List<Number> but expected List<Int>
```

The same goes for an operand whose type is unknown. `Any` says nothing about
what the value holds, and no later step re-checks it, so it is rejected too:

```ndc,fails
fn opaque(x: Any) => x;
let values = [1];
values ++= opaque([2, 3]); // error: mismatched types: found Any but expected List<Int>
```

[Cast](casts.md) the operand to say what it holds. The cast checks the value, so
the assignment gets its guarantee from the cast site:

```ndc
fn opaque(x: Any) => x;
let values = [1];
values ++= opaque([2, 3]) as List<Int>;
print(values);  // [1,2,3]
```

A cast that does not hold fails there rather than corrupting the target:

```ndc,fails
fn opaque(x: Any) => x;
let values = [1];
values ++= opaque([0.5]) as List<Int>; // error: cannot cast List<Float> to List<Int>
```

Annotate the target with `Any` to opt into heterogeneous contents:

```ndc
let mixed: List<Any> = [1];
mixed ++= ["two"];
print(mixed);  // [1,"two"]
```

Operators without an in-place implementation behave like `target = target op value`,
so the result has to fit the target's type, just like in a regular assignment:

```ndc,fails
let numbers = ["a".len()];   // a List<Int>
numbers[0] += 0.5;           // error: Int + Float is a Float, which doesn't fit in List<Int>
```

A list of number literals takes the type its uses need, so there the numbers are Floats:

```ndc
let numbers = [1, 2];
numbers[0] += 0.5;
print(numbers);  // [1.5,2.0]
```

## Optimization

You might expect `list ++= [1,2,3]` to desugar to `list = list ++ [1,2,3]`, but that would waste work. Andy C++ handles some augmented assignments directly. In this case, it appends `[1,2,3]` without creating an intermediate list.

## Flexibility

Note: I stole this feature from [Noulith](https://github.com/betaveros/noulith).

Augmented assignment also works with built-in functions and user-defined functions. For example:

```ndc
let x = 3;
let f = fn (a, b) { a + b }; // simple addition
x f= 5;   // similar to: x = f(x, 5);
print(x); // 8
```

One common use case is tracking the highest or lowest value in a loop:

```ndc
fn g(x) => (x - 40) ^ 2;
let lowest, highest = Inf, -Inf;

for x in 1..100 {
  lowest min= g(x);
  highest max= g(x);
}

print(lowest, highest);  // 0 3481
```
