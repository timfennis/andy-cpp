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

`a op= b` is checked like `a = a op b`: the operator has to take both operands,
and what it returns has to fit `a`:

```ndc,fails
let values = [1];
values ++= ["two"]; // error: No function called '++' found that matches the arguments 'List<Int>, List<String>'
```

Annotate the target with `Any` to opt into mixed contents:

```ndc
let mixed: List<Any> = [1];
mixed ++= ["two"];
print(mixed);  // [1,"two"]
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
fn g(x) => (x - 40.0) ^ 2;
let lowest, highest = Inf, -Inf;

for x in 1..100 {
  lowest min= g(x);
  highest max= g(x);
}

print(lowest, highest);  // 0.0 3481.0
```
