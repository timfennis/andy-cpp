# Overload dispatch with collections

## Background

Andy C++ picks the overload a call uses before the program runs, from the types of its arguments, so the call costs nothing extra at runtime. When every argument type is known and no overload can accept them, the call could only ever fail, so it is rejected before the program runs with a `No function called '…' found that matches the arguments` error.

## An `Any` takes a cast

An argument of type `Any`, because it was annotated as `Any` or comes from a function that returns `Any`, only goes to an overload that takes an `Any`. Two overloads like these take none:

```ndc,fails
fn process(xs: List<Int>) => "ints";
fn process(xs: List<String>) => "strings";
let value: Any = [1];
process(value)
// error[analysis]: No function called 'process' found that matches the arguments 'Any'
```

A [cast](../features/casts.md) says which one is meant. It checks the value as the program runs, and for a list that scans each element, but the cost lands at a site you wrote:

```ndc
fn process(xs: List<Int>) => "ints";
fn process(xs: List<String>) => "strings";
let value: Any = [1];
print(process(value as List<Int>));  // ints
```

To find out which one it is, test it with `as?`, which gives `None` when the value isn't one:

```ndc
let value: Any = ["a"];
print(value as? List<Int>, value as? List<String>);  // None Some(["a"])
```

## A `List<Any>` fits no typed overload

Standard library overloads also differ by element type. Numeric sequence functions keep the numeric type, so `sum` has three typed overloads:

```
fn sum(Iterable<Int>) -> Int
fn sum(Iterable<Float>) -> Float
fn sum(Iterable<Number>) -> Number
```

A value known only as a `List<Any>` fits none of them, as its elements could be anything, so the call is rejected before it runs:

```ndc,fails
let values: List<Any> = [1, 2, 3];
values.sum()
// error[analysis]: No function called 'sum' found that matches the arguments 'List<Any>'
// = An overload would accept a narrower argument type. Cast to say what the value holds,
//   as in `value as List<Int>`.
```

## Workaround

State the element type with a [cast](../features/casts.md):

```ndc
let values: List<Any> = [1, 2, 3];

// values is typed List<Any>; the cast recovers Int so sum can resolve.
print((values as List<Int>).sum());  // 6
```

Better still, leave out the `Any`: a value keeps the type it was created with unless it's stored as an `Any`, and a parameter without an annotation takes the type of each call's argument.
