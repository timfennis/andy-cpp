# Overload dispatch with collections

## Background

Andy C++ picks the overload a call uses before the program runs, from the types of its arguments, so the call costs nothing extra at runtime. Only when an argument has type `Any`, because it was annotated as `Any` or comes from a function that returns `Any`, does the VM pick as the program runs, by testing each candidate overload against the values.

The reverse also holds. When every argument type is known and no overload can accept them, the call could only ever fail at runtime, so it is rejected before the program runs with a `No function called '…' found that matches the arguments` error.

## Element types cost a scan

To pick an overload for a value of type `Any`, the VM first looks only at what kind of value it is, which doesn't depend on its size:

| Parameter type | Check performed |
|---|---|
| `List` | Is the value a list? |
| `Map` | Is the value a map? |
| `Deque` | Is the value a deque? |
| `Iterable` | Is the value any sequence type? |
| `String`, `Int`, … | Exact kind check |

A parameter like `List<Int>` doesn't take a list in that first pass. When no overload takes the arguments without looking at their elements, the VM checks the elements before the call fails, scanning each container. So two overloads like these still work for a value of type `Any`:

```
fn process(xs: List<Int>)
fn process(xs: List<String>)
```

Each call through an `Any` scans the list, though, and an overload that takes the value without looking at its elements, like `process(x: Any)`, is picked before either of them.

## A `List<Any>` fits no typed overload

Standard library overloads also differ by element type. Numeric sequence functions keep the numeric type, so `sum` has three typed overloads:

```
fn sum(Iterable<Int>) -> Int
fn sum(Iterable<Float>) -> Float
fn sum(Iterable<Number>) -> Number
```

A value known only as a `List<Any>` fits none of them, as its elements could be anything. Rather than pick as the program runs and scan, the call is rejected before it runs:

```ndc,fails
let values: List<Any> = [1, 2, 3];
values.sum()
// error[analysis]: No function called 'sum' found that matches the arguments 'List<Any>'
// = An overload would accept a narrower argument type. Cast to say what the value holds,
//   as in `value as List<Int>`.
```

## Workaround

State the element type with a [cast](../features/casts.md). A cast scans the value, but the cost lands at a site you wrote rather than inside every call:

```ndc
let values: List<Any> = [1, 2, 3];

// values is typed List<Any>; the cast recovers Int so sum can resolve.
print((values as List<Int>).sum());  // 6
```

Better still, leave out the `Any`: a value keeps the type it was created with unless it passes through something typed `Any`, and a parameter without an annotation takes the type of each call's argument.
