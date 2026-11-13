# Overload dispatch with collections

## Background

When Andy C++ can determine at compile time which function overload to call, it does so — the
call is free of any type-checking overhead at runtime. When it cannot (because an argument was
inferred as `Any`), the VM performs **dynamic dispatch**: it tests each candidate overload at
runtime to find the best match.

The reverse also holds. Sometimes every argument type is known at compile time, but no
overload can accept them: each candidate is fully annotated, and none matches the call's
arity and argument types. Such a call could only ever fail at runtime. Instead of waiting
for that, it is rejected at compile time with a
`No function called '…' found that matches the arguments` error.

## O(1) dispatch guarantee

For dynamic dispatch the VM checks whether a value *conforms to* the parameter type without
iterating the container contents. Specifically:

| Parameter type | Check performed |
|---|---|
| `List` | Is the value a list? |
| `Map` | Is the value a map? |
| `Deque` | Is the value a deque? |
| `Iterable` | Is the value any sequence type? |
| `String`, `Int`, … | Exact kind check |

This means that dispatch is **O(1)** regardless of how many elements are in the collection.

## Element types are checked only when nothing else fits

Because the element-type check is skipped, a parameter like `List<Int>` doesn't take a list in that first pass. When no overload takes the arguments without looking at their elements, the VM checks the elements before the call fails, scanning each container. So two overloads like these still work under dynamic dispatch:

```
fn process(List<Int>)
fn process(List<String>)
```

Each call through an `Any` scans the list, though, and an overload that takes the value without looking at its elements, like `process(Any)`, is picked before either of them.

Standard library overloads do differ by element type. Numeric sequence functions preserve the
concrete numeric type, so `sum` has three typed overloads:

```
fn sum(Iterable<Int>) -> Int
fn sum(Iterable<Float>) -> Float
fn sum(Iterable<Number>) -> Number
```

A value the analyser only knows as `List<Any>` matches none of them. Rather than dispatch on
it and scan, the call is rejected at compile time:

```ndc,fails
let values: List<Any> = [1, 2, 3];
values.sum()
// error[analysis]: No function called 'sum' found that matches the arguments 'List<Any>'
// = An overload would accept a narrower argument type. Cast to say what the value holds,
//   as in `value as List<Int>`.
```

Overloads that differ only by container *kind* (e.g. `pop(List<T>)` vs `pop(MinHeap<T>)`)
are still distinguished by the kind check alone. User-defined functions cannot yet declare
typed container parameters (the syntax is not implemented), so user overloads always use
`Any`.

## Workaround

State the element type with a [cast](../features/casts.md). A cast scans the value, but the
cost lands at a site you wrote rather than inside every dispatch:

```ndc
let values: List<Any> = [1, 2, 3];

// values is typed List<Any>; the cast recovers Int so sum can resolve.
print((values as List<Int>).sum());  // 6
```

Otherwise, move the call to a location where Andy C++ can infer the argument types
statically — for example, directly at the call site rather than through an intermediate
untyped function parameter:

```ndc,norun
// The type of `data` is Any here — dynamic dispatch used
fn handle(data) {
    process(data)
}

// Preferred: call process() directly where the type is known
process(my_list)
```
