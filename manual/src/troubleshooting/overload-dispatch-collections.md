# Overload dispatch with collections

## Background

When Andy C++ can determine at compile time which function overload to call, it does so — the
call is free of any type-checking overhead at runtime. When it cannot (because an argument was
inferred as `Any`), the VM performs **dynamic dispatch**: it tests each candidate overload at
runtime to find the best match.

## O(1) dispatch guarantee

For dynamic dispatch the VM checks whether a value *conforms to* the parameter type without
iterating the container contents. Specifically:

| Parameter type | Check performed |
|---|---|
| `List` | Is the value a list? |
| `Map` | Is the value a map? |
| `Deque` | Is the value a deque? |
| `Sequence` | Is the value any sequence type? |
| `String`, `Int`, … | Exact kind check |

This means that dispatch is **O(1)** regardless of how many elements are in the collection.

## Limitation: element types are not checked at runtime

Because the element-type check is skipped, the VM **cannot distinguish** overloads that differ
only in their container element types via dynamic dispatch. For example, two hypothetical overloads:

```
fn process(List<Int>)
fn process(List<String>)
```

would both fail to match under dynamic dispatch if the list type cannot be resolved at compile
time, because verifying element types would require scanning the entire container.

In practice this limitation is not currently visible:

- User-defined functions cannot yet declare typed container parameters (the syntax is not
  implemented), so user overloads always use `Any` and dispatch works correctly.
- All standard library overloads on container parameters use `<Any>` element types
  (e.g. `List<Any>`, `Sequence<Any>`, `Map<Any, Any>`), so they also hit the fast path.
  Overloads that differ by container *kind* (e.g. `pop(List<Any>)` vs `pop(MinHeap<Any>)`)
  are distinguished by the container kind check alone.

## Workaround

If you notice that a function call unexpectedly fails to match an overload, move the call to a
location where Andy C++ can infer the argument types statically — for example, directly at the
call site rather than through an intermediate untyped function parameter:

```ndc
// The type of `data` is Any here — dynamic dispatch used
fn handle(data) {
    process(data)
}

// Preferred: call process() directly where the type is known
process(my_list)
```
