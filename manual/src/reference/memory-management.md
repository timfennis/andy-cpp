# Memory Management

Andy C++ uses **reference counting** to manage memory. Every value that lives on the heap (strings, lists, maps, closures, etc.) is wrapped in a reference-counted pointer. When the last reference to a value is dropped, it is freed immediately — there is no garbage collector and no pause times.

## Reference Cycles

Reference counting cannot detect **cycles**: two or more values that refer to each other. When a cycle exists, the reference count of each value in the cycle never reaches zero, so the memory is never freed.

There are two common ways to create cycles:

### Mutable containers that reference each other

```
{
    let m = %{};
    let n = %{};
    m["n"] = n;
    n["m"] = m;
}
// m and n are out of scope, but each map still holds
// a reference to the other — neither can be freed.
```

### Self-referential closures

A closure that captures a variable which holds a reference back to itself creates a cycle:

```
{
    let f = ();
    f = fn(x) { if x > 0 then f(x - 1) else 0 };
}
// The upvalue for f is now closed over a closure
// that itself holds a reference to the same upvalue.
```

## Practical Impact

For short-lived scripts this is a non-issue — all memory is reclaimed when the process exits. Cycles only become a problem in long-running programs that repeatedly create and discard cyclic structures. If you find yourself in that situation, break the cycle manually by assigning `()` to one side before the values go out of scope:

```
m["n"] = ();  // break the cycle
```
