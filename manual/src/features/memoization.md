# Memoization

Andy C++ supports memoization through the `pure` keyword. Mark a function as `pure` when it has no side effects and returns the same output for the same inputs. Andy C++ can then cache and reuse previous results.

### Syntax

Mark both named and anonymous functions as `pure`:

```ndc
pure fn add(x, y) {
    x + y
}

let multiply = pure fn (x, y) { x * y };
```

> **Note:** The interpreter does not check whether a function is actually pure. You must avoid side effects yourself.

### Performance: keep memoization keys small

The cache key is computed by hashing **all arguments**. For container types like maps and lists this
is an O(n) operation proportional to the number of elements. Passing large containers as arguments
to a `pure fn` therefore adds hashing overhead on every call, even on cache hits.

If a container is large and does not change between recursive calls, such as a lookup table or graph, capture it as an **upvalue** instead of passing it as an argument. The upvalue is not part of the cache key, so memoization cost depends only on the arguments that change.

```ndc
// Slow: `graph` is hashed on every call
pure fn count(graph, node, visited) { ... }

// Fast: `graph` captured as upvalue, only (node, visited) are hashed
let graph = build_graph();
pure fn count(node, visited) {
    // use graph here
}
```

### Example: Fibonacci Sequence

```ndc
pure fn fib (n) {
  return if n == 0 {
    0
  } else if n <= 2 {
    1
  } else {
    fib(n-2) + fib(n-1)
  }
}

for x in 1..1000 {
  print(x, fib(x));
}
```
