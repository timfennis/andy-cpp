# Memoization

Andy C++ natively supports memoization through the `pure` keyword. Declaring a function as `pure` indicates that
it has no side effects and always produces the same output for the same inputs. This allows the language to
automatically cache and reuse results, improving performance for repeated calls with the same arguments.

### Syntax

You can mark both named and anonymous functions as `pure`:

```ndc
pure fn add(x, y) {
    x + y
}

let multiply = pure fn (x, y) { x * y };
```

> **Note:** the interpreter does not perform any checks to see if the function is actually pure. It's your
> responsibility to ensure that functions don't have side-effects.

### Performance: keep memoization keys small

The cache key is computed by hashing **all arguments**. For container types like maps and lists this
is an O(n) operation proportional to the number of elements. Passing large containers as arguments
to a `pure fn` therefore adds hashing overhead on every call — even on cache hits.

If a container is large and doesn't change between recursive calls (e.g. a lookup table or graph),
capture it as an **upvalue** instead of passing it as an argument. The upvalue is not part of the
cache key, so the memoization cost is proportional only to the arguments that actually vary.

```ndc
// Slow: `graph` is hashed on every call
pure fn count(graph, node, visited) { ... }

// Fast: `graph` captured as upvalue, only (node, visited) are hashed
let graph = build_graph();
pure fn count(node, visited) {
    // use graph here
}
```

### Example: Fibornacci Sequence

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
