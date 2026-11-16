# Memoization

Andy C++ supports memoization through the `pure` keyword. Mark a function as `pure` when it has no side effects and returns the same output for the same inputs. Andy C++ can then cache and reuse previous results.

### Syntax

Mark both named and anonymous functions as `pure`:

```ndc
pure fn add(x, y) {
    x + y
}

let multiply = pure fn (x, y) { x * y };

print(add(2, 3));       // 5
print(multiply(4, 5));  // 20
```

> **Note:** The interpreter does not check whether a function is actually pure. You must avoid side effects yourself.

### Which calls share a result

A call gives what an earlier call returned when its arguments are the same
values of the same types:

- Numbers are told apart by kind as well as value: `1` and `1.0` are equal, but
  `f(1)` and `f(1.0)` are different calls.
- Containers are compared by what they hold, and a map by its default too. Heaps
  with the same elements are the same, whatever order they were pushed in.
- A function is its code and the values of the variables it captured, so two
  closures made by one `fn` expression are the same argument only when they
  captured equal values.
- An iterator is only ever the same as itself.

A `pure fn` called with values of different types has a cache for each set of
types, and for each type it returns:

```ndc
pure fn same(x) => x;
print(same(1), same(1.0));  // 1 1.0
```

The cached value is what the later call gets, not a copy of it. A change to a
list it returned is in what the next call with the same arguments returns:

```ndc
pure fn empty() -> List<Int> => [];
empty().push(1);
print(empty());  // [1]
```

### Performance: keep memoization keys small

The cache key is computed by hashing **all arguments**, and for a function argument the values of the variables it captured. For container types like maps and lists this
is an O(n) operation proportional to the number of elements. Passing large containers as arguments
to a `pure fn` therefore adds hashing overhead on every call, even on cache hits.

If a container is large and does not change between recursive calls, such as a lookup table or graph, let the function use it from outside instead of passing it as an argument. A value the function uses from outside is not part of the cache key, so memoization cost depends only on the arguments that change.

```ndc,norun
// Slow: `graph` is hashed on every call
pure fn count(graph, node, visited) { ... }

// Fast: `graph` is used from outside, only (node, visited) are hashed
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

print(fib(92));  // 7540113804746346429
```
