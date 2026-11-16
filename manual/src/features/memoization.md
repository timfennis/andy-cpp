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

### A `pure fn` takes data

A `pure fn` gives a call the result of an earlier one with the same arguments,
so its arguments must be values it can compare. Its parameters can't be, or
hold, a function, whose result can depend on what it uses from outside; an
iterator, which changes as it's read; or an `Any`, which could hold either.
That includes the fields of a struct:

```ndc,fails
pure fn call(f: Fn(Int) -> Int, n: Int) => f(n);
// error[analysis]: `call` is a `pure fn`, which only takes data, but its parameter `f` has type `Fn(Int) -> Int`
```

A `pure fn` without annotations is checked for the types each call gives it.

### Which calls share a result

A call gets an earlier call's result when its arguments are the same values:
lists, tuples and deques with the same elements in the same order, and maps
with the same entries and the same default. Two heaps are the same when they
keep the same elements in the same order, which heaps built by the same pushes
do. Numbers that are equal but different, as `0.0` and `-0.0` are, are
different arguments:

```ndc
pure fn inverse(x: Float) => 1.0 / x;
print(inverse(0.0), inverse(-0.0));  // Inf -Inf
```

A map's default can be a function, as in `%{: fn() => []}`, which the map's
type doesn't show. It's told apart by its code: like the `pure fn` itself,
what it uses from outside isn't part of the arguments.

A `pure fn` called with values of different types has a cache for each set of
types, and for each type it returns. The cached value is what the later call
gets, not a copy of it, so a change to a list it returned is in what the next
call with the same arguments returns:

```ndc
pure fn empty() -> List<Int> => [];
empty().push(1);
print(empty());  // [1]
```

### Performance: keep memoization keys small

The cache key is computed by hashing **all arguments**. For container types like maps and lists this
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
