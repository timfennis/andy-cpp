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

### When a result is reused

A pure function remembers its results. When you call it again with arguments
that are equal to an earlier call's, it gives back the remembered result
instead of running again:

```ndc
let calls = 0;
pure fn square(n) {
    calls += 1;
    n * n
}
print(square(4), square(4), calls);  // 16 16 1
```

"Equal" means what `==` says. Two separate lists with the same items are
equal, so the second call below reuses the first one's result:

```ndc
let calls = 0;
pure fn total(xs: List<Int>) {
    calls += 1;
    xs.sum()
}
print(total([1, 2, 3]), total([1, 2, 3]), calls);  // 6 6 1
```

So a pure function must give the same answer for any two arguments that are
equal. Watch out for values that are equal but not quite the same:

- `0.0 == -0.0` is true, so `inverse(-0.0)` below gets the result of
  `inverse(0.0)`.
- Maps are equal when they have the same entries. Their default value isn't
  compared, so two maps with different defaults count as the same argument.

```ndc
pure fn inverse(x: Float) => 1.0 / x;
print(inverse(0.0), inverse(-0.0));  // Inf Inf
```

A pure function keeps separate results for each type it's used with, so an
Int and a Float never mix:

```ndc
pure fn same(x) => x;
print(same(1), same(1.0));  // 1 1.0
```

### What a pure function can take

The arguments of a pure function must be plain data, which `==` compares by
what's in it:

- numbers, strings, booleans and `None`
- lists, tuples, options, deques and maps of those
- structs whose fields are all of those

These aren't allowed:

- **Functions.** `==` only says two functions are equal when they're the very
  same function, and what a function returns can depend on things outside it.
- **Iterators.** An iterator changes every time you read from it.
- **Heaps** (`MinHeap` and `MaxHeap`). `==` only says two heaps are equal when
  they're the very same heap.
- **`Any`.** It could hold any of the above.

The same goes for what's inside an argument: a list of functions isn't allowed,
and neither is a struct with an `Any` field.

```ndc,norun
pure fn path_length(grid: Map<(Int, Int), Int>, from: (Int, Int)) { ... }  // fine
pure fn apply(f: Fn(Int) -> Int, x: Int) => f(x);                          // a function
pure fn first(numbers: Iterator<Int>) { ... }                              // an iterator
pure fn smallest(h: MinHeap<Int>) => h.min();                              // a heap
pure fn describe(x: Any) { ... }                                           // an Any
```

Using one of them is an error before the program runs:

```ndc,fails
pure fn apply(f: Fn(Int) -> Int, x: Int) => f(x);
// error[analysis]: `apply` is a `pure fn`, which only takes data, but its parameter `f` has type `Fn(Int) -> Int`
```

When a parameter has no type, each call is checked instead. `pure fn same(x)
=> x` can be called with `5` or `[1, 2]`, but not with a function.

### The remembered result isn't a copy

Every call that reuses a result gets the very same value. If that value is a
list and you change it, later calls see the change:

```ndc
pure fn empty() -> List<Int> => [];
empty().push(1);
print(empty());  // [1]
```

If you want to change a result, copy it first with `clone`:

```ndc
pure fn empty() -> List<Int> => [];
let mine = empty().clone();
mine.push(1);
print(empty());  // []
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
