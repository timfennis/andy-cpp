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

A pure function remembers its results. When you call it again with the same
arguments, it gives back the remembered result instead of running again:

```ndc
let calls = 0;
pure fn square(n) {
    calls += 1;
    n * n
}
print(square(4), square(4), calls);  // 16 16 1
```

Arguments are the same when they're equal with `==`, so two separate lists with
the same items count as the same argument:

```ndc
let calls = 0;
pure fn total(xs: [Int]) {
    calls += 1;
    xs.sum()
}
print(total([1, 2, 3]), total([1, 2, 3]), calls);  // 6 6 1
```

### What a pure function can take

The arguments of a pure function have to be plain data: numbers, strings,
booleans, `None`, and lists, tuples, maps, deques, options and structs of
those. Functions, iterators, heaps and `Any`s aren't allowed:

```ndc,fails
pure fn apply(f: fn(Int) -> Int, x: Int) => f(x);
// error[analysis]: `apply` is a `pure fn`, which only takes data, but its parameter `f` has type `fn(Int) -> Int`
```

### The remembered result isn't a copy

Every call that reuses a result gets the very same value. If that value is a
list and you change it, later calls see the change:

```ndc
pure fn empty() -> [Int] => [];
empty().push(1);
print(empty());  // [1]
```

If you want to change a result, copy it first with `clone`:

```ndc
pure fn empty() -> [Int] => [];
let mine = empty().clone();
mine.push(1);
print(empty());  // []
```

### Performance: keep the arguments small

Every call looks at everything in its arguments, so passing a big list or map
to a pure function costs time on every call. See
[Performance](./performance.md#keep-the-arguments-of-a-pure-function-small)
for how to avoid that.

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
