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
