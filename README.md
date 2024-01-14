# Andy C++

This is the source code for the Andy C++ programming language. Not meant for anything serious.

## Example

Currently, the language only has some very basic features. I'll try to update the sample below as I make more progress.

### Factorial

```ndc
n := v := 10;
while { n = n - 1 } > 0 {
    v = v * n;
}
print v;
```

### Recursive factorial

```ndc
fn factorial(n) {
    if n == 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// Easily prints 100! because we use num::BigInt 
// under the hood
print factorial(100);

```

### Recursive fibonacci

Does not yet enjoy the benefits of memoization.

```ndc
fn fib(n) {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

i := 0;
while { i = i + 1 } < 30 {
    print fib(i);
}
```

## Thanks

This language and implementation was inspired by Robert Nystrom's
book [Crafting Interpreters](https://craftinginterpreters.com/). I've also taken
plagiaristic levels of inspiration from [Noulith](https://github.com/betaveros/noulith) which is the language that
inspired me to read the book in the first place.
