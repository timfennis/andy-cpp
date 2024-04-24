# Andy C++

This is the source code for the Andy C++ programming language. Not meant for anything serious.

## Example

Currently, the language only has some very basic features. I'll try to update the sample below as I make more progress.

### Factorial

```ndc
n := v := 10;
while { n -= 1 } > 0 {
    v *= n;
}
print v;
```

### Recursive factorial

```ndc
fn factorial(n) {
    if n == 1 {
        return 1
    } 
    
    n * factorial(n - 1)
}

// Easily prints 100! because we use num::BigInt under the hood
print(factorial(100));
// Or call functions as if they are methods on an object
print(100.factorial());

```

### Recursive fibonacci

Does not yet enjoy the benefits of memoization.

```ndc
fn fib(n) {
    if n <= 1 {
        return n
    } 

    fib(n - 1) + fib(n - 2)
}

i := 0;
while i < 30 {
    print(fib(i));
    i += 1;
}
```

### Overloading

The interpreter looks for a function using the types and the number of values.

```ndc
fn add(n) { n + 1 }
fn add(a, b) { a + b }

print(add(add(5), add(4))); // prints 11
print(add(5).add(4.add())); // prints 11 as well
```

### Use functions as augmented assignment operators

Many functions can be used to
create [augmented assignment operators](https://blog.vero.site/post/noulith#augmented-assignment).

```ndc
i := r := 0;

while { i += 1 } < 100 {
    // translates to r = max(r, i * 8333446703 % 94608103)
    r max= i * 8333446703 % 94608103;
}

print(r);
```

## Thanks

This language and implementation was inspired by Robert Nystrom's
book [Crafting Interpreters](https://craftinginterpreters.com/). I've also taken
plagiaristic levels of inspiration from [Noulith](https://github.com/betaveros/noulith) which is the language that
inspired me to read the book in the
first place.
