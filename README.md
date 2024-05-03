# Andy C++

This is the source code for the Andy C++ programming language. Not meant for anything serious.

## Example

Currently, the language only has some very basic features. I'll try to update the sample below as I make more progress.

### Factorial

You can produce very large numbers quite quickly because we use the [num](https://docs.rs/num/latest/num/) crate under
the hood.

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

print(factorial(100));

// You can call all functions as if they are methods on an object
print(100.factorial());

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
    // roughly translates to r = max(r, i * 8333446703 % 94608103)
    r max= i * 8333446703 % 94608103;
}

print(r);
```

### Maps and Sets

Maps and sets are the same type and have their own special syntax

```ndc
map := %{"foo": "bar"};

set := %{1,2,3,4};
```

Something like the `defaultdict` in python is natively supported using this syntax (stolen from Noulith)

```ndc
default_map := %{:1, 0: 10};
default_map[0] == 10
default_map[1] == 1 // default value

// supports augmented asignment
default_map[5] += 3; // puts 4 in the map

// pitfall: lists are copied by reference
uhm := %{:[]};
uhm[0] ++= [1];

// true because the default value was changed in the line above
uhm[1] == [1] 
```

## Thanks

This language and implementation was inspired by Robert Nystrom's
book [Crafting Interpreters](https://craftinginterpreters.com/). I've also taken
plagiaristic levels of inspiration from [Noulith](https://github.com/betaveros/noulith) which is the language that
inspired me to read the book in the
first place.
