# While loop

Like in every other language a `while` loop will run as long as a condition is true.
A `while` loop is an expression that always evaluates to the unit value `()`.

```ndc
let n = 1;

while n <= 100 {
  if n % 15 == 0 {
    print("fizzbuzz");
  } else if n % 3 == 0 {
    print("fizz");
  } else if n % 5 == 0 {
    print("buzz");
  } else {
    print(n);
  }

  n += 1;
}
```

A `while true` loop that no `break` leaves never ends, so it has no value at all, like a `return`. A function can end in one and leave it with `return`, and it returns what its `return`s do:

```ndc
fn first_square_above(n) {
    let i = 1;
    while true {
        if i * i > n {
            return i * i;
        }
        i += 1;
    }
}
print(first_square_above(50) + 1);  // 65
```
