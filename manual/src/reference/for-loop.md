# For loop

Start with a basic `for` loop:

```ndc
for n in 1..=100 {
  if n % 15 == 0 {
    print("fizzbuzz");
  } else if n % 3 == 0 {
    print("fizz");
  } else if n % 5 == 0 {
    print("buzz");
  } else {
    print(n);
  }
}
```

You can loop over several sequences in one loop:

```ndc
let drinks = ["Coffee", "Tea", "Juice"];
let desserts = ["Cake", "Pie", "Ice Cream"];

// Print all combinations of drinks and desserts
for drink in drinks, dessert in desserts {
  print(drink <> " and " <> dessert);
}
```

You can also add one or more guards. This example finds all pairs from `1..10` with an even sum.

```ndc
for x in 1..10, y in 1..10, if (x + y) % 2 == 0 {
  print(x, y, "is even");
}
```

## For comprehensions

You can use the same syntax in a list comprehension.

```ndc
// Produce a series of perfect squares
let perfect_squares = [x * x for x in 1..=10];

print(perfect_squares);  // [1,4,9,16,25,36,49,64,81,100]
```

The earlier example can also produce pairs:

```ndc
let pairs_with_even_sum = [x, y for x in 1..10, y in 1..10, if (x + y) % 2 == 0];
print(pairs_with_even_sum[0..4]);  // [(1,1),(1,3),(1,5),(1,7)]
```

## Changing what you loop over

A loop over a list reads it as it is when it gets to each element, so elements you add during the loop are visited too:

```ndc
let xs = [1, 2, 3];
for x in xs {
    if x < 3 { xs.push(x * 10); }
}
print(xs);  // [1,2,3,10,20]
```

Functions that take a function, like `map` and `filter`, go over a list the same way. A loop that adds an element every time it gets to one never ends.
