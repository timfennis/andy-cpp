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

You can combine multiple iterators in one loop:

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
let perfect_squares = [x * x for x in 1..10];

assert_eq([1,4,9,16,25,36,49,64,81,100], perfect_squares);
```

The earlier example can also produce pairs:

```ndc
let pairs_with_even_sum = [x, y for x in 1..10, y in 1..10, if (x + y) % 2 == 0]
```
