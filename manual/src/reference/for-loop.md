# For loop

For loops is where Andy C++ get's a little spicier. First lets look at some basic examples:

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

You can combine multiple iterations in a single loop:

```ndc
let drinks = ["Coffee", "Tea", "Juice"];
let desserts = ["Cake", "Pie", "Ice Cream"];

// Print all combinations of drinks and desserts
for drink in drinks, dessert in desserts {
  print(drink <> " and " <> dessert);
}
```

Finally you can also add one or more guards. The example below finds all pairs of numbers from 1 until 10 that have an even sum.

```ndc
for x in 1..10, y in 1..10, if (x + y) % 2 == 0 {
  print(x, y, "is even");
}
```

## For comprehensions

The same features, using a similar syntax, can also be used to produce lists on the go using list comprehensions.

```ndc
// To produce a series fo perfect squares
let perfect_squares = [x * x for x in 1..10];

assert_eq([1,4,9,16,25,36,49,64,81,100], perfect_squares);
```

Once again the earlier example can be used to produce actual pairs as follows:

```ndc
let pairs_with_even_sum = [x, y for x in 1..10, y in 1..10, if (x + y) % 2 == 0]
```
