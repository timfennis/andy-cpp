# Range

A range is the `Int`s from one number to another. `a..b` stops before `b`, and `a..=b` includes it:

```ndc
print([x for x in 1..4]);   // [1,2,3]
print([x for x in 1..=4]);  // [1,2,3,4]
```

A range without an end, `a..`, goes on counting until you stop the loop:

```ndc
for n in (10..) {
    if n * n > 200 {
        print(n);  // 15
        break;
    }
}
```

## A range is a value

Looping over a range doesn't use it up, so you can keep one in a variable and use it as often as you like:

```ndc
let digits = 0..10;
print([d for d in digits].sum());  // 45
print([d for d in digits].sum());  // 45
```

Asking whether a number is in a range, or how many numbers it holds, doesn't loop over it:

```ndc
let valid = 1..=12;
print(7 in valid, 13 in valid);  // true false
print(len(valid));               // 12
```

Two ranges are equal when they hold the same numbers, and a range can be a map key:

```ndc
print((1..=3) == (1..4));  // true

let names = %{1..=3: "low", 4..=6: "high"};
print(names[1..4]);  // low
```

Ranges also select a part of a list or string; see [slices](../../snippets/slices.md).
