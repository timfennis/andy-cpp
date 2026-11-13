# Unit

In Andy C++ the unit type is written as `()` and indicates that there is no value. Statements evaluate
to the unit type for instance when a semicolon is used after the last value.

```ndc
let x = 1;
let y = 2;

let result = if x == y {
  5 + 5;
} else {
  10 + 10;
};

// Result is unit because the last expression in the blocks has a semicolon
print(result == ());  // true
```

Unit is also a 0-length tuple:

```ndc
print((1,2,3)[0..0] == ());  // true
```

The language does not have `null` or `nil`.
