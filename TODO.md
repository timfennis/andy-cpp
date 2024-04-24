# TODO

This is an attempt of keeping sort of a TODO list for stuff I still need to add or remember to fix in the future.

## List expressions (and destructuring apparently)

The following expressions should all be valid Andy C++

```ndc
a := []; // a is an empty list (DONE? NEEDS TEST)
a := [1,2,3]; // a is a list of 1, 2 and 3 (DONE)
a := [1..10]; // a is a (lazy) list of the numbers 1 up to 10

// QUESTION about ranges, does the following expression mean anything?
a := 1..10;

// Destructuring with lists
a, b, c = [1, 2, 3]; // Destructure the list into the variables a, b and c
[a, b, c] = [1, 2, 3]; // Identical to the expression before it?

// QUESTION: should we allow the splat operator?
[a, *b] = [1, 2, 3]; // a == 1, b == [2, 3]

// QUESTION: should we support the cons operator?
// THOUGHT: this probably conflicts with allowing type specifications like `a: int = 3`
(a:b) = [1, 2, 3]; // a == 1, b == [2, 3]
(a:b:c) = [1, 2, 3]; // a == 1, b == 2, c == [3]
(a:b:c:d) = [1, 2, 3]; // a == 1, b == 2, c == 3, d == []
```

## List comprehensions

```ndc
x := [ sqrt(n) | n in ns ]
cartesian_product := [ x, y | x in xs, y in ys ]
```

### General

* [x] turn `print` into a function
* [ ] change `print` to `dbg` and use a library to get rust's `println!` behavior
* [ ] sequence functions like `map`, `reduce`, and `foldl`

### Math

* [ ] abs
* [ ] gcd
* [x] lcm
* [ ] integer division `//`

### Type conversion

`float(), int(), string()` to convert between types (possibly others)

### Strings

* [ ] ord (convert a single character string to an int)

### Refactorings:

* Treat operators as functions instead of special expression types

## Trace all clones

There are a couple of places where I wrote `.clone()` on a `Value` where I probably shouldn't have. These should be
annotated with `TODO: `'s

## Improved error handling

The error handling right now is just good enough to be usable. We don't correctly use the `start, end` location fields
at the moment, and we don't use them to give nice error messages.

## Structs (or classes?)

```ndc
struct Foo { x, y, z };
```

## Type annotations

```ndc
fn math_baby(f: Float) { ... }
```

## Match expressions

Some real cool advanced pattern matching? Probably low prio since this is by far the most work.