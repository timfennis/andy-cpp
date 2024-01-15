# TODO

This is an attempt of keeping sort of a TODO list for stuff I still need to add or remember to fix in the future.

### Trace all clones
There are a couple of places where I wrote `.clone()` on a `Value` where I probably shouldn't have. These should be annotated with `TODO: `'s

### List expressions
The following expressions should all be valid Andy C++

```ndc
a := []; // a is an empty list
a := [1,2,3]; // a is a list of 1, 2 and 3
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

