# Tuple

Tuples are similar to [lists](../list.md), but you cannot change their elements after you create them.

```ndc
let my_tuple = (1,2,3);

// You can access elements using indexing
assert_eq(my_tuple[0], 1);

// Tuples are immutable, so assignment fails
my_tuple[0] = 99; // ERROR

// You may also iterate over tuples
for item in my_tuple {
  // ..
}
```

Create a 1-element tuple by adding a trailing comma:

```ndc
assert_eq((1,).len(), 1);
```

## Copy-on-write

Tuples use copy-on-write. If an operation looks like it modifies a tuple, such as appending elements, Andy C++ keeps the original tuple and creates a new one for the updated value.


```ndc
let a = (1,2,3);
let b = a;
b ++= (4,5);

// A remains the same
assert_eq(a, (1,2,3));

// B was copied and (4,5) was appended
assert_eq(b, (1,2,3,4,5));
```

## Operators

{{#include ../../snippets/list-operators.md}}

## Vectorization

Operators broadcast element-wise over tuples. Both arguments must be tuples
of the same length, or one side may be a scalar that broadcasts:

```ndc
assert_eq((1, 2) + (3, 4), (4, 6));
assert_eq(-(1, 2, 3), (-1, -2, -3));
assert_eq((1, 2) + 5, (6, 7));
assert_eq(("a", "b") ++ ("c", "d"), ("ac", "bd"));
```

Vectorization only kicks in for operator syntax (`a + b`, `-x`, `a ++ b`).
Regular function calls never broadcast, so `f((1, 2, 3))` passes the whole
tuple to `f` and does not call `f` once per element.

Mixed-element tuples or length mismatches error at compile time rather than
silently producing wrong results:

```ndc
(1, 2, 3) + (4, 5)   // ERROR: no overload accepts those argument types
(1, "a") + (2, "b")  // ERROR: no `+(String, String)` overload
```
