# Tuple

Tuples are similar to [lists](./list.md), but you cannot change their elements after you create them.

```ndc
let my_tuple = (1,2,3);

// You can access elements using indexing
print(my_tuple[0]);  // 1

// You may also iterate over tuples
for item in my_tuple {
  print(item);
}
```

```ndc,fails
let my_tuple = (1,2,3);

// Tuples are immutable, so assignment fails
my_tuple[0] = 99; // ERROR
```

The elements of a tuple can have different types. An index written as a number, like `t[1]` or `t[-1]`, gives that element with its own type. An index in a variable, like `t[i]`, could pick any of them, so it gives an `Any` that you have to [cast](../../features/casts.md): `t[i] as Int`.

```ndc
let point = ("origin", 0, 0);
let name: String = point[0];
let y: Int = point[-1];
print(name, y);  // origin 0
```

Create a 1-element tuple by adding a trailing comma:

```ndc
print((1,).len());  // 1
```

## Operators

Like lists, tuples support `in`, `not in`, `==`, `!=`, `<>` and the comparisons `<`, `<=`, `>` and `>=`. Other
operators work on each element, as described below.

## Vectorization

Operators broadcast element-wise over tuples. Both arguments must be tuples
of the same length, or one side may be a scalar that broadcasts:

```ndc
print((1, 2) + (3, 4));           // (4,6)
print(-(1, 2, 3));                // (-1,-2,-3)
print((1, 2) + 5);                // (6,7)
print(("a", "b") ++ ("c", "d"));  // ("ac","bd")
```

This only works for operators, not for function calls: `f((1, 2, 3))` passes
the whole tuple to `f`. Comparisons like `<` and `<=>` don't work element by
element either: they compare two tuples as a whole and give one result:

```ndc
print((1, 2) < (1, 3), (2, 0) <=> (1, 9));  // true 1
```

Tuples of different lengths are an error:

```ndc,fails
(1, 2, 3) + (4, 5)  // ERROR: no overload accepts those argument types
```
