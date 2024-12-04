# Map and Set

Andy C++ does not have separate map and set types, a set is just a map where all the values are the unit type `()`.

## Set

Because `{` is already used to create a block expression, Andy C++ uses `%{` to create a set or map.

```ndc
let my_set = %{5, 3, 2, 1};
```

## Map

```ndc
let my_map = %{
    "apples": 60,
    "oranges": 22,
    "bananas": 0,
};
```

## Default values

```ndc
let defaultdict = %{:0};

// All nonexisting elements in defaultdict default to 0
assert(defaultdict[10] == 0);

// You can also mutate elements that don't exist in this way
defaultdict[33] += 7; // adds 7 to 0 and associates it to key 33
```

> **Note:** Because lists are copied by reference setting the default value of a dictionary to `[]` will insert
> the same instance of the list into the dictionary every time. Use the function syntax instead!


## Default functions

You may also specify the default value as a function, in this case the function is evaluated every time a new value is produced.
This is usefull when you want the default value of the dictionary to be a new list instead of a pointer to the same list.

```ndc
let dd = %{:fn() => []};
dd["foo"].push(3);
dd["bar"].push(4);

assert_eq(dd["foo"].len, 1);
assert_eq(dd["bar"].len, 1);
```

This does make it a bit more complicated to set the default value of your dictionary to a function, but luckily that's still
possible.

```ndc
let dd = %{:fn() => fn(x) => x * x};
print(dd["test"](5)); // 25
```

## Operators

| Operator | Function | Support agumented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `\|` | Union | `true` | `false` |
| `&` | Intersection | `true` | `false` |
| `~` | [Symmetric difference](https://en.wikipedia.org/wiki/Symmetric_difference) | `true` | `false` |
| `in` | Test if `lhs` is part of the Set or a key in the Map | `true` | `true` |
| `==` | Equality | `true` | `true` |
| `!=` | Inequality | `true` | `true` |

> **Note:** The union, intersection and symmetric difference operations retain the default value from the **left operand**

> **Note:** The equality operations ignore the default value
