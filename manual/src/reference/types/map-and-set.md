# Map and Set

Andy C++ does not have separate map and set types. A set is a map whose values all use the unit type `()`.

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

// Missing keys in defaultdict default to 0
assert(defaultdict[10] == 0);

// You can also mutate a missing key this way
defaultdict[33] += 7; // adds 7 to 0 and associates it to key 33
```

> **Note:** Lists are copied by reference. If you use `[]` as a dictionary default value, every missing key gets the same list instance. Use a default function instead.


## Default functions

You can also specify the default value as a function. Andy C++ evaluates that function each time it needs a new value. Use this form when each missing key needs its own list.

```ndc
let dd = %{:fn() => []};
dd["foo"].push(3);
dd["bar"].push(4);

assert_eq(dd["foo"].len, 1);
assert_eq(dd["bar"].len, 1);
```

You can still use a function itself as the default value:

```ndc
let dd = %{:fn() => fn(x) => x * x};
print(dd["test"](5)); // 25
```

## Iteration

Iterate over a map with a `for` loop. Each element is a `(key, value)` tuple:

```ndc
let m = %{"a": 1, "b": 2};
for (k, v) in m {
    print(k, v);
}
```

**Iteration order is unspecified.** Maps are hash-based, so keys may appear in any order.

**Keys are snapshotted at the start of the loop.** Mutations to the map during iteration, such as adding or removing keys, are not reflected in the current loop. The set of keys visited is fixed when the `for` loop begins. Values read during iteration do reflect changes to existing keys.

## Operators

| Operator | Function | Support augmented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `\|` | Union | `true` | `false` |
| `&` | Intersection | `true` | `false` |
| `~` | [Symmetric difference](https://en.wikipedia.org/wiki/Symmetric_difference) | `true` | `false` |
| `in` | Test if `lhs` is part of the Set or a key in the Map | `true` | `true` |
| `==` | Equality | `true` | `true` |
| `!=` | Inequality | `true` | `true` |

> **Note:** The union, intersection and symmetric difference operations retain the default value from the **left operand**

> **Note:** The equality operations ignore the default value
