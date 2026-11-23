# Map and Set

Andy C++ does not have separate map and set types. A set is a map whose values all use the unit type `()`.

## Set

Because `{` is already used to create a block expression, Andy C++ uses `%{` to create a set or map.

```ndc
let my_set = %{5, 3, 2, 1};
print(3 in my_set);  // true
```

## Map

```ndc
let my_map = %{
    "apples": 60,
    "oranges": 22,
    "bananas": 0,
};
print(my_map["apples"]);  // 60
```

## Default values

```ndc
let defaultdict = %{:0};

// Missing keys in defaultdict default to 0
print(defaultdict[10]);  // 0

// You can also mutate a missing key this way
defaultdict[33] += 7; // adds 7 to 0 and associates it to key 33
print(defaultdict[33]);  // 7
```

## Map and set types

The type of a map is written like the map itself, with the types of its keys and values: `%{String: Int}`. A set leaves out the values: `%{Int}`.

```ndc
let stock: %{String: Int} = %{};
let seen: %{Int} = %{};
stock["apples"] = 60;
seen.insert(3);
print(stock, seen);  // %{"apples": 60} %{3}
```

The keys of a map share one type, and so do its values. To store values of different types, like `%{"a": 1, "b": "two"}`, annotate the map: `let m: %{String: Any} = …` (see [Values of different types](../types.md#values-of-different-types)).

> **Note:** Lists are copied by reference. If you use `[]` as a dictionary default value, every missing key gets the same list instance. Use a default function instead.


## Default functions

You can also specify the default value as a function. Andy C++ evaluates that function each time it needs a new value. Use this form when each missing key needs its own list.

```ndc
let dd = %{:fn() => []};
dd["foo"].push(3);
dd["bar"].push(4);

print(dd["foo"]);  // [3]
print(dd["bar"]);  // [4]
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

**Keys are snapshotted at the start of the loop.** A key added to the map during the loop isn't visited, and a key removed before the loop gets to it is skipped. Values read during iteration do reflect changes to existing keys. Functions that take a function, like `map` and `filter`, go over a map the same way.

## Grouping

`group_by(seq, key)` makes a map from each key the function gives to the elements with that key, in the order they came:

```ndc
let words = ["apple", "bean", "avocado"].group_by(fn(word) => word[0]);
print(words["a"]);  // ["apple","avocado"]
print(words["b"]);  // ["bean"]
```

## Operators

| Operator | Function | Support augmented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `\|` | Union | `true` | `false` |
| `&` | Intersection | `true` | `false` |
| `~` | [Symmetric difference](https://en.wikipedia.org/wiki/Symmetric_difference) | `true` | `false` |
| `in` | Test if `lhs` is part of the Set or a key in the Map | `true` | `true` |
| `==` | Equality | `false` | `true` |
| `!=` | Inequality | `false` | `true` |

> **Note:** The union, intersection and symmetric difference operations retain the default value from the **left operand**

> **Note:** The equality operations ignore the default value
