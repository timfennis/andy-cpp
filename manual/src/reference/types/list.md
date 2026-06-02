# List

Lists are mutable in Andy C++.

```ndc
let my_list = [1,2,3];

// Values inside a list can be changed
my_list[2] = 4;

// You can add elements to the end of a list
my_list.push(99);

// Remove and return the last element of the list
let element = my_list.pop();
```

## Indexing

Lists, strings, and tuples also support negative indexes. Negative indexes count from the end.

```
let my_list = [1,2,3,4,5,6,7,8,9];
assert_eq(9, my_list[-1]);
```

### Element accessors

Besides the `[]` operator, lists provide four element accessor functions. They
differ on two axes: whether negative indexes are allowed, and what happens when
the index is out of bounds.

| function | index | out of bounds |
|----------|-------|---------------|
| `get`    | non-negative only (negative is an error) | error |
| `get?`   | non-negative only (negative is an error) | `None` |
| `index`  | signed; negatives wrap from the end      | error |
| `index?` | signed; negatives wrap from the end      | `None` |

```ndc
let my_list = [10, 20, 30];

assert_eq(my_list.get(0), 10);
assert_eq(my_list.get?(9), None);   // in range for a usize, past the end
assert_eq(my_list.index(-1), 30);   // wraps like `[]`
assert_eq(my_list.index?(-9), None);
```

`index` behaves like the `[]` operator (wrap + error on out of bounds);
`index?` is its non-throwing counterpart.

## Slicing

{{#include ../../snippets/slices.md}}

## Operators

{{#include ../../snippets/list-operators.md}}
