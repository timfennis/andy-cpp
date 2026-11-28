# List

Lists are mutable in Andy C++.

```ndc
let my_list = [1,2,3];

// Values inside a list can be changed
my_list[2] = 4;
print(my_list);           // [1,2,4]

// You can add elements to the end of a list
my_list.push(99);
print(my_list);           // [1,2,4,99]

// Remove and return the last element of the list
let element = my_list.pop();
print(element, my_list);  // 99 [1,2,4]
```

The values of a list share one type. To put values of different types in one list, like `[1, "a"]`, annotate it: `let xs: [Any] = [1, "a"];` (see [Values of different types](../types.md#values-of-different-types)).

## Indexing

Lists, strings, and tuples also support negative indexes. Negative indexes count from the end.

```ndc
let my_list = [1,2,3,4,5,6,7,8,9];
print(my_list[-1]);  // 9
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

print(my_list.get(0));      // 10
print(my_list.get?(9));     // None
print(my_list.index(-1));   // 30
print(my_list.index?(-9));  // None
```

`index` works like the `[]` operator, and `index?` gives `None` instead of an
error.

### First and last

`first` and `last` give the first and last element of a list (or any sequence), and with a function, the first and last element the function gives `true` for. Their `?` forms give `None` where they would fail:

```ndc
let numbers = [1, 4, 5, 8, 9];
print(numbers.first, numbers.last);                // 1 9
print(numbers.first(fn(x) => x % 2 == 0));         // 4
print(numbers.last(fn(x) => x % 2 == 0));          // 8
print(numbers.first?(fn(x) => x > 10), [].last?);  // None None
```

## Runs

`chunk_by` splits a list (or any sequence) into runs of equal neighbours, and with a function, into runs of neighbours the function gives `true` for:

```ndc
print([1, 1, 2, 1, 1].chunk_by());                         // [[1,1],[2],[1,1]]
print([1, 2, 3, 5, 6].chunk_by(fn(a, b) => b == a + 1));  // [[1,2,3],[5,6]]
```

To group equal elements wherever they are, rather than only neighbours, use `group_by` on [maps](./map-and-set.md#grouping).

## Slicing

{{#include ../../snippets/slices.md}}

## Operators

{{#include ../../snippets/list-operators.md}}
