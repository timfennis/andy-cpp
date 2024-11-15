# List

Lists are mutable constructs in Andy C++.

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

Aside from regular (forward) indexing lists, strings and tuples also support negative indexes to retrieve elements based
on their position relative to the end of the list.

```
let my_list = [1,2,3,4,5,6,7,8,9];
assert_eq(9, my_list[-1]);
```

## Slicing

{{#include ../../snippets/slices.md}}
