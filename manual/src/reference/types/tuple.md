# Tuple

Tuples are similar to [lists](../list.md) but are immutable, meaning their elements cannot be changed after creation.

```ndc
let my_tuple = (1,2,3);

// You can access elements using indexing
assert_eq(my_tuple[0], 1);

// However, tuples are immutable, so modification is not allowed
my_tuple[0] = 99; // ERROR

// You may also iterate over tuples
for item in my_tuple {
  // ..
}
```


## Copy-on-write

Tuples in this language are immutable data structures. Once a tuple is created, its contents cannot be changed. However,
operations that seem to modify a tuple, such as appending elements, trigger a copy-on-write mechanism. This means the
original tuple remains unchanged, and a new tuple is created to reflect the modification.


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
