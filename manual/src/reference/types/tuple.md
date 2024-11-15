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
