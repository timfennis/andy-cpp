You can slice lists using ranges to extract sublists. Ranges can be inclusive or exclusive, and you can also use negative
indices to count from the end of the list.

```ndc
let my_list = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

// Exclusive range: 3 to 6 (does not include index 6)
assert_eq([30, 40, 50], my_list[3..6]);

// Inclusive range: 3 to 6 (includes index 6)
assert_eq([30, 40, 50, 60], my_list[3..=6]);

// Negative indices: Counting from the end of the list
assert_eq([80, 90], my_list[-3..-1]);
```
