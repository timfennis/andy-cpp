# Option

```ndc
let value = Some(3);

if value.is_some() {
   // Some!!
   let value = value.unwrap();
}

if value.is_none() {
    value.unwrap(); // ERROR!!
}
```

Some functions have variations with a `?` appended to their name that return options instead of throwing errors:

```ndc
let empty = [];
let fst = empty.first(); // ERROR: list is empty
let fst = empty.first?(); // None

let my_list = [1,2,3];
let fst = my_list.first?; // Some(1)
```

A list has two pairs of element accessors. `get` / `get?` take a non-negative
index (a `usize`); `index` / `index?` take a signed integer and wrap negative
indices from the end, like the `[]` operator. The `?` variant of each returns
`None` when the index is past the end instead of erroring:

```ndc
let my_list = [10, 20, 30];

my_list.get(0);     // 10
my_list.get(5);     // ERROR: index 5 is out of bounds
my_list.get?(0);    // Some(10)
my_list.get?(5);    // None
my_list.get?(-1);   // ERROR: a negative index is not a valid usize

my_list.index(-1);  // 30   (wraps from the end)
my_list.index(5);   // ERROR: index out of bounds
my_list.index?(-1); // Some(30)
my_list.index?(5);  // None
```

`unwrap_or` extracts the contained value, falling back to a default when the option is `None`:

```ndc
[10, 20, 30].get?(1).unwrap_or(0); // 20
[10, 20, 30].get?(9).unwrap_or(0); // 0
```

> **Note:** unfortunately the language doesn't support pattern matching on options
