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

`get?` retrieves a list element by index, returning `None` instead of erroring when
the index is out of bounds. Unlike the `[]` operator, a negative index does not wrap
around — it is simply out of bounds:

```ndc
let my_list = [10, 20, 30];
my_list.get?(0);   // Some(10)
my_list.get?(5);   // None
my_list.get?(-1);  // None
```

`unwrap_or` extracts the contained value, falling back to a default when the option is `None`:

```ndc
[10, 20, 30].get?(1).unwrap_or(0); // 20
[10, 20, 30].get?(9).unwrap_or(0); // 0
```

> **Note:** unfortunately the language doesn't support pattern matching on options
