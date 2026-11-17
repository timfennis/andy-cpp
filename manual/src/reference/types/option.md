# Option

```ndc
let value = Some(3);

if value.is_some() {
   // Some!!
   print(value.unwrap());  // 3
}

if value.is_none() {
    value.unwrap(); // ERROR!!
}
```

`None` is a literal, like `true` and `false`, so it can't be used as a variable or parameter name.

Some functions have variations with a `?` appended to their name that return options instead of throwing errors:

```ndc,fails
let my_list = [1,2,3];
print(my_list.first?);    // Some(1)

let empty = [];
print(empty.first?());    // None
empty.first();            // ERROR: collection is empty
```

The list accessors `get?` and `index?` give `None` when the index is past the end (see [List](./list.md#element-accessors)):

```ndc
let my_list = [10, 20, 30];
print(my_list.get?(0));     // Some(10)
print(my_list.index?(5));   // None
```

`unwrap_or` extracts the contained value, falling back to a default when the option is `None`:

```ndc
print([10, 20, 30].get?(1).unwrap_or(0));  // 20
print([10, 20, 30].get?(9).unwrap_or(0));  // 0
```

> **Note:** unfortunately the language doesn't support pattern matching on options
