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

A list has two pairs of element accessors. `get` / `get?` take a non-negative
index (a `usize`); `index` / `index?` take a signed integer and wrap negative
indices from the end, like the `[]` operator. The `?` variant of each returns
`None` when the index is past the end instead of erroring:

```ndc
let my_list = [10, 20, 30];

print(my_list.get(0));      // 10
print(my_list.get?(0));     // Some(10)
print(my_list.get?(5));     // None

print(my_list.index(-1));   // 30
print(my_list.index?(-1));  // Some(30)
print(my_list.index?(5));   // None
```

Each of these is an error:

```ndc,fails
let my_list = [10, 20, 30];
my_list.get(5);    // ERROR: index 5 is out of bounds
```

```ndc,fails
let my_list = [10, 20, 30];
my_list.get?(-1);  // ERROR: arg 1: expected a non-negative integer, but the value was negative
```

```ndc,fails
let my_list = [10, 20, 30];
my_list.index(5);  // ERROR: index out of bounds
```

`unwrap_or` extracts the contained value, falling back to a default when the option is `None`:

```ndc
print([10, 20, 30].get?(1).unwrap_or(0));  // 20
print([10, 20, 30].get?(9).unwrap_or(0));  // 0
```

> **Note:** unfortunately the language doesn't support pattern matching on options
