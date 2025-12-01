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
let fst = empty.first?; // Some(1)
```

> **Note:** unfortunately the language doesn't support pattern matching on options
