# Casts

A cast is how you get a value back out of an `Any`. Anything can go into an
`Any`, but an `Any` can't be used where another type is expected (see
[Getting a value out of an `Any`](../reference/types.md#getting-a-value-out-of-an-any)):

```ndc,fails
let value: Any = 5;
value + 1
// error[analysis]: No function called '+' found that matches the arguments 'Any, Int'
```

`cast::<T>(value)` says that the value is a `T`. It checks that while the
program runs:

```ndc
let value: Any = 5;
print(cast::<Int>(value) + 1);  // 6
```

`cast` is a function: the type it casts to goes between `::<` and `>`, and
the value in the parentheses. Like other functions, it can be called as a
method, `value.cast::<Int>()`.

A cast that turns out to be wrong stops the program with an error:

```ndc,fails
let value: Any = "five";
cast::<Int>(value)
// error[vm]: cannot cast String to Int
```

A cast only checks the value, it doesn't convert it, so `cast::<Float>(n)` is
an error when `n` is an `Int`. To turn one type into another, use functions like
`float(n)`, `int(x)` and `string(x)`.

## Casting a list or a map

A cast checks every element of a list, map or other container:

```ndc
let values: Any = [1, 2];
let ints = cast::<[Int]>(values);
print(ints.sum());  // 3
```

A list of `Any`s needs a cast too before you use a function that wants a list
of something more specific, like `sum`:

```ndc
let values: [Any] = [1, 2, 3];
print(cast::<[Int]>(values).sum());  // 6
```

A cast of a list gives you a list of your own. When a variable still holds
the list, the cast copies it, so changing one doesn't change the other:

```ndc
let values: [Any] = [1, 2];
let ints = cast::<[Int]>(values);
values.push("three");
print(ints);    // [1,2]
print(values);  // [1,2,"three"]
```

## Testing a value's type with `cast?`

`cast?::<T>(value)` is the cast that doesn't fail: it gives `Some(value)` when
the value is a `T`, and `None` when it isn't.

```ndc
let value: Any = 5;
print(cast?::<Int>(value));     // Some(5)
print(cast?::<String>(value));  // None
```

That makes it the way to ask what a value is:

```ndc
fn describe(value: Any) {
    if cast?::<Int>(value).is_some() {
        "a number"
    } else if cast?::<String>(value).is_some() {
        "some text"
    } else {
        "something else"
    }
}
print(describe(1));    // a number
print(describe("a"));  // some text
print(describe([1]));  // something else
```

## Casting each element with `map`

`cast::<T>` without the parentheses is a function that casts what it's given,
so it can be passed to `map`:

```ndc
let values: [Any] = [1, 2, 3];
print(values.map(cast::<Int>).sum());  // 6
```
