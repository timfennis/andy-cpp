# Casts

A cast is how you get a value back out of an `Any`. Anything can go into an
`Any`, but an `Any` can't be used where another type is expected (see
[Getting a value out of an `Any`](../reference/types.md#getting-a-value-out-of-an-any)):

```ndc,fails
let value: Any = 5;
value + 1
// error[analysis]: No function called '+' found that matches the arguments 'Any, Int'
```

The `as` operator says what type the value has. It checks that while the
program runs:

```ndc
let value: Any = 5;
print((value as Int) + 1);  // 6
```

A cast that turns out to be wrong stops the program with an error:

```ndc,fails
let value: Any = "five";
value as Int
// error[vm]: cannot cast String to Int
```

A cast only checks the value, it doesn't convert it, so `n as Float` is an
error when `n` is an `Int`. To turn one type into another, use functions like
`float(n)`, `int(x)` and `string(x)`.

## Casting a list or a map

A cast checks every element of a list, map or other container. It also gives
the container a new type, so it refuses a container that a variable still
holds: the variable would still expect the old type. Cast a copy instead, made
with `clone`:

```ndc
let values: Any = [1, 2];
let ints = values.clone() as List<Int>;
print(ints.sum());  // 3
```

```ndc,fails
let values: Any = [1, 2];
values as List<Int>
// error[vm]: cannot cast to List<Int>: the list is also held by something else, which would still expect what it held before
```

A container that nothing else holds, such as what a function returns, can be
cast without a copy.

A list of `Any`s needs a cast too before you use a function that wants a list
of something more specific, like `sum`:

```ndc
let values: List<Any> = [1, 2, 3];
print((values.clone() as List<Int>).sum());  // 6
```

## Testing a value's type with `as?`

`value as? T` is the cast that doesn't fail: it gives `Some(value)` when the
value is a `T`, and `None` when it isn't.

```ndc
let value: Any = 5;
print(value as? Int);     // Some(5)
print(value as? String);  // None
```

That makes it the way to ask what a value is:

```ndc
fn describe(value: Any) {
    if (value as? Int).is_some() {
        "a number"
    } else if (value as? String).is_some() {
        "some text"
    } else {
        "something else"
    }
}
print(describe(1));    // a number
print(describe("a"));  // some text
print(describe([1]));  // something else
```

## Precedence

`as` and `as?` bind tighter than binary operators, so `a + b as Int` means
`a + (b as Int)`. Method calls bind tighter than the cast, so a method call on
the result of a cast needs parentheses: `(value as? Int).is_some()`.
