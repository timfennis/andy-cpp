# Casts

The `as` operator asserts that a value has the given type:

```ndc
let values: List<Any> = [1, 2];

// values is a List<Any>; the cast recovers the element type so the
// result works with functions that expect a List<Int>.
let ints = values as List<Int>;
print(ints.sum());  // 3
```

A cast is the way a value comes out of an `Any`. Anything goes into an
`Any`, but an `Any` doesn't go where another type is expected, and no
operator or function takes it unless it takes an `Any` (see [Getting a value
out of an `Any`](../reference/types.md#getting-a-value-out-of-an-any)):

```ndc,fails
let value: Any = 5;
value + 1
// error[analysis]: No function called '+' found that matches the arguments 'Any, Int'
```

```ndc
let value: Any = 5;
print((value as Int) + 1);  // 6
```

A cast checks the value without converting it. `n as Float` is an error
when `n` is an `Int`; use conversion functions like `float(n)` to change a
value's type. A number literal takes the type the cast says, so `5 as Float`
is the Float `5.0`.

## Checking

When the compiler can already prove the cast from the value's static type,
the cast is free: no runtime check is emitted. Otherwise the value is
checked at runtime, and a failed check raises an error at the cast site:

```ndc,fails
let value: Any = ["a"];
value as List<Int>
// error[vm]: cannot cast List<String> to List<Int>
```

Checking a container inspects every element (and recurses into nested
containers), so a cast costs one pass over the value. Empty containers
conform to any element type: `[] as List<Int>` succeeds.

A map's default value is checked against the value type too, since a
missing-key lookup inserts it. A default *function*'s results can't be
verified without calling it, so such a map only conforms when the value
type is `Any`.

A function is checked against what its own type says: one that takes a
`String` can't be a `Fn(Int) -> Int`. When what it returns isn't known until
it runs, each call's result is checked:

```ndc,fails
fn text(x) => "s";             // its value takes and returns anything
let value: Any = text;
let f = value as Fn(Int) -> Int;
f(1)
// error[vm]: expected the function to return Int but it returned String
```

The analyser rejects casts between types that cannot share a value:

```ndc,fails
"foo" as Int
// error[analysis]: invalid cast: String can never be Int
```

## Testing a value's type with `as?`

`value as? T` is the cast that doesn't fail: it is `Some(value)`, as a `T`,
when the value is a `T`, and `None` when it isn't. Its type is `Option<T>`.

```ndc
let value: Any = 5;
print(value as? Int);     // Some(5)
print(value as? String);  // None
```

It checks a value the way `as` does, so a container is checked element by element:

```ndc
let values: List<Any> = [1, "a"];
let mixed: Any = values;
print(mixed as? List<Int>);  // None
print(mixed as? List<Any>);  // Some([1,"a"])
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

`as` and `as?` bind tighter than binary operators and looser than unary operators
and calls, so `a + b as Int` means `a + (b as Int)`. Method calls bind
tighter than the cast: recovering an element type before a method call
needs parentheses, as in `(values.keys as List<Int>).max()`. An `as?` cast
needs them too before a method call on the option: `(value as? Int).is_some()`.

A `<` after the cast type is read as a type argument list when the tokens
that follow form one, and as a less-than comparison otherwise, so
`n as Int < 10` compares while `xs as List<Int>` casts.

## Limitations

Checking an iterator's elements would consume it, so a runtime check against
an element type other than `Any` fails. Cast a value into a typed collection
before turning it into an iterator.
