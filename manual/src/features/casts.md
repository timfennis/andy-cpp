# Casts

The `as` operator asserts that a value has the given type:

```ndc
let values = %{};
values.insert(1);
values.insert(2);

// keys returns List<Any>; the cast recovers the element type so the
// result works with functions that expect a List<Int>.
let keys = values.keys as List<Int>;
```

A cast never converts the value — it only checks it. `5 as Float` is an
error because `5` is an `Int`; use conversion functions like `float(5)` to
change a value's type.

## Checking

When the compiler can already prove the cast from the value's static type,
the cast is free: no runtime check is emitted. Otherwise the value is
checked at runtime, and a failed check raises an error at the cast site:

```ndc
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

Casts between types where neither is a subtype of the other are rejected at
compile time:

```ndc
"foo" as Int
// error[resolver]: invalid cast: String can never be Int
```

## Precedence

`as` binds tighter than binary operators and looser than unary operators
and calls, so `a + b as Int` means `a + (b as Int)`. Method calls bind
tighter than the cast: recovering an element type before a method call
needs parentheses, as in `(values.keys as List<Int>).max()`.

## Limitations

An iterator cannot be inspected without consuming it, so a runtime check
against an element type other than `Any` fails. Cast a value into a typed
collection before turning it into an iterator.
