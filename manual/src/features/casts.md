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

A cast checks the value without converting it. `5 as Float` is an error
because `5` is an `Int`; use conversion functions like `float(5)` to change
a value's type.

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

The analyser rejects casts between types that cannot share a value:

```ndc
"foo" as Int
// error[resolver]: invalid cast: String can never be Int
```

## Precedence

`as` binds tighter than binary operators and looser than unary operators
and calls, so `a + b as Int` means `a + (b as Int)`. Method calls bind
tighter than the cast: recovering an element type before a method call
needs parentheses, as in `(values.keys as List<Int>).max()`.

A `<` after the cast type is read as a type argument list when the tokens
that follow form one, and as a less-than comparison otherwise, so
`n as Int < 10` compares while `xs as List<Int>` casts.

## Casting to state what a value holds

The analyser only accepts what it can prove. Where it cannot, a cast states the
value's type and checks it at the cast site.

Reassigning a variable widens its type, which can lose the detail a function
needs:

```ndc
let line = "a b c";
line = line.split(" ");    // line is now Sequence<String>, not List<String>
line.remove(0);            // error: no `remove` matches Sequence<String>, Int
assert_eq((line as List<String>).remove(0), "a");
```

A specialized `op=` mutates its target in place and keeps its type, so it only
accepts a right operand that provably fits. A value of unknown type does not,
and a cast gets the elements checked instead of letting them slip into the
target unverified:

```ndc
fn opaque(x) => x;
let values = [1];
values ++= opaque([2, 3]) as List<Int>;
assert_eq(values, [1, 2, 3]);
```

## Limitations

Checking an iterator's elements would consume it, so a runtime check against
an element type other than `Any` fails. Cast a value into a typed collection
before turning it into an iterator.
