# Casts

The `as` operator asserts that a value has the given type:

```ndc
let values: List<Any> = [1, 2];

// values is a List<Any>; the cast recovers the element type so the
// result works with functions that expect a List<Int>.
let ints = values as List<Int>;
print(ints.sum());  // 3
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
`String` can't be a `Fn(Int) -> Int`. What a function returns often isn't
known until it runs, so a function that may fit a function type returns a
function that checks each call's result:

```ndc,fails
fn text(x) => "s";             // its value takes and returns anything
let value: Any = text;
let f = value as Fn(Int) -> Int;
f(1)
// error[vm]: expected the function to return Int but it returned String
```

Inside a container, where the function can't be replaced by one that checks
its calls, only a function whose own type shows it fits does. So does an
overload picked as the program runs from a value of type `Any`: cast the
value to the function type first to have its calls checked.

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

It checks a value the way `as` does, so a container is checked element by
element, and a function that may fit a function type comes out checking its
calls:

```ndc
let mixed: Any = [1, "a"];
print(mixed as? List<Int>);  // None
print(mixed as? List<Any>);  // Some([1,"a"])

fn inc(x: Int) => x + 1;
let f: Any = inc;
print((f as? Fn(Int) -> Int).unwrap()(2));  // 3
print((f as? Fn(String) -> Int).is_some());  // false
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

An `as?` that can never succeed is rejected like the `as` it stands for,
since it would always be `None`:

```ndc,fails
"foo" as? Int
// error[analysis]: invalid cast: String can never be Int
```

## Precedence

`as` and `as?` bind tighter than binary operators and looser than unary operators
and calls, so `a + b as Int` means `a + (b as Int)`. Method calls bind
tighter than the cast: recovering an element type before a method call
needs parentheses, as in `(values.keys as List<Int>).max()`. An `as?` cast
needs them too before a method call on the option: `(value as? Int).is_some()`.
A name can contain `?`, so `as?` needs a space after it: `x as?Int` reads as
`x` followed by a name `as?Int`.

A `<` after the cast type is read as a type argument list when the tokens
that follow form one, and as a less-than comparison otherwise, so
`n as Int < 10` compares while `xs as List<Int>` casts.

## Casting to state what a value holds

The analyser only accepts what it can prove. Where it cannot, a cast states the
value's type and checks it at the cast site.

A wider annotation than the value needs can lose the detail a function needs:

```ndc,fails
let values: List<Any> = [1, 2, 3];
values.sum();  // error: no `sum` matches List<Any>
```

A cast states what the list holds:

```ndc
let values: List<Any> = [1, 2, 3];
print((values as List<Int>).sum());  // 6
```

A specialized `op=` such as `++=` changes its target in place, so it's only
used with a right operand that provably fits. With a value of unknown type,
`a op= b` makes a new value like `a = a op b` does; a cast checks the elements
first, so the target can be changed in place:

```ndc
fn opaque(x) => x;
let values = [1];
values ++= opaque([2, 3]) as List<Int>;
print(values);  // [1,2,3]
```

## Limitations

Checking an iterator's elements would consume it, so a runtime check against
an element type other than `Any` fails. Cast a value into a typed collection
before turning it into an iterator.
