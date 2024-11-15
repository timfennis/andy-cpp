# String

In Andy C++ a String is a mutable list of characters. Characters don't have their own type in the language so if you
iterate over a string you get strings of length 1. Just like in Rust strings are guaranteed (and required) to be valid
UTF-8. This means that you can't store arbitrary binary data in a String.

Indexing into a String is done using the characters in the string and not using byte offsets (this was probably a mistake).
This means that indexing into a string is `O(n)` instead of `O(1)`.

```ndc
let string = "I ❤ Andy C++";
assert_eq(string[2], "❤");
assert_eq(string[4], "A");
```

The advantage is that it's a bit easier to guess that `A` is the 4th character in the example above. The downside is that
depending on which heart you're using there might be an invisible [Variation Selector](https://en.wikipedia.org/wiki/Variation_Selectors_(Unicode_block))
after the heart which messes everything up. This specific behavior will probably change in the future.

## Operators

| Operator | Function |
| --- | --- |
| `++` | concatenate two strings |
| `<>` | coerces both operands into strings before concatenating them |
| `in` | check if left operand is a substring of right operand |

### Examples

```ndc
let a = "foo" ++ 3; // Error: cannot concatenate string and int
let b = "foo" <> 3; // foo3
let c = 10 <> 5.0; // 105.0
let d = "oo" in "foobar"; // true
```
