# String

In Andy C++ a String is a mutable list of characters. Characters don't have their own type in the language so if you
iterate over a string you get strings of length 1. Just like in Rust strings are guaranteed (and required) to be valid
UTF-8. This means that you can't store arbitrary binary data in a String.

Indexing into a String is done by UTF-8 codepoint (equivalent to Rust's `char`) rather than by byte offset.
The first indexed read builds a character index in `O(n)` time. Subsequent reads use
that cache: `O(1)` for ASCII strings and `O(log n)` in the worst case for Unicode strings.
Mutating the string discards the cache, so the next indexed read rebuilds it.

```ndc
let string = "I ❤ Andy C++";
assert_eq(string[2], "❤");
assert_eq(string[4], "A");
```

The advantage is that it's a bit easier to guess that `A` is the 4th character in the example above. The downside is that
depending on which heart you're using there might be an invisible [Variation Selector](https://en.wikipedia.org/wiki/Variation_Selectors_(Unicode_block))
after the heart which messes everything up. This specific behavior will probably change in the future.

# Raw strings

You may also define raw strings using this syntax borrowed from rust.

```ndc
let string = r#"raw string with "" lots of "" quotes"#;
```

and even

```ndc
let string = r###"raw string with r#"stop doing this"#"###;
```

## Removing prefixes and suffixes

`strip_prefix(string, prefix)` and `strip_suffix(string, suffix)` remove one matching
prefix or suffix. `strip_circumfix(string, prefix, suffix)` removes both only when
both match without overlapping. Each function returns a new string, leaving the
original string unchanged, and it's an error when the pattern doesn't match. Matches
are exact and case-sensitive; empty patterns match without removing any text.

```ndc
assert_eq("foobar".strip_prefix("foo"), "bar");
assert_eq("foobar".strip_suffix("bar"), "foo");
assert_eq("[hello]".strip_circumfix("[", "]"), "hello");
assert_eq("foofoo".strip_prefix("foo"), "foo");
assert_eq("[]".strip_circumfix("[", "]"), "");
"hello".strip_prefix("x");   // ERROR: "hello" doesn't start with "x"
```

When a mismatch isn't an error, the `?` variants return an `Option` instead:

```ndc
assert_eq("foobar".strip_prefix?("foo"), Some("bar"));
assert_eq("hello".strip_prefix?("x"), None);
assert_eq("aaa".strip_circumfix?("aa", "aa"), None);
```

## Splitting once

`split_once(string, pattern)` splits a string at the first occurrence of `pattern` into
the parts before and after it. It's an error when `pattern` doesn't occur, and
`split_once?` returns `None` instead:

```ndc
let (key, value) = "port=80".split_once("=");
assert_eq(key, "port");
assert_eq("a=b=c".split_once("="), ("a", "b=c"));
assert_eq("abc".split_once?("="), None);
```

## Operators

| Operator | Function |
| --- | --- |
| `++` | concatenate two strings |
| `<>` | coerces both operands into strings before concatenating them |
| `in` | check if left operand is a substring of right operand |
| `==` | Equality |
| `!=` | Inequality |
| `>` | Greater (lexicographically) |
| `<` | Less (lexicographically) |
| `>=` | Greater equals (lexicographically) |
| `<=` | Less equals (lexicographically) |
| `<=>` | `-1`, `0`, or `1` (lexicographically) |
| `>=<` | `-1`, `0`, or `1` (reverse lexicographically) |

> **Note:** The `in` operator checking if the left operand is a substring of the right operand is different from
> the behavior of `in` on lists.

### Examples

```ndc
let a = "foo" ++ 3; // Error: cannot concatenate string and int
let b = "foo" <> 3; // foo3
let c = 10 <> 5.0; // 105.0
let d = "oo" in "foobar"; // true
```
