# String

In Andy C++ a String is a mutable list of characters. Characters don't have their own type in the language so if you
iterate over a string you get strings of length 1. Just like in Rust strings are guaranteed (and required) to be valid
UTF-8. This means that you can't store arbitrary binary data in a String.

Indexing into a String counts characters (UTF-8 codepoints, like Rust's `char`) rather than bytes.

```ndc
let string = "I ❤ Andy C++";
print(string[2]);  // ❤
print(string[4]);  // A
```

The advantage is that it's a bit easier to guess that `A` is at index 4 in the example above. The downside is that
depending on which heart you're using there might be an invisible [Variation Selector](https://en.wikipedia.org/wiki/Variation_Selectors_(Unicode_block))
after the heart which messes everything up. This specific behavior will probably change in the future.

# Escape sequences

Inside a string, a backslash starts an escape: `\n` is a newline, `\t` a tab, `\r` a carriage return, `\0` a null
character, `\\` a backslash and `\"` a double quote. `\u{...}` is the Unicode character with the hexadecimal code point
between the braces:

```ndc
print("caf\u{e9} \u{1F600}");  // café 😀
```

# Raw strings

You may also define raw strings using this syntax borrowed from rust.

```ndc
let string = r#"raw string with "" lots of "" quotes"#;
print(string);  // raw string with "" lots of "" quotes
```

and even

```ndc
let string = r###"raw string with r#"stop doing this"#"###;
print(string);  // raw string with r#"stop doing this"#
```

## Removing prefixes and suffixes

`strip_prefix(string, prefix)` and `strip_suffix(string, suffix)` remove one matching
prefix or suffix. `strip_circumfix(string, prefix, suffix)` removes both. Each
function returns a new string, leaving the original string unchanged, and it's
an error when the pattern doesn't match.

```ndc
print("foobar".strip_prefix("foo"));         // bar
print("foobar".strip_suffix("bar"));         // foo
print("[hello]".strip_circumfix("[", "]"));  // hello
print("foofoo".strip_prefix("foo"));         // foo
print("[]".strip_circumfix("[", "]") == "");  // true
```

A mismatch is an error:

```ndc,fails
"hello".strip_prefix("x");  // error: "hello" doesn't start with "x"
```

The `?` variants return an `Option` instead of failing:

```ndc
print("foobar".strip_prefix?("foo"));         // Some("bar")
print("hello".strip_prefix?("x"));            // None
print("aaa".strip_circumfix?("aa", "aa"));    // None
```

## Splitting once

`split_once(string, pattern)` splits a string at the first occurrence of `pattern` into
the parts before and after it. It's an error when `pattern` doesn't occur, and
`split_once?` returns `None` instead:

```ndc
let (key, value) = "port=80".split_once("=");
print(key);                       // port
print("a=b=c".split_once("="));   // ("a","b=c")
print("abc".split_once?("="));    // None
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
print("foo" <> 3);        // foo3
print(10 <> 5.0);         // 105.0
print("oo" in "foobar");  // true
```

```ndc,fails
let a = "foo" ++ 3; // Error: No function called '++' found that matches the arguments 'String, Int'
```
