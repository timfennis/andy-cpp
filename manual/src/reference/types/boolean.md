# Boolean

Booleans in Andy C++ represent a binary state, capable of holding either the value `true` or `false`.
Unlike some other languages, where booleans are often used as numbers, Andy C++ treats them as a
distinct type. When attempting to perform arithmetic operations with booleans, such as `true + true`,
Andy C++ will throw an error because these operations are not defined for this type.

Operators defined for booleans:

| Operator | Function |
| :-: | --- |
| `&` | non-lazy and |
| `\|` | non-lazy or |
| `~` | non-lazy xor |
| `!` | not |
| `or` | lazy logical or |
| `and` | lazy logical and |
| `not` | logical not like `!` but lower presedence |

## Lazy evaluation of `and` and `or`

The `and` and `or` operators are **lazy**, meaning their operands are evaluated only when necessary to determine the result.
This differs from bitwise operators (`|`, `&`, `~`), which always evaluate both operands.

```ndc
fn a() {
    print("a invoked");
    false
};

fn b() {
    print("b invoked");
    true
};

// Only "a invoked" is printed because evaluation is lazy.
if a() and b() {
    // ...
}
```
