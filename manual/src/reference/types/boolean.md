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
| `not` | logical not like `!` but lower precedence |

See [Logical operators](../logical-operators.md) for short-circuiting and precedence rules.
