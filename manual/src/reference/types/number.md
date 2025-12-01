# Numbers

Andy C++ has four number types that you should keep in mind when programming.

 * Int: which is subdivided into `Int64` and `BigInt` to support arbitrarily large numbers
 * Float: which is backed by an `f64`
 * Rational: which consists of two `BigInt`s and represents a fraction
 * Complex: which is a pair of two `f64`'s

## Operators

| Operator | Function | Support agumented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `+` | Addition | `true` | `false` |
| `-` | Subtraction | `true` | `false` |
| unary `-` | Negation | `true` | `false` |
| `*` | Multiplication | `true` | `false` |
| `/` | Division | `true` | `false` |
| `^` | Exponentiation | `true` | `false` |
| `%` | C-style modulo (can be negative) | `true` | `false` |
| `%%` | Remainder of [euclidean division](https://en.wikipedia.org/wiki/Euclidean_division) | `true` | `false` |
| `==` | Strict equality | `false` | `true` |
| `<=` | Less or equal | `false` | `true` |
| `<` | Less than | `false` | `true` |
| `>=` | Greater or equal | `false` | `true` |
| `>` | Greater than | `false` | `true` |
| `!=` | Not equal | `false` | `true` |
| `<=>` | Compare | `false` | `false` |
| `>=<` | Reverse compare | `false` | `false` |
| `<>` | Concatenate string values | `true` | `false` |

Additionally for **integers** the following operations are available:

| Operator | Function | Support agumented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `\|` | Bitwise OR | `true` | `false` |
| `&` | Bitwise AND | `true` | `false` |
| `~` | Bitwise XOR, or bitwise NOT in unary position | `true` | `false` |
| unary `~` | bitwise NOT | `true` | `false` |
| `>>` | Bitshift right | `true` | `false` |
| `<<` | Bitshift left | `true` | `false` |

### Integers

Andy C++ uses signed 64-bit integers under the hood to do math. But if you write an expression that would normally overflow it
automatically switches to a `BitInt` and calculates the result using the [num crate](https://crates.io/crates/num). The advantage
of this is that you can quickly compute some really big numbers, but the downside is that naive solutions to puzzles like
[Advent of Code 2022 - Day 11](https://adventofcode.com/2022/day/11) will never throw an error and continue running you run out of memory.

```ndc
let result = 2 ^ 1024;

// Result: 179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216
```

### Rational numbers and Floats
The math system is designed to never lose precision unless you ask for it. That means that dividing with integers always results
in a rational number instead of a float. Expression will only have a float as a result if one of the operands is a float.

However, there is an exception when raising an integer to the power of a rational. In such cases, Andy C++ will use a float to
represent the result.

For example:
```ndc
let result = 5^(1/2);

// result is Float because Int^ on Rational results in Float
assert_eq(result, 2.23606797749979); // Using == assertions on floats is risky
```

### Complex numbers

Complex numbers are supported in the language, and you can use both i and j as the imaginary unit. Complex numbers can be
created by adding a real number and an imaginary number, represented with either i or j.

```ndc
let complex = 5.0 + 3.1j;
let result = complex * 1.3; // 6.5+4.03i
let result = complex + 5.3; // 10.3+3.1i
```
