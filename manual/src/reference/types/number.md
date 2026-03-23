# Numbers

Andy C++ has four number types:

 * Int: which is subdivided into `Int64` and `BigInt` to support arbitrarily large numbers
 * Float: which is backed by an `f64`
 * Rational: which consists of two `BigInt`s and represents a fraction
 * Complex: which is a pair of two `f64`'s

## Operators

| Operator | Function | Support augmented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `+` | Addition | `true` | `false` |
| `-` | Subtraction | `true` | `false` |
| unary `-` | Negation | `true` | `false` |
| `*` | Multiplication | `true` | `false` |
| `/` | Division (returns rational for integers) | `true` | `false` |
| `\` | Floor division (integer result, rounds toward negative infinity) | `true` | `false` |
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

Integers also support these operations:

| Operator | Function | Support augmented assignment <sup>[[1]](../../features/augmented-assignment.md)</sup> | Augmentable with `not` |
| :-: | --- | --- | --- |
| `\|` | Bitwise OR | `true` | `false` |
| `&` | Bitwise AND | `true` | `false` |
| `~` | Bitwise XOR, or bitwise NOT in unary position | `true` | `false` |
| unary `~` | bitwise NOT | `true` | `false` |
| `>>` | Bitshift right | `true` | `false` |
| `<<` | Bitshift left | `true` | `false` |

### Integers

Andy C++ uses signed 64-bit integers until an expression overflows. At that point it switches to `BigInt` and computes the result with the [num crate](https://crates.io/crates/num). You can compute very large numbers this way, but code such as a naive solution to [Advent of Code 2022 - Day 11](https://adventofcode.com/2022/day/11) may keep allocating until you run out of memory.

```ndc
let result = 2 ^ 1024;

// Result: 179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216
```

### Rational numbers and Floats
The math system keeps exact values unless you ask for a float. Dividing integers produces a rational number, not a float. An expression produces a float only when one of its operands is a float.

One exception exists: raising an integer to a rational power produces a float.

For example:
```ndc
let result = 5^(1/2);

// result is Float because Int^ on Rational results in Float
assert_eq(result, 2.23606797749979); // Using == assertions on floats is risky
```

### Complex numbers

Andy C++ supports complex numbers. Use either `i` or `j` as the imaginary unit. Create a complex number by combining a real part with an imaginary part.

```ndc
let complex = 5.0 + 3.1j;
let result = complex * 1.3; // 6.5+4.03i
let result = complex + 5.3; // 10.3+3.1i
```
