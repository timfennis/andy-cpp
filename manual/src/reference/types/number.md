# Numbers

Andy C++ exposes three sibling numeric types:

* `Int` stores a signed 64-bit integer. Checked arithmetic reports overflow. The remainder
  operators `%` and `%%` are the exception: once the divisor is non-zero the result always
  fits, even where the quotient it implies would not.
* `Float` stores an IEEE 754 `f64`.
* `Number` supports arbitrary-size integers, exact rational values, floats, and complex values.

`Int`, `Float`, and `Number` share `Any` as their nearest common supertype. An `Int` does not satisfy a `Number` annotation. Use an `n` literal or the `Number` constructor when you need the advanced mode:

```ndc
let count: Int = 42;
let measurement: Float = 42.0;
let exact: Number = 42n;

assert_eq(Number(42), 42n);
assert_eq(Number(42.0), 42.0n);
```

## Literals

The `n` suffix creates a `Number` from a decimal integer or float. Binary, octal, and hexadecimal integers also accept it:

```ndc
let large = 123456789123456789123456789n;
let decimal = 1.25n;
let binary = 0b101010n;
let octal = 0o52n;
let hexadecimal = 0x2an;
```

An integer literal without `n` must fit in `i64`. The lexer reports an error and suggests the suffixed form when it does not fit.

Arbitrary-radix literals such as `16r2a` remain `Int` literals and do not accept `n`. The `i` and `j` suffixes create complex `Number` values:

```ndc
let z: Number = 2 + 3i;
assert_eq(z, 2 + 3j);
```

## Arithmetic modes

The arithmetic operators `+`, `-`, `*`, `/`, `\`, `%`, `%%`, and `^` define all nine pairs of `Int`, `Float`, and `Number`. The operands select the result type:

| Operands | Result |
| --- | --- |
| `Int`, `Int` | `Int` |
| `Int`, `Float` or `Float`, `Int` | `Float` |
| `Float`, `Float` | `Float` |
| Any pair containing `Number` | `Number` |

`Int` uses checked `i64` arithmetic. `/` truncates toward zero, while `\` rounds toward negative infinity. `%` pairs with truncating division and `%%` returns a Euclidean remainder:

```ndc
assert_eq(-7 / 2, -3);
assert_eq(-7 \ 2, -4);
assert_eq(-7 % 2, -1);
assert_eq(-7 %% 2, 1);
```

`Float` follows IEEE 754 behavior. `Number` keeps integer and rational operations exact when it can:

```ndc
assert_eq(7 / 2, 3);
assert_eq(7n / 2n, 7n / 2n);
assert_eq(2n ^ 100n, 1267650600228229401496703205376n);
```

Integer powers and shifts must fit their checked `Int` result. Use `Number` for negative exponents, arbitrary-size powers, and complex continuation. Roots, logarithms, inverse trigonometric functions, and fractional powers return a complex `Number` when the real result does not exist:

```ndc
assert_eq(5n ^ -1n, 1n / 5n);
assert_eq(sqrt(-1n), 1i);
```

## Division by zero

`Int` division and remainder by zero report an error. `Float` returns IEEE infinity or NaN. `Number` also falls back to a wrapped Float result when an exact zero divisor has no rational representation:

```ndc
assert_eq(1n / 0n, Inf);
assert_eq(1n \ 0n, Inf);

let nan = 0n / 0n;
assert(nan == nan);
```

Both remainder operators follow the same rule, so `5n % 0n` and `5n %% 0n` are
`NaN` where `5 % 0` and `5 %% 0` are errors. Moving an accumulator from `Int`
to `Number` therefore trades the zero-divisor diagnostic for a value that
propagates.

## Equality, hashing, and ordering

Numeric equality compares exact values across all three modes. Equal values produce the same map or set hash. Andy C++ converts each finite Float to its exact binary rational value for this comparison, so decimal approximation does not make values equal:

```ndc
assert(1 == 1.0);
assert(1.0 == 1n);
assert(1n == 1 + 0i);
assert(0.1 != 1n / 10n);
assert_eq(%{1, 1.0, 1n, 1 + 0i}.len(), 1);
```

Positive and negative zero compare equal. All NaN values compare equal and hash alike. Real scalars sort in this order:

```text
-Inf < finite values < Inf < NaN
```

Complex values keep lexicographic ordering. The comparison checks the real part, then the imaginary part:

```ndc
assert((2 + 0i) > (1 + 100i));
assert((1 + 2i) < (1 + 3i));
```

## Integer-only operators

Only `Int` supports bitwise operations and shifts:

| Operator | Function |
| :-: | --- |
| `\|` | Bitwise OR |
| `&` | Bitwise AND |
| `~` | Binary XOR or unary NOT |
| `>>` | Checked right shift |
| `<<` | Checked left shift |

Use `Int` values for list indices, range bounds, and APIs that take counts. Convert with `int(value)` when the value fits in `i64`.
