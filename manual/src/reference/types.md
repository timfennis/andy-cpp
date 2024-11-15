# Types

Andy C++ is currently a dynamically typed language, that means that type checks are performed at runtime. Although
you currently can't annotate your variables using type names they do have types at runtime.

The type system is hierarchical with the root type being `Any`:

* Any
  * Unit
  * Boolean
  * Number
    * Integer
      * Int64 (64bit signed)
      * Bigint (unlimited size)
    * Float
    * Complex
    * Rational
  * Sequence
    * String
    * List
    * Tuple
    * Map
    * Iterator
  * Function

## Unit

In Andy C++ the unit type is written as `()` and indicates that there is no value. Statements evaluate
to the unit type for instance when a semicolon is used after the last value.

```ndc
let result = if x == y {
  5 + 5;
} else {
  10 + 10;
};

// Result is unit because the last expression in the blocks has a semicolon
assert_eq(result, ());
```

The language does not have `null` or `nil`.

## Boolean

Booleans in Andy C++ represent a binary state, capable of holding either the value `true` or `false`.
Unlike some other languages, where booleans are often used as numbers, Andy C++ treats them as a
distinct type. When attempting to perform arithmetic operations with booleans, such as `true + true`,
Andy C++ will throw an error because these operations are not defined for this type.

Operators defined for booleans:
 * `&` non-lazy and
 * `|` non-lazy or
 * `~` non-lazy xor
 * `!` not
 * `or` lazy logical or
 * `and` lazy logical and
 * `not` logical not like `!` but lower presedence

## Numbers

Andy C++ has four number types that you should keep in mind when programming.

 * Int: which is subdivided into `Int64` and `BigInt` to support arbitrarily large numbers
 * Float: which is backed by an `f64`
 * Rational: which consists of two `BigInt`s and represents a fraction
 * Complex: which is a pair of two `f64`'s

The following operators are available:

 * `+`: Addition
 * `-`: Subtraction
 * `*`: Multiplication
 * `/`: Division
    * For integers division might create rational numbers
 * `^`: Exponentiation
 * `%`: C-style modulo (can be negative)
 * `%%`: Remainder of euclidean division (always positive)

Additionally for integers the following operations are available:

 * `|`: Bitwise OR
 * `&`: Bitwise AND
 * `~`: Bitwise XOR or bitwise NOT when used in unary position
 * `>>`: Bitshift right (not yet implemented)
 * `<<`: Bitshift left (not yet implemented)
### Integers

Andy C++ uses signed 64-bit integers under the hood to do math. But if you write an expression that would normally overflow it
automatically switches to a `BitInt` and calculates the result using the [num crate](https://crates.io/crates/num). The advantage
of this is that you can quickly compute some really big numbers, but the downside is that naive solutions to puzzles like
[Advent of Code 2022 - Day 11](https://adventofcode.com/2022/day/11) will never throw an error and continue running until the end
of the universe.

```ndc
let result = 2 ^ 1024;

// Result:
// 1797693134862315907729305190789024733617976978942
// 3065727343008115773267580550096313270847732240753
// 6021120113879871393357658789768814416622492847430
// 6394741243777678934248654852763022196012460941194
// 5308295208500576883815068234246288147391311054082
// 7237163350510684586298239947245938479716304835356
// 329624224137216
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

You can use both `i` and `j` to create complex numbers.

```ndc
let complex = 5.0 + 3.1j;
let result = complex * 1.3; // 6.5+4.03i
let result = complex + 5.3; // 10.3+3.1i
```

## Sequence

Sequences are a collection of types that can be iterated over. There are currently 5 types of sequences:

 * String: A mutable list of characters
 * List: A mutable list
 * Tuple: An immutable list
 * Map: A hashmap that associates keys with values
 * Iterator: A type that can be consumed and produces values (Currently only used for range expressions like `5..100`)

### String

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


### List

Lists are mutable constructs in Andy C++.

```ndc
let my_list = [1,2,3];
my_list[2] = 4; // Change value in list

my_list.push(99); // Add element to the end of the list
my_list.pop_first(); // Pop the head off the list (note that this is slow because we use a Vec behind the scenes)
```


### Tuples

Tuples are a lot like lists but they are immutable.

```ndc
let my_tuple = (1,2,3);

// You can index them as if they are lists
assert_eq(my_tuple[0], 1);

// But you're not allowed to modify tuples
my_tuple[0] = 99; // ERROR
```

### Maps and Sets

Andy C++ does not have separate map and set types. A set is just a map where all the values are unit `()`.

```
let my_set = %{5, 3, 2, 1};
let my_map = %{
  "apples": 60,
  "oranges": 22,
  "bananas": 0,
};
```

Maps and sets are mutable datastructures.

## Functions

Functions are first class citizens in the type system and can be stored in variables and passed around as arguments.

```ndc
// An anonymous function that adds its operands together
let my_function = fn(a, b) { a + b };

assert_eq(my_function(5, 3), 8);

fn foo() {
  // whatever
}

let my_variable = foo; // Store the foo-function in a variable
```
