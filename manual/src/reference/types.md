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
result := if x == y {
  5 + 5;
} else {
  10 + 10;
};

// Result is unit because the last expression in the blocks has a semicolon
assert_eq(result, ());
```

The language does not have `null` or `nil`.

## Boolean

Booleans are a special type that can either contain the value `true` or `false`. Booleans are not treated
as numbers like many languages do; expressions like `true + true` result in an error instead of `2`.

Operators defined for booleans:
 * `&` non-lazy and
 * `|` non-lazy or
 * `~` non-lazy xor
 * `!` not
 * `or` lazy logical or
 * `and` lazy logical and
 * `not` logical not like `!` but lower presedence
