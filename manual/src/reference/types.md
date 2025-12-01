# Types

Andy C++ is currently a dynamically typed language, that means that type checks are performed at runtime. Although
you currently can't annotate your variables using type names they do have types at runtime.

The type system is hierarchical with the root type being `Any`:

* Any
  * [Option](./types/option.md)
  * [Boolean](./types/boolean.md)
  * [Number](./types/number.md)
    * Integer
      * Int64 (64bit signed)
      * Bigint (unlimited size)
    * Float
    * Complex
    * Rational
  * Sequence
    * [String](./types/string.md): A mutable list of characters
    * [List](./types/list.md): A mutable list
    * [Tuple](./types/tuple.md): An immutable list
      * [Unit](./types/unit.md)
    * [Map](./types/map-and-set.md): A hashmap that associates keys with values
    * [Deque](./types/deque.md): A double ended queue
    * [MinHeap & MaxHeap](./types/min-max-heap.md): Min/max Heap
    * Iterator: A type that can be consumed and produces values (Currently only used for range expressions like `5..100`)
  * [Function](./types/function.md)

> **Note:** `Any` is the base type for all other types. When you declare a function, its arguments default to type `Any`.
> Currently, the `Any` type is implicit and does not appear explicitly in the language.
