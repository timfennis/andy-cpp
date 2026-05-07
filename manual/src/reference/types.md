# Types

Andy C++ runs as a dynamically typed language — values carry their types at runtime and most checking happens then. You can also attach type annotations to variables, function parameters, and return values, and the analyser will use them to flag obvious mismatches before the program runs.

The type system is hierarchical with `Any` at the root:

* `Any`
  * [`Option<T>`](./types/option.md)
  * [`Bool`](./types/boolean.md)
  * [`Number`](./types/number.md)
    * `Int` — machine `i64` or arbitrary-precision `BigInt`, picked automatically
    * `Float`
    * `Complex`
    * `Rational`
  * `Sequence<T>`
    * [`String`](./types/string.md): a mutable list of characters
    * [`List<T>`](./types/list.md): a mutable list
    * [`Tuple<T, ...>`](./types/tuple.md): an immutable list
      * [`()`](./types/unit.md): unit, the empty tuple
    * [`Map<K, V>`](./types/map-and-set.md): a hashmap that associates keys with values
    * [`Deque<T>`](./types/deque.md): a double-ended queue
    * [`MinHeap<T>` / `MaxHeap<T>`](./types/min-max-heap.md): min/max heap
    * `Iterator<T>`: produces values when consumed (currently only from range expressions like `5..100`)
  * [`Function`](./types/function.md)

These are also the names you write in annotations. Generic types take their parameters in angle brackets:

```ndc
let xs: List<Int> = [1, 2, 3];
let table: Map<String, Int> = %{"a": 1, "b": 2};
let maybe: Option<Any> = Some("hi");
let pair: Tuple<Int, String> = (1, "hi");
let pair2: (Int, String) = (1, "hi");  // tuple shorthand
```

Nested generics work too — the parser handles the `>>` ambiguity for you:

```ndc
let grid: List<List<Int>> = [[1, 2], [3, 4]];
```

> **Note:** `Any` is the base type for every other type, so an `Any`-annotated binding will accept anything. When a parameter or value has no annotation and the analyser can't infer a type, it falls back to `Any`. There is also a `Never` type used internally for things like `break` that don't produce a value — you'll rarely need to write it by hand.
