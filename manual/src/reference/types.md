# Types

Andy C++ runs as a dynamically typed language — values carry their types at runtime and most checking happens then. You can also attach type annotations to variables, function parameters, and return values, and the analyser will use them to flag obvious mismatches before the program runs.

The type system is hierarchical with `Any` at the root:

* `Any`
  * [`Option<T>`](./types/option.md)
  * [`Bool`](./types/boolean.md)
  * [`Int`](./types/number.md) — checked signed 64-bit integers
  * [`Float`](./types/number.md) — IEEE 754 `f64` values
  * [`Number`](./types/number.md) — arbitrary-size, rational, floating-point, and complex values
  * `Sequence<T>`
    * [`String`](./types/string.md): a mutable list of characters
    * [`List<T>`](./types/list.md): a mutable list
    * [`Tuple<T, ...>`](./types/tuple.md): an immutable list
      * [`()`](./types/unit.md): unit, the empty tuple
    * [`Map<K, V>`](./types/map-and-set.md): a hashmap that associates keys with values
    * [`Deque<T>`](./types/deque.md): a double-ended queue
    * [`MinHeap<T>` / `MaxHeap<T>`](./types/min-max-heap.md): min/max heap
    * `Iterator<T>`: produces values when consumed (currently only from range expressions like `5..100`)
  * [`Fn(A, B) -> C`](./types/function.md): a function taking an `A` and a `B` and returning a `C`
  * [Structs](./types/struct.md): user-defined record types, one per `struct` declaration

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

[Struct](./types/struct.md) names are type names like any other, including inside generics:

```ndc
struct Point { x: Int, y: Int }

fn manhattan(p: Point) => p.x + p.y
let points: List<Point> = [Point(1, 2), Point(3, 4)];
```

A struct name refers to the struct declared earlier in an enclosing scope; it cannot be
used before (or inside) its own declaration, and it goes out of scope together with the
block or function that declared it. See [Struct](./types/struct.md) for the scoping
rules.

> **Note:** `Any` is the base type for every other type, so an `Any`-annotated binding will accept anything. When a value has no annotation and the analyser can't infer a type, it falls back to `Any`. A parameter of a function declared in the program gets the type of each call's argument instead (see [Generic functions](./types/function.md#generic-functions)). There is also a `Never` type used internally for things like `break` that don't produce a value — you'll rarely need to write it by hand.

A value of type `Any` also goes where another type is expected, and is checked to have that type when the program gets there:

```ndc
let a: Any = 5;
let n: Int = a;      // checked: a holds an Int
fn first(xs: Any) -> Int { xs[0] }
print(first([3]));   // checked: xs[0] is an Int
```

The check happens when a value is stored in an annotated variable, assigned to a variable, an element or a field, passed for a parameter whose type is known, or returned from a function with an annotated return type. If it fails, the program stops with an error like `expected Int but found String`. A container is checked element by element.

To recover a precise type explicitly, use a [cast](../features/casts.md): `value as List<Int>`.
