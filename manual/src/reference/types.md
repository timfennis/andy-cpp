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

### Element types are exact

A `List<Int>` is not a `List<Any>`: a `List<Any>` could be given a String, which the list of Ints would then hold. The same goes for maps, tuples, options, the other containers, and function types. To change a container's element type, go through `Any`:

```ndc,fails
let words = "a b".split(" ");     // List<String>
let any: Any = words;
let anything: List<Any> = any;    // fine: checked as the program runs
let wrong: List<Any> = words;     // error: found List<String> but expected List<Any>
```

A literal takes the type its use needs, so an annotation can widen it directly. A literal whose values share no type is a container of `Any`:

```ndc
let xs: List<Any> = [1, 2];
xs.push("three");
let mixed = [1, "a"];             // List<Any>
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

### Using an `Any` value where a type is expected

You can use an `Any` value where a specific type is expected. The program checks the value when it gets there:

```ndc,fails
let a: Any = 5;
let n: Int = a;      // fine: a holds an Int
print(n + 1);        // 6

let b: Any = "five";
let m: Int = b;      // error: expected Int but found String
```

The same goes for assigning it, passing it to a function, or returning it from one. For a list or a map, every element is checked.

You can also check it yourself with a [cast](../features/casts.md): `value as List<Int>`.
