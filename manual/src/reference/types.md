# Types

Every value in Andy C++ has a type, but you rarely have to write one down. Before the program runs, Andy C++ works out the type of everything in it and reports code that can't work. Annotations on variables, parameters and return values are optional.

## Mistakes found before the program runs

A mistake is found even in code that never runs, and the error points through the calls to the line that can't work. This program prints nothing:

```ndc,fails
fn inc(x) => x + 1;
fn shout(s) => inc(s) ++ "!";
print("hello");
if false {
    shout("a");  // error: No function called '+' found that matches the arguments 'String, Int'
}
```

An anonymous function knows the types of its parameters:

```ndc,fails
struct Player { name: String, score: Int }
let players = [Player("ann", 3), Player("bob", 5)];
players.map(fn(p) => p.score ++ " points");
// error: No function called '++' found that matches the arguments 'Int, String'
```

An empty list gets its type from what you put in it:

```ndc,fails
let xs = [];
xs.push(1);
xs.push("two");  // error: No function called 'push' found that matches the arguments '[Int], String'
```

If nothing is put in it, annotate it: `let xs: [Int] = [];`.

## The types

Every value is an `Any`. The other types are:

* `Any`
  * [`Option<T>`](./types/option.md)
  * [`Bool`](./types/boolean.md)
  * [`Int`](./types/number.md) — checked signed 64-bit integers
  * [`Float`](./types/number.md) — IEEE 754 `f64` values
  * [`Number`](./types/number.md) — arbitrary-size, rational, floating-point, and complex values
  * Sequences, which a `for` loop iterates over:
    * [`String`](./types/string.md): a mutable list of characters
    * [`[T]`](./types/list.md): a mutable list
    * [`(T, ...)`](./types/tuple.md): an immutable list
      * [`()`](./types/unit.md): unit, the empty tuple
    * [`%{K: V}`](./types/map-and-set.md): a hashmap that associates keys with values
    * [`Deque<T>`](./types/deque.md): a double-ended queue
    * [`MinHeap<T>` / `MaxHeap<T>`](./types/min-max-heap.md): min/max heap
    * [`Range` / `RangeFrom`](./types/range.md): the `Int`s from one number to another, `5..100`, or from one number on, `5..`
    * [`Lazy<T>`](./types/lazy.md): a sequence that makes its elements when it's read, `xs.lazy.map(f)`
  * [`fn(A, B) -> C`](./types/function.md): a function taking an `A` and a `B` and returning a `C`
  * [Structs](./types/struct.md): user-defined record types, one per `struct` declaration

These are also the names you write in annotations. Generic types take their parameters in angle brackets:

```ndc
let xs: [Int] = [1, 2, 3];
let table: %{String: Int} = %{"a": 1, "b": 2};
let maybe: Option<String> = Some("hi");
let pair: (Int, String) = (1, "hi");
let grid: [[Int]] = [[1, 2], [3, 4]];

print(maybe, pair, grid);  // Some("hi") (1,"hi") [[1,2],[3,4]]
```

[Struct](./types/struct.md) names are type names like any other:

```ndc
struct Point { x: Int, y: Int }

fn manhattan(p: Point) => p.x + p.y
let points: [Point] = [Point(1, 2), Point(3, 4)];
print(points.map(manhattan));  // [3,7]
```

## Values of different types

The elements of a list, the keys and values of a map, the branches of an `if`, and the values a function returns all share one type. Numbers mix: `[1, 2.5]` is a list of Floats. Values that share no type are an error:

```ndc,fails
let mixed = [1, "a"];  // error: values of different types: Int and String share no type
```

To mix them, say that they are `Any`s:

```ndc
let b: Any = if true { 1 } else { "a" };
let xs: [Any] = [1, "a"];
let m: %{String: Any} = %{"a": 1, "b": "two"};
print(b, xs, m["b"]);  // 1 [1,"a"] two
```

Write the `Any` where the list or map is made: a list made as a `[String]` can't be used as a `[Any]` later.

## Getting a value out of an `Any`

Anything can go into an `Any`, but to get a value back out you need a [cast](../features/casts.md). `cast::<Int>(value)` checks that the value is an Int, and `cast?::<Int>(value)` gives an `Option<Int>`:

```ndc,fails
let a: Any = 5;
let n: Int = a;  // error: mismatched types: found Any but expected Int
```

```ndc
let a: Any = 5;
let n: Int = cast::<Int>(a);
print(n + 1);           // 6
print(cast?::<String>(a));    // None
```

The same goes for operators and functions: `a + 1` needs `cast::<Int>(a) + 1`. Functions that take any value, like `print`, `string` and `==`, take an `Any` as it is.

Indexing is the exception: `a[i]` works on an `Any` and gives an `Any`, so you can read nested data and cast what you find:

```ndc
let data: %{String: Any} = %{"scores": [3, 5]};
print(cast::<Int>(data["scores"][1]) + 1);  // 6
```
