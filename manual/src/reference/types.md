# Types

Values in Andy C++ carry their types at runtime, but you rarely have to write one down: the analyser works out the type of every expression before the program runs, and rejects code that can't work. Annotations on variables, parameters and return values are optional.

## What the analyser works out

Functions declared with `fn` are generic without saying so. Here the analyser works out that `compose` takes a function from some `B` to some `C` and a function from that `C` onwards:

```ndc
fn compose(f, g) => fn(x) => g(f(x));
let shout = compose(to_upper, fn(s) => s ++ "!");
let half = compose(len, fn(n) => n / 2.0);
print(shout("hi"), half("four"));  // HI! 2.0
```

So it knows `len` and `to_upper` don't compose, without running anything:

```ndc,fails
fn compose(f, g) => fn(x) => g(f(x));
compose(len, to_upper);
// error: No function called 'compose' found that matches the arguments 'Fn(Iterable<Any>) -> Int, Fn(String) -> String'
//   `compose` takes (Fn(B) -> C, Fn(C) -> A)
```

A mistake is found even in code that never runs, and the error leads you through the calls to the line that can't work. This program prints nothing:

```ndc,fails
fn inc(x) => x + 1;
fn shout(s) => inc(s) ++ "!";
print("hello");
if false {
    shout("a");  // error: No function called '+' found that matches the arguments 'String, Int'
}                //   `shout` calls `inc` here, and `inc` needs `x + 1`
```

A lambda knows what it's given:

```ndc,fails
struct Player { name: String, score: Int }
let players = [Player("ann", 3), Player("bob", 5)];
players.map(fn(p) => p.score ++ " points");
// error: No function called '++' found that matches the arguments 'Int, String'
```

An empty list learns what it holds from how it's used:

```ndc,fails
let xs = [];
xs.push(1);
xs.push("two");  // error: No function called 'push' found that matches the arguments 'List<Int>, String'
```

And the types pick which overload of a function is meant. A generic function is compiled once for each set of types it's called with, so `signum` below is the `Int` version in one call and the `Float` version in the other:

```ndc
print(["a b", "c d"].map(split));  // [["a","b"],["c","d"]]

fn signs(xs) => xs.map(signum);
print(signs([3, -2]), signs([0.5, -1.5]));  // [1,-1] [1.0,-1.0]
```

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
let maybe: Option<String> = Some("hi");
let pair: Tuple<Int, String> = (1, "hi");
let pair2: (Int, String) = (1, "hi");  // tuple shorthand

print(maybe, pair2);  // Some("hi") (1,"hi")
```

Nested generics work too — the parser handles the `>>` ambiguity for you:

```ndc
let grid: List<List<Int>> = [[1, 2], [3, 4]];
print(grid);  // [[1,2],[3,4]]
```

### Any kind of sequence

A parameter without an annotation takes any kind of sequence its body can iterate. The function is compiled once for each kind it's called with:

```ndc
fn total(xs) {
    let sum = 0;
    for x in xs {
        sum += x;
    }
    sum
}
print(total([1, 2]), total((3, 4)), total(1..4));   // 3 7 6
```

### Element types are exact

A `List<Int>` is not a `List<Any>`: a `List<Any>` could be given a String, which the list of Ints would then hold. The same goes for maps, tuples, options, the other containers, and function types. To change a container's element type, go through `Any`:

```ndc,fails
let words = "a b".split(" ");     // List<String>
let any: Any = words;
let anything: List<Any> = any;    // fine: checked as the program runs
let wrong: List<Any> = words;     // error: found List<String> but expected List<Any>
```

[Struct](./types/struct.md) names are type names like any other, including inside generics:

```ndc
struct Point { x: Int, y: Int }

fn manhattan(p: Point) => p.x + p.y
let points: List<Point> = [Point(1, 2), Point(3, 4)];
print(points.map(manhattan));  // [3,7]
```

A struct name refers to the struct declared earlier in an enclosing scope; it cannot be
used before (or inside) its own declaration, and it goes out of scope together with the
block or function that declared it. See [Struct](./types/struct.md) for the scoping
rules.

> **Note:** every value fits `Any`, so an `Any`-annotated binding will accept anything. When a value has no annotation and the analyser can't infer a type, it falls back to `Any`. A parameter of a function declared in the program gets the type of each call's argument instead (see [Generic functions](./types/function.md#generic-functions)). There is also a `Never` type used internally for things like `break` that don't produce a value — you'll rarely need to write it by hand.

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
