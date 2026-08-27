# Struct

Structs are user-defined record types: a named collection of typed fields.

```ndc
struct Point {
    x: Int,
    y: Int,
}

let p = Point(1, 2);
assert_eq(p.x, 1);

p.y = 20;
assert_eq(p, Point(1, 20));
```

## Declaring a struct

A declaration names the struct and lists its fields. Every field requires a type
annotation, and a trailing comma after the last field is allowed. A struct may
have zero fields.

```ndc
struct Person {
    name: String,
    age: Int,
    locations: List<String>,
}

struct Marker { }
```

Declare a struct on its own line, like a `let` declaration — at the top level of
a program, inside a block, or in the REPL. A struct cannot be declared in the
middle of another expression, so `let s = struct P { x: Int }` is an error.

Struct names are lexically scoped, like variables. Declaring two structs with
the same name in the same scope is an error, but the name can be reused in
scopes that never coexist, and a declaration in an inner scope shadows a
same-named struct from an outer scope.

```ndc
struct Point { x: Int }
struct Point { y: Int } // ERROR: Illegal redefinition of struct 'Point'
```

A struct declared inside a function or block goes out of scope with it: the
type name, the constructor, and the field accessors are all unavailable
outside.

A struct cannot take the name of a built-in type:

```ndc
struct Int { x: Float } // ERROR: Struct 'Int' is not allowed to shadow the built-in type 'Int'
```

A struct can only be used after its declaration, and the name becomes usable as a
[type annotation](../types.md) in the scopes where the struct is visible.

## Constructing instances

Declaring a struct binds a constructor function with the same name. It takes the
field values positionally, in declaration order.

```ndc
struct Point { x: Int, y: Int }

let p = Point(1, 2);
```

Constructor calls are checked before the program runs: passing the wrong number of
arguments or incompatible types is a compile-time error.

```ndc
Point(1);      // ERROR: no 'Point' matches the arguments 'Int'
Point("x", 2); // ERROR: no 'Point' matches the arguments 'String, Int'
```

## Field access

`p.x` reads the field `x` from `p`. This is not special syntax for structs:
declaring a struct binds an ordinary getter function per field, and `p.x` is
exactly the call `x(p)`. [Method call syntax](../../features/method-call-syntax.md)
works too, so `p.x()` is the same call again.

```ndc
struct Point { x: Int, y: Int }
let p = Point(1, 2);

assert_eq(p.x, 1);
assert_eq(x(p), 1);
assert_eq(p.x(), 1);
```

Because getters are ordinary function values you can pass them to higher-order
functions:

```ndc
let points = [Point(1, 10), Point(2, 20)];

assert_eq(points.map(x), [1, 2]);
assert_eq(points.map(fn (p) => p.x), points.map(x));

// The constructor is a function value too.
struct Wrap { v: Int }
assert_eq([1, 2, 3].map(Wrap), [Wrap(1), Wrap(2), Wrap(3)]);
```

Accessors are resolved by overloading, so two structs can share a field name
without interfering:

```ndc
struct Foo { size: Int }
struct Bar { size: Int }

assert_eq(Foo(1).size, 1);
assert_eq(Bar(10).size, 10);
```

Because `s.f()` is method-call syntax, calling a *function stored in a field*
needs parentheses around the member access: `(s.f)()` first evaluates `s.f`
(the getter) and then calls its result.

```ndc
struct Callback { f: Any }
let cb = Callback(fn (a) => a * 2);

cb.f(21);    // ERROR: this is method-call syntax for `f(cb, 21)`
(cb.f)(21)   // 42: reads the field, then calls the stored function
```

## Field assignment

`p.x = value` writes to a field. The value must fit the field's declared type:

```ndc
struct Point { x: Int }
let p = Point(1);

p.x = 10;      // fine
p.x = "ten";   // ERROR: mismatched types: found String but expected Int
```

[Augmented assignment](../../features/augmented-assignment.md) works on fields
and evaluates the receiver expression exactly once:

```ndc
struct Counter { hits: Int }
let c = Counter(1);

c.hits += 4;
assert_eq(c.hits, 5);
```

A field is a typed location, so an augmented assignment whose result would not
fit the field type is rejected:

```ndc
c.hits += 0.5; // ERROR: mismatched types: found Float but expected Int
```

## Reference semantics

Struct instances are passed by reference, like lists and maps (see
[Memory Management](../memory-management.md)). Assigning an instance to another
variable aliases it rather than copying it:

```ndc
struct Point { x: Int, y: Int }

let a = Point(1, 2);
let b = a;
b.x = 99;

assert_eq(a.x, 99);
```

Use `clone` for an independent instance (nested containers are still shared,
like cloning a list of lists) or `deepcopy` to duplicate nested mutable state
as well:

```ndc
let c = clone(a);
c.x = 1;
assert_eq(a.x, 99);
assert_eq(c.x, 1);
```

## Equality and hashing

Typing is nominal: instances of the same struct compare field by field, and
instances of different structs are never equal, even when the fields match.

```ndc
struct Foo { v: Int }
struct Bar { v: Int }

assert_eq(Foo(1) == Foo(1), true);
assert_eq(Foo(1) == Foo(2), false);
assert_eq(Foo(1) == Bar(1), false);
```

Instances are hashable, so they work as map keys and set members:

```ndc
struct Point { x: Int, y: Int }

let visited = %{Point(0, 0): true};
assert_eq(visited[Point(0, 0)], true);
```

## Structs and JSON

`json_encode` rejects structs, because the struct type would be lost: a JSON
object decodes back to a map, not a struct. Use `json_encode_lossy` to encode an
instance as a JSON object with the field names as keys.

```ndc
struct Point { x: Int, y: Int }

json_encode_lossy(Point(1, 2)); // "{\"x\":1,\"y\":2}"
json_encode(Point(1, 2));       // ERROR: cannot convert a struct to JSON
```

## Current limitations

- Constructors are positional only; there is no named-field or default-value
  syntax.
- Structs do not take generic parameters: `Point<Int>` is an error.
- A struct cannot reference itself in its own field types.
  `struct Node { next: Option<Node> }` fails with `unknown type`, because the
  name is only registered after its field annotations are resolved.
