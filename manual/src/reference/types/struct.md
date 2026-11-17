# Struct

Structs are user-defined record types: a named collection of typed fields.

```ndc
struct Point {
    x: Int,
    y: Int,
}

let p = Point(1, 2);
print(p.x);  // 1

p.y = 20;
print(p);    // Point {x: 1, y: 20}
```

## Declaring a struct

A declaration names the struct and lists its fields. Every field requires a type
annotation, and a trailing comma after the last field is allowed. A struct may
have zero fields.

```ndc
struct Person {
    name: String,
    age: Int,
    locations: [String],
}

struct Marker { }

print(Person("Ada", 36, ["London"]));  // Person {name: "Ada", age: 36, locations: ["London"]}
print(Marker());                       // Marker {}
```

Struct names are scoped like variables: a struct declared inside a function or
block can only be used there. Declaring two structs with the same name in the
same scope is an error:

```ndc,fails
struct Point { x: Int }
struct Point { y: Int } // ERROR: Illegal redefinition of struct 'Point'
```

## Constructing instances

Declaring a struct binds a constructor function with the same name. It takes the
field values positionally, in declaration order.

```ndc
struct Point { x: Int, y: Int }

let p = Point(1, 2);
print(p);  // Point {x: 1, y: 2}
```

Passing the wrong number of arguments, or arguments of the wrong type, is an
error before the program runs:

```ndc,fails
struct Point { x: Int, y: Int }
Point("x", 2); // ERROR: no 'Point' matches the arguments 'String, Int'
```

## Field access

`p.x` reads the field `x` from `p`. Declaring a struct also declares a function
for each field, and `p.x` is the [method call](../../features/method-call-syntax.md)
`x(p)`, so these are all the same:

```ndc
struct Point { x: Int, y: Int }
let p = Point(1, 2);

print(p.x);    // 1
print(x(p));   // 1
print(p.x());  // 1
```

Because these are ordinary functions, you can pass them to other functions:

```ndc
struct Point { x: Int, y: Int }
let points = [Point(1, 10), Point(2, 20)];

print(points.map(x));                // [1,2]
print(points.map(fn (p) => p.x));    // [1,2]

// The constructor is a function value too.
struct Wrap { v: Int }
print([1, 2, 3].map(Wrap));          // [Wrap {v: 1},Wrap {v: 2},Wrap {v: 3}]
```

Two structs can have a field with the same name:

```ndc
struct Foo { size: Int }
struct Bar { size: Int }

print(Foo(1).size);   // 1
print(Bar(10).size);  // 10
```

To call a function stored in a field, put parentheses around the field:
`(s.f)()` reads the field and calls what it holds, while `s.f()` is the method
call `f(s)`.

```ndc
struct Callback { f: fn(Int) -> Int }
let cb = Callback(fn (a) => a * 2);

print((cb.f)(21));  // 42
```

```ndc,fails
struct Callback { f: fn(Int) -> Int }
let cb = Callback(fn (a) => a * 2);

cb.f(21);    // ERROR: this is method-call syntax for `f(cb, 21)`
```

## Field assignment

`p.x = value` writes to a field. The value must fit the field's declared type:

```ndc,fails
struct Point { x: Int }
let p = Point(1);

p.x = "ten";   // ERROR: mismatched types: found String but expected Int
```

[Augmented assignment](../../features/augmented-assignment.md) works on fields too:

```ndc
struct Counter { hits: Int }
let c = Counter(1);

c.hits += 4;
print(c.hits);  // 5
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

print(a.x);  // 99
```

Use `clone` for an independent instance (nested containers are still shared,
like cloning a list of lists) or `deepcopy` to duplicate nested mutable state
as well:

```ndc
struct Point { x: Int, y: Int }
let a = Point(99, 2);

let c = clone(a);
c.x = 1;
print(a.x);  // 99
print(c.x);  // 1
```

## Equality and hashing

Instances of the same struct are equal when their fields are equal. Instances
of different structs are never equal, even when their fields match.

```ndc
struct Foo { v: Int }
struct Bar { v: Int }

print(Foo(1) == Foo(1));  // true
print(Foo(1) == Foo(2));  // false
print(Foo(1) == Bar(1));  // false
```

Instances are hashable, so they work as map keys and set members:

```ndc
struct Point { x: Int, y: Int }

let visited = %{Point(0, 0): true};
print(visited[Point(0, 0)]);  // true
```

## Structs and JSON

`json_encode` rejects structs, because the struct type would be lost: a JSON
object decodes back to a map, not a struct. Use `json_encode_lossy` to encode an
instance as a JSON object with the field names as keys.

```ndc,fails
struct Point { x: Int, y: Int }

print(json_encode_lossy(Point(1, 2)));  // {"x":1,"y":2}
json_encode(Point(1, 2));                // ERROR: cannot convert a struct to JSON
```

## Current limitations

- Constructors are positional only; there is no named-field or default-value
  syntax.
- Structs do not take generic parameters: `Point<Int>` is an error.
- A struct can't refer to itself in its own fields, so
  `struct Node { next: Option<Node> }` is an error.
