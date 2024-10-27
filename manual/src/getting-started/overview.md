# Overview

```ndc
// Andy C++ has a dynamic type system that automatically converts different types of
// numbers without losing precision

n := 5 / 3; // Division produces a rational number, no funny bussiness with floats
assert_eq(n * 3, 5); // No funny bussiness this is always true

// You can create floats using a function with the same name
n := float(5 / 3); // 1.6666666666666667

// Andy C++ will generally never produce floats unless one of the parameters to an
// opration is a float
x := 3 * 5.0; // 15.0 float!

// The exception is raising to the power of a rational number, which seems like a
// fair compromise
assert_eq!(5^(1/2), 2.23606797749979);

// Usually Andy C++ will try to use 64-bit signed integers under the hood, but
// using the power of the num crate we can also create some really big numers
n := 2 ^ 1000; // This is a very big number

// Andy C++ also supports complex numbers
n := 5.3 + 3.2j;

// Some different number literals
x := 0b010101; // binary
x := 0o123; // octal
x := 0xFF; // hexadecimal
x := 36rFOOBARBAZ; // arbitrary bases up to base36 are allowed

// Andy C++ has quite a few special operators
// the <> operator coerces it's operands into strings and concatenates them
x := "Foo: " <> 3;

// the ++ operator concatenates two lists
x := [1,2,3] ++ [4,5,6];

// ^ is exponentiation and ~ is bitwise XOR
x := 3 ^ 5; // exponentiation
x := 5 ~ 3; // bitwise xor
x := 5 & 3; // bitwise and
x := 5 | 3; // bitwise or
x := ~5; // bitwise complement

// Logical operators have lazy evaluation and use words like they do in python
x := x == 5 and y == 3;
y := z != 4 or x > 2;
z := not x == 3;

// The not keyword can be combined with the in operator to perform handy checks
if 4 not in [1,2,3] {
  // item is not in list
} else if 6 in some_list {
  // 6 is in some_list
}

// Declare functions, any parameters always default to the `Any` type.
fn add(a, b) {
  return a + b;
}

// Just like in rust the return keyword can be omitted
fn mul(a, b) {
  a * b
}

// All functions can be called as if they are methods of their first argument
two := add(1, 1); // Regular call
three := 2.add(1); // Calls the same function as if it's a method

// Lists are mutable datastructures
l := [1,2,3];
l.push(4); // [1,2,3,4]
l.pop() == 4; // [1,2,3]

// Tuples are like lists but are immutable
t := (1,2,3);
t.push(); // no function called 'push' found matches the arguments: (tuple, int)

// Maps have special syntax (that looks like JSON) similar to Elixir
m := %{"key": "value", 3: 8};
m["test"] = 7; // insert into map
m.remove("key"); // removes the key "key"

// Maps can have a default value like defaultdict in python with this special syntax
x := %{:0};
x[(18, 3)] == 0;

// Sets aren't a separate thing but they are just maps with the values set to unit `()`
m := %{"foo": (), "bar": ()}; // Map
s := %{"foo", "bar"}; // Set
assert_eq(m, s); // This succeeds!

// Destructuring is available in many forms and mostly follows the way python does it.
x, y := 5, 3;

// Using tuple syntax is optional
(x, y, z) := (1, 2, 3);

// Nested tuples can be broken down
(key, (x, y)) := ("foo", (3, 6));

// Destructuring ignores the container type so tuples can be assigned to lists and vice
// versa as long as the structure matches
(key, [x, y]) := ["bar", (6, 3)];

// If statements work almost exactly like they do in Rust
if x == y {
  do_something();
} else if y == z {
  do_something_else();
} else {
  error("failed");
}

// If statements are actually expressions
v := if x > y { x } else { y };

// A normal foreach loop
l := [5,4,3,2,1];
for x in l {
  print(x);
}

// foreach loops can have multiple iterations
for x in 3..10, y in 6..66 {
  print(x * y);
}

// foreach loops can also have guards
for x in 1..=10, y in 1..=10, if x != y, if y != 8 {
  print(x / y);
}

// Andy C++ also supports for-comprehensions with a similar syntax
ll := [x / y for x in 1..=10, y in 1..=10, if x != y];

// For comprehensions can also be used to build a map/dictionary
// The value here is optional you can also produce a set
grid := %{(x, y): true for x in 0..10, y in 0..10};

// Note that destructuring is also supported in these types of expressions
positions := [x, y for x in 0..10, y in 0..10];
for x, y in positions {
  print("X: " <> x <> ", Y: " <> y);
}
```
