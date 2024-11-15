# Augmented assignment

In Andy C++ you can augment assignment operators to quickly perform operations on existing variables like you might expect from any language.

In this example we're augmenting the assignment operator with another operator to quickly increment a number.

```
let my_number = 3;
my_number += 5;

assert_eq(my_number, 8);
```

## Optimization

You might expect `list ++= [1,2,3]` desugar to something like `list = list ++ [1,2,3]` but this would be needlessly inefficient. This is why
languages like Andy C++ have special handling for some of these assignment operators. For instance the earlier example simply appends `[1,2,3]` to
list without creating an intermediary list first.

## Flexibility

Note: I stole this feature from [Noulith](https://github.com/betaveros/noulith).

Augmented assignment is not limited to built in operators, you can also use built in function or user created functions to agument assignment. Consider the following example:

```ndc
let x = 3;
let f = fn (a, b) { a + b }; // simple addition
x f= 5; // similar to: x = f(x, 5);
assert_eq(x, 8);
```

A common situation where you might want to use is when finding the highest or lowest value in an iteration:

```ndc
let lowest, highest = Inf, -Inf;

for x in 1..100 {
  lowest min= g(x);
  highest max= g(x);
}
```
