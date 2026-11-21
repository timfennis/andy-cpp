# Lazy

Functions like `map` and `filter` make a new list straight away:

```ndc
let squares = [1, 2, 3, 4].map(fn(x) => x * x);
print(squares);  // [1,4,9,16]
```

A lazy sequence makes its elements only when something reads them. Write `.lazy` to get one, and the same functions add a step to it instead of making a list:

```ndc
let squares = [1, 2, 3, 4].lazy.map(fn(x) => x * x);
print(squares);       // lazy([1,2,3,4]).map(<fn ?>)
print(squares.list);  // [1,4,9,16]
```

Printing a lazy sequence shows what it will do, not its elements, as making them may call your functions.

## Stopping early

A lazy sequence is useful when you don't need every element. Here `find` stops at the first square over 5, so `map` runs only three times:

```ndc
let checked = [];
let first = [1, 2, 3, 4, 5]
    .lazy
    .map(fn(x) { checked.push(x); x * x })
    .find(fn(x) => x > 5);
print(first, checked);  // 9 [1,2,3]
```

## Ranges without an end

A range without an end, `1..`, never ends, so `map` and `filter` give a lazy sequence for one without `.lazy`. Take as many elements as you need:

```ndc
let multiples_of_7 = (1..).map(fn(x) => x * 7).take(4);
print(multiples_of_7.list);  // [7,14,21,28]
```

## Reading it again runs it again

A lazy sequence doesn't keep its elements. Every `for` loop, and every function that reads it, makes them again:

```ndc
let loud = [1, 2].lazy.map(fn(x) { print("making", x); x });
print(loud.sum());
print(loud.sum());
// making 1
// making 2
// 3
// making 1
// making 2
// 3
```

If you need the elements more than once, make them into a list with `list`, and use the list.

## Where it goes

A `for` loop, a comprehension and these functions read a lazy sequence: `list`, `set`, `sorted`, `sum`, `product`, `min`, `max`, `join`, `count`, `find`, `any`, `all`, `none`, `first_or`, `fold`, `reduce` and `in`. These add a step to one: `map`, `filter`, `flat_map`, `enumerate`, `take`, `skip`, `take_while` and `skip_while`.

Other functions that take a list don't take a lazy sequence. Make it into a list first:

```ndc
let pairs = list([1, 2, 3].lazy.map(fn(x) => x * 10)).pairwise();
print(pairs);  // [(10,20),(20,30)]
```
