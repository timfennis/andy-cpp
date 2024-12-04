# Method call syntax

{{#include ../snippets/method-call-syntax.md }}

## Examples

```ndc
let l = [50, 40, 20, 40, 10];

// Usually you might write something like this
let x = reduce(map(sorted(l), fn(x) => x + 5), fn(a, b) => a * b);

// It's much easier to read and write this type of code using method call syntax
let y = l.sorted
         .map(fn(x) => x + 5)
         .reduce(fn(a, b) => a * b);
```
