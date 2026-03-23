# Method call syntax

{{#include ../snippets/method-call-syntax.md }}

## Examples

```ndc
let l = [50, 40, 20, 40, 10];

// A plain function-call version
let x = reduce(map(sorted(l), fn(x) => x + 5), fn(a, b) => a * b);

// The same code with method call syntax
let y = l.sorted
         .map(fn(x) => x + 5)
         .reduce(fn(a, b) => a * b);
```
