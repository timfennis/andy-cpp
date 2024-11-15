In Andy C++, functions can be called using a *method syntax* on their first argument. This provides a cleaner,
object-oriented feel without needing to define member functions.

```ndc
fn add(x, y) {
  return x + y;
}

// Normal function call
print(add(3, 5));

// Method-like syntax
print(3.add(5));
```

Both styles are equivalent, allowing you to choose the one that suits your code best. While such flexibility might feel
messy in traditional languages, in Andy C++ (a language designed around ease of typing) switching between these
syntaxes is especially helpful for writing concise and readable code.
