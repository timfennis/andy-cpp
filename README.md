# Andy C++

This is the source code for the Andy C++ programming language. Not meant for anything serious.

## Example

Currently, the language only has some very basic features. I'll try to update the sample below as I make more progress.

```ndc
x := 3;
y := 5;

// introduce a scope that introduces a temporary variable and evaluates to a value
x = { 
    val := 1;
    x + val
};

z := x * y;
print z; // 20

print "Hello" + (2 * " hello") + ", World!"; // Hello hello hello, World!
```

## Thanks

This language and implementation was inspired by Robert Nystrom's book [Crafting Interpreters](https://craftinginterpreters.com/).
