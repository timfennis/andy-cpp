# Andy C++

This is the source code for the Andy C++ programming language. Not meant for anything serious.

## Example

Currently, the language only has a Lexer and isn't able to execute any expressions. This is what the language might look like in the future:

```andycpp
foo := 3 * (2 + 5);
print(foo, str(foo) + " wow so good");

while foo > true {
    // creates a new variable bar
    bar := if 3 > 2 { 6 } else { 5 };
    foo = bar;
}

fn do_something() {
    return "does nothing";
}
```

## Thanks

This language and implementation was inspired by Robert Nystrom's book [Crafting Interpreters](https://craftinginterpreters.com/).
