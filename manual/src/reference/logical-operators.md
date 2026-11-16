# Logical operators

Andy C++ uses the keywords `and`, `or`, and `not` for logical operators. If you are coming from Rust, write `and` and `or` instead of `&&` and `||`.

```ndc
let has_input, failed, timed_out, disconnected = true, false, false, true;

let ready = has_input and not failed;
let retry = timed_out or disconnected;
print(ready, retry);  // true true
```

## Short-circuiting

`and` and `or` are lazy. Andy C++ evaluates the right-hand side only when it needs that value to decide the result.

```ndc
let x = 0;

true and { x = x + 1; false };
false and { x = x + 1; false };
true or { x = x + 1; false };
false or { x = x + 1; false };

print(x);  // 2
```

`false and ...` stops at `false`, and `true or ...` stops at `true`.

Both sides are Bools. There's no truthiness: a number, a list or an `Option` in their place is an error, as it is in the condition of an `if` or a `while`. An `Any` takes a cast first, as in `value as Bool`.

```ndc,fails
let count = 3;
if count { print("some"); }
// error[analysis]: mismatched types: found Int but expected Bool
```

## Precedence

`and` binds tighter than `or`.

```ndc
print(true or true and false);  // true
print(false and true or true);  // true
```

That means Andy C++ reads those expressions as:

```ndc
print(true or (true and false));  // true
print((false and true) or true);  // true
```

## Bitwise operators

Boolean values also support the non-lazy operators `&`, `|`, and `~`.

Use `and` and `or` when you want short-circuiting. Use `&` and `|` when you need both sides to run.
