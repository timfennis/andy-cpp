# Formatter

`ndc fmt` formats Andy C++ code in place:

```bash
ndc fmt solution.ndc lib/*.ndc
```

Without files it reads standard input and writes the formatted code to standard
output, which is how editors usually call a formatter:

```bash
ndc fmt < solution.ndc
```

With `--check` it changes nothing, lists the files that aren't formatted and
fails if there are any, which suits a CI job:

```bash
ndc fmt --check *.ndc
```

## What it does

The formatter has no options. It changes the spaces and line breaks between
tokens and leaves everything else as you wrote it, so

```ndc,norun
let   xs =[1,2 ,3];
fn double(x) => { x*2 }
```

becomes

```ndc,norun
let xs = [1, 2, 3];
fn double(x) { x * 2 }
```

- Code is indented with four spaces, and lines are kept within 120 columns
  where breaking them allows.
- Something that doesn't fit on a line is broken: the items of a list or the
  arguments of a call each go on their own line, with a comma after the last
  one, and a long chain of operators breaks before each operator. A function
  passed as the last argument stays on the line of the call:

  ```ndc,norun
  let total = xs.fold(0, fn(sum, x) {
      sum + x * x
  });
  ```

- A block or map stays on one line if you wrote it on one line and it fits.
  One you wrote over several lines stays broken.
- At most one empty line is kept between statements.
- A comment on its own line stays on its own line, and a comment after code
  stays after that code, one space away.
- Parentheses, semicolons, number and string literals are kept as you wrote
  them. `fn f() => { ... }` becomes `fn f() { ... }`, which means the same.

The formatter checks its own work: if the formatted code would parse to a
different program, or would lose a comment, it reports an error and leaves the
file as it is. Code that doesn't parse isn't formatted either; `ndc fmt`
reports the syntax error instead.
