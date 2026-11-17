# Reading input

A program can read its input from a file with `read_file`, or from standard input with
`read_stdin`. Both return the whole input as one string, which you can then split with
`lines`, `words` or `paragraphs`.

## From a file

`read_file` takes a path and returns what the file holds:

```ndc,norun
let text = read_file("numbers.txt");
let total = 0;
for line in text.lines {
    total += int(line);
}
print(total);
```

A relative path is relative to the directory you run `ndc` from, not the directory the
script is in.

## From standard input

`read_stdin` reads everything on standard input. Save this as `sum.ndc`:

```ndc,norun
let total = 0;
for line in read_stdin().lines {
    total += int(line);
}
print(total);
```

Then give it a file to read with `<`, or pipe another program's output into it:

```bash
$ printf '1\n2\n3\n' > numbers.txt
$ ndc sum.ndc < numbers.txt
6
$ seq 10 | ndc sum.ndc
55
```

This keeps the file name out of the program, so the same script runs on any input.

If you leave out `< numbers.txt`, the program reads what you type instead, until you
press Ctrl-D (Ctrl-Z and Enter on Windows).

`read_stdin` reads all of the input at once, so call it once and keep the result:

```ndc,norun
let input = read_stdin();
let sections = input.trim().paragraphs;
```
