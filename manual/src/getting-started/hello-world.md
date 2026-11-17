# Hello, World!

Now that the Andy C++ interpreter is installed we can write our first program.

First create a file called `hello.ndc` add the code in Listing 1-1.

```ndc
print("Hello, world!");
```

In Andy C++ we don't need a main function. Semicolons are mandatory, just like they are in Rust. If you already know Rust, Andy C++ will feel very familiar.

```bash
$ ndc hello.ndc
Hello, world!
```

You can also pass a program on the command line with `--eval` (or `-e`), without
creating a file. Errors in it are reported against `<eval>`:

```bash
$ ndc run --eval 'print("Hello, world!");'
Hello, world!
```

## Reading input

`read_stdin()` reads everything on standard input and returns it as a string.
Split it with `lines`, `words` or `paragraphs`. Save this as `sum.ndc`:

```ndc,norun
let total = 0;
for line in read_stdin().lines {
    total += int(line);
}
print(total);
```

Then give it a file to read with `<`:

```bash
$ printf '1\n2\n3\n' > numbers.txt
$ ndc sum.ndc < numbers.txt
6
```

If you leave out `< numbers.txt`, the program reads what you type instead, until you
press Ctrl-D.
