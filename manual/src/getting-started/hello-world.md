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
