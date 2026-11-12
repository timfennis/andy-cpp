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
