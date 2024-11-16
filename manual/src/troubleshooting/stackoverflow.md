# Stackoverflow

Andy C++ is a straightforward tree-walk interpreter that, at present, does not support tail-call optimization.
As a result, recursive function calls can consume significant stack space, potentially leading to a stack
overflow for deeply recursive functions.

While future versions of Andy C++ may address this limitation, you can work around it by increasing the
available stack space for the interpreter. On Linux, you can do this by running the following command:

```bash
ulimit -s 65536
```

This should allow most programs with reasonable recursion depths to execute successfully.

|Stack size|Estimated recursion depth|
|---|---|
|8192 KiB|1108|
|65536 KiB|8883|
|262144 KiB|35542|
