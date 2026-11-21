# Performance

Most programs run fast without any thought about speed. This page collects the
few habits that make a real difference when they don't.

## Keep the arguments of a pure function small

A [pure function](./memoization.md) remembers its results by its arguments.
To find an earlier result, every call looks at everything in its arguments, so
passing a big list or map to a pure function costs time on every call, even
when the result is already known.

If a big value stays the same between calls, such as a grid or a graph, don't
pass it in. Use it from outside the function, so that only the small arguments
that change are looked at:

```ndc
let graph = %{"a": ["b", "c"], "b": ["d"], "c": ["b", "d"], "d": []};

pure fn paths(node) {
    if node == "d" {
        return 1;
    }
    [paths(next) for next in graph[node]].sum()
}

print(paths("a"));  // 3
```

Written as `pure fn paths(graph, node)`, the same program looks through the
whole graph on every call. With a graph of a few hundred nodes, that made one
solution ten times slower.

A pure function that uses a value from outside keeps the results it remembered,
so only do this with a value that no longer changes once the function is
called.
