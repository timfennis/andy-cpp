# Map and Set

Andy C++ does not have separate map and set types. A set is just a map where all the values are unit `()`.

## Set

```ndc
let my_set = %{5, 3, 2, 1};
```

## Map

```ndc
let my_map = %{
  "apples": 60,
  "oranges": 22,
  "bananas": 0,
};
```

## Default values

```ndc
let defaultdict = %{:0};

// All nonexisting elements in defaultdict default to 0
assert(defaultdict[10] == 0);

// You can also mutate elements that don't exist in this way
defaultdict[33] += 7; // adds 7 to 0 and associates it to key 33
```

>
> will use the same list every time.
