# MinHeap & MaxHeap

A data structure backed by Rust's [BinaryHeap](https://doc.rust-lang.org/std/collections/struct.BinaryHeap.html) that keeps elements in sorted order. This is very useful when implementing algorithms like Dijkstra and A*.

## Iterating

A heap is a sequence of its elements in the order it gives them: smallest first for a `MinHeap`, largest first for a `MaxHeap`. A `for` loop, `sum`, `map`, `sorted` and the other functions that take a sequence all see that order, and `len` counts the elements:

```ndc
let heap = MaxHeap();
for x in [2, 3, 1] {
    heap.push(x);
}
print(heap.list(), heap.len(), heap.pop());  // [3,2,1] 3 3
```
