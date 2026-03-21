# MinHeap & MaxHeap

A data structure backed by Rust's [BinaryHeap](https://doc.rust-lang.org/std/collections/struct.BinaryHeap.html) that keeps elements in sorted order. This is very useful when implementing algorithms like Dijkstra and A*.

> **Note:** comparisons between types like `Int` and `String` are undefined and the Heap will treat them as equal. If you like well-defined behavior DO NOT MIX THEM.