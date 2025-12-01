# Deque

A double-ended queue backed by Rust's [VecDeque](https://doc.rust-lang.org/std/collections/struct.VecDeque.html).

This is very useful for implementing algorithms like BFS.

```ndc

let queue = Deque();
queue.push_back((0, 0));

while not queue.is_empty() {
    let cur = queue.pop_front();
    
    // etc.
}
```