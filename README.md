# Andy C++

Andy C++ is a programming language built primarily for solving [Advent of Code](https://adventofcode.com/) puzzles and quick one-off scripts. It's syntax and semantics are designed to feel familiar to those who know Rust, while offering some of the high-level flexibility from Python (without all of its pitfalls).     

Since version 0.3.0 the language uses a custom byte-code VM based on the Crafting Interpreters book. Code is: lexed, parsed, analysed, compiled and executed in separate steps. The project ships as a single binary that contains everything including a REPL, LSP and basic profiler. 

**Features:**
* Arbitrary-precision arithmetic (including rational numbers)
* A dynamic type system that tries to rescue you with static type checks ahead of compilation
* Higher order functions and closures
* `a.map(b)` and `map(a,b)` are [exactly the same](https://timfennis.github.io/andy-cpp/features/method-call-syntax.html)
* Functions that take two arguments can be used in [augmented assignment](https://timfennis.github.io/andy-cpp/features/augmented-assignment.html): `l map= fn(x) => x + 3`
* Marking a function `pure` enables [memoization](https://timfennis.github.io/andy-cpp/features/memoization.html) (but only through hashing, no equality checks; use at your own risk)
* Built in support for default dictionaries, MinHeap, MaxHeap and Deque
* A pretty rich but work in progress standard library

## Getting Started

The best way to try this project is to build it from source using the rust toolchain. There are binary releases but those are symbollic milestones, and contain bugs.

### Prerequisites

You need a working [Rust toolchain](https://rustup.rs/).

### Install

```bash
cargo install --git https://github.com/timfennis/andy-cpp
```

This installs the `ndc` binary. You can then run a script:

```bash
ndc script.ndc
```

Or start the interactive REPL:

```bash
ndc
```

To browse the built-in function documentation:

```bash
ndc docs
ndc docs map  # filter by the keyword 'map'
```

For the language manual, see <https://timfennis.github.io/andy-cpp/>.

### Editor support

Installation and configuration instructions for VS Code, JetBrains IDEs, Neovim,
Helix, and other LSP-capable editors are available on the
[editor support page](https://timfennis.github.io/andy-cpp/tooling/editor-support.html)
in the manual.

## Examples

Many examples of the language can be found in [this](https://github.com/timfennis/advent-of-code-ndc) repository.

## Thanks

This language and implementation was inspired by Robert Nystrom's
book [Crafting Interpreters](https://craftinginterpreters.com/). I've also taken plagiaristic levels of inspiration
from [Noulith](https://github.com/betaveros/noulith) which is the language that inspired me to read the book in the
first place.

## LLM Disclosure

This project has had various levels of LLM involvement during its lifetime. The codebase is designed by humans and is meant to be read and maintained primarily by humans. Large language models are tools and, like all other tools, have strengths and limitations. They are allowed in this project when used responsibly. All contributions will be judged on their merits.
