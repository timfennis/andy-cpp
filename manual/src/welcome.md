# Welcome

Welcome to the documentation for **Andy C++**!

This language is the result of a hobby project that I've poured a lot of time and effort into, purely for the fun of exploring new
ideas in programming. I was inspired to create this language by [Brian Chen](https://github.com/betaveros), who developed
[Noulith](https://github.com/betaveros/noulith) to participate in (and ultimately win) Advent of Code. I’d like to thank Brian
for the inspiration his work provided.


## Disclaimers
 * To enhance clarity and minimize errors in this documentation, I used AI assistance, hoping it would make the language’s concepts
   easier to understand. Thank you for checking out Andy C++—I hope you enjoy exploring it as much as I enjoyed creating it!
 * This is the first time I attempted to make a programming language, and it's one of my first Rust projects. You're welcome to
   look at the source or even send in contributions but don't expect it to be easy. I'm just an average guy bashing my head
   against problems until I stumble into a solution that seems to work. There will be a lot of really bad stuff under the hood.
 * I made this language for **fun**, and it's meant to be used for **fun**. Don't rely on the output of the interpreter to make
   important decissions. [It took me 4 months before I noticed that subtracting floats was actually performing addition.](https://github.com/timfennis/andy-cpp/commit/0d27629c8980d2d860c72f679053a57f0539bb0a#diff-15cd64cded3cdc9c61fac55eaac6ec14449add762cbadaf51745df463bcb658dR205)
 * The performance of the interpreter is **pretty bad**, but as long as you come up with smart solutions and offload some of the
   calculations to functions written in rust you should be fine.
