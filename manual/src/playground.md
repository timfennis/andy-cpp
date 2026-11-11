# Playground

Write Andy C++ and run it right here: the interpreter is compiled to WebAssembly
and runs in your browser. Press <kbd>Ctrl</kbd>+<kbd>Enter</kbd> to run, or open
the [playground on a page of its own](playground/index.html).

<iframe class="ndc-playground" title="Andy C++ playground"></iframe>

The editor knows the language the way an editor with the language server does:

- Errors are underlined as you type.
- Completion offers the functions that fit: after `xs.` with `xs` a list, the
  ones that take a list. <kbd>Ctrl</kbd>+<kbd>Space</kbd> asks for it.
- Hovering over a name shows its type, or a function's signature and
  documentation.
- <kbd>F12</kbd>, or <kbd>Ctrl</kbd>+click, jumps to where a name is declared.
- Tick **Vim** for vim's keys; `:w` runs the program.

The examples in this manual can be run too: hover over one and press the ▶
button, or the button next to it to open the example in the playground.
