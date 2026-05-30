# andy-cpp README

## Features

This extension provides editor support for the Andy C++ programming language:

- Syntax highlighting for `.ndc` files
- Language server features (via the bundled `ndc` binary):
  - Diagnostics (lexer, parser, and type errors)
  - Inlay type hints
  - Hover (inferred types; signatures and docs for built-ins)
  - Completion (method-call style on `.`, plus locals and keywords)
  - Document symbols (outline)
  - Go-to-definition
- "Run Script" command

## Known Issues

Not all the syntax of the language is highlighted correctly.
