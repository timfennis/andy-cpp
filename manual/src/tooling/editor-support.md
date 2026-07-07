# Editor support

Andy C++ ships a language server (LSP) so editors can offer rich feedback as you
write `.ndc` files. The server is built into the `ndc` binary and is started with:

```bash
ndc lsp --stdio
```

Most users don't run this by hand — the [VS Code extension](https://open-vsx.org/)
launches it automatically. Any LSP-capable editor can use it by pointing at the
`ndc lsp --stdio` command for the `andy-cpp` language and the `.ndc` file extension.

## What the language server provides

- **Diagnostics** — lexer, parser, and semantic/type errors are reported inline as
  you type.
- **Inlay type hints** — inferred types are shown after `let` bindings and function
  parameters, and inferred return types after function signatures. Hints are only
  shown where you didn't already write an annotation.
- **Hover** — hovering an expression shows its inferred type; hovering a built-in
  function shows its signature and documentation.
- **Completion** — typing `.` offers functions whose first parameter accepts the
  receiver's type (method-call style). General completion offers built-in functions,
  in-scope variables, and language keywords.
- **Document symbols** — an outline of the top-level and nested functions and
  variable declarations in the file.
- **Go-to-definition** — jump from a variable or function usage to its declaration.

## JetBrains IDEs (RustRover, IntelliJ, …)

JetBrains IDEs are supported without a dedicated plugin, in two independent parts.

**Syntax highlighting** — the bundled *TextMate Bundles* plugin can import the VS Code
extension directory directly:

1. Go to *Settings → Editor → TextMate Bundles*, click **+**, and select the
   `ext/andy-cpp` directory from the repository.
2. Open a `.ndc` file — it should highlight immediately. If it renders as plain text,
   check *Settings → Editor → File Types* and make sure `*.ndc` is not claimed by
   another file type.

**Language intelligence** — the [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij)
plugin connects the IDE to the language server:

1. Build the binary with `cargo build --release`, then install **LSP4IJ** from the
   plugin marketplace.
2. Open the *Language Servers* tool window, click **+**, and configure a new server:
   - **Command**: `/path/to/andy-cpp/target/release/ndc lsp --stdio`
   - In the **Mappings** tab, add a *File name patterns* mapping with pattern `*.ndc`
     and language id `ndc`.
3. Open a `.ndc` file. The server starts automatically and provides everything listed
   above. `ext/lsp4ij-ndc/template.json` in the repository contains the same
   configuration as a reference.

## Notes

- The server uses full-document synchronisation and re-analyses on each edit.
- While the buffer is mid-edit and doesn't parse, the last successful analysis is
  retained so hints and dot-completion keep working.
