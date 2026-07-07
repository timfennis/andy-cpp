# Andy C++ support for JetBrains IDEs (RustRover, IntelliJ, …)

Editor support for `.ndc` files in JetBrains IDEs comes in two independent parts:

1. **Syntax highlighting** via the bundled *TextMate Bundles* plugin, using the grammar in
   this directory.
2. **Language intelligence** (diagnostics, hover, completion, inlay type hints, go to
   definition, document symbols) via the [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij)
   plugin talking to the `ndc lsp` language server.

This directory is laid out as a VS Code-style extension (`package.json` +
`syntaxes/ndc.tmLanguage.json`), which the TextMate Bundles plugin can import directly.

## 1. Syntax highlighting (TextMate bundle)

1. Make sure the **TextMate Bundles** plugin is enabled
   (*Settings → Plugins → Installed* — it ships with the IDE).
2. Go to *Settings → Editor → TextMate Bundles*, click **+**, and select this directory
   (`ext/textmate-ndc`).
3. Open a `.ndc` file. It should highlight immediately.

If `.ndc` files render as plain text, check *Settings → Editor → File Types* and make sure
`*.ndc` is not claimed by another file type (it should be associated with
"Files supported via TextMate bundles").

## 2. Language server (LSP4IJ)

First build the `ndc` binary:

```bash
cargo build --release   # produces target/release/ndc
```

Then in the IDE:

1. Install the **LSP4IJ** plugin from the marketplace.
2. Open the *Language Servers* tool window (*View → Tool Windows → Language Servers*),
   click **+** to add a new language server, and pick the *New server* template.
3. Configure it:
   - **Name**: `Andy C++ (ndc)`
   - **Command**: `/path/to/andy-cpp/target/release/ndc lsp --stdio`
   - In the **Mappings** tab, add a *File name patterns* mapping with pattern `*.ndc`
     and language id `ndc`.
4. Open a `.ndc` file. The server starts automatically and provides diagnostics, hover,
   completion, inlay type hints, go to definition, and document symbols.

`ext/lsp4ij-ndc/template.json` contains the same configuration as an LSP4IJ template for
reference; adjust the command to the absolute path of your `ndc` binary if it is not on
the IDE's `PATH`.

## Keeping the grammar up to date

The grammar is written against the token definitions in `ndc_lexer/src/token.rs` and the
tree-sitter grammar in `ext/tree-sitter-andy-cpp/grammar.js`. When the language gains new
keywords, literals, or operators, update `syntaxes/ndc.tmLanguage.json` to match.
