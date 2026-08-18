# Editor support

Andy C++ ships a language server (LSP) so editors can offer rich feedback as you
write `.ndc` files. If you have Andy C++ isntalled you automatically also have the language server.

You can start the language server like this:

```bash
ndc lsp --stdio
```

Most users do not run this command by hand. Editor integrations either launch it
automatically or are configured to run it for `.ndc` files.

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

## VS Code and compatible editors

The [Andy C++ extension on Open VSX](https://open-vsx.org/extension/TimFennis/andy-cpp)
provides syntax highlighting, all of the language-server features listed above, and
a **Run Script** command that executes the current file in the integrated terminal.

Install it from the Extensions view in editors that use the Open VSX registry. For
Microsoft VS Code, download the VSIX file from the Open VSX page and install it with
**Extensions: Install from VSIX...** in the command palette.

The extension launches `ndc lsp` automatically. If `ndc` is not on the `PATH` seen
by the editor, set `andy-cpp.ndcPath` to the full path of the binary.

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

## Neovim

Neovim has a built-in Tree-sitter runtime, so `nvim-treesitter` is not required.
The generated parser is committed to the Andy C++ repository and can be compiled
with a C compiler; installing it does not require Node.js or npm.

Clone the repository and run the installer:

```bash
git clone --depth 1 https://github.com/timfennis/andy-cpp.git
cd andy-cpp/ext/tree-sitter-andy-cpp
./install.sh neovim
```

The script supports Linux, the BSDs, and macOS. It installs the parser and queries
under `${XDG_CONFIG_HOME:-$HOME/.config}/nvim`. Set `CC` to select a different C
compiler.

Add the following to `init.lua`:

```lua
-- Treat .ndc files as the `andy_cpp` filetype.
vim.filetype.add({ extension = { ndc = "andy_cpp" } })

-- Start Tree-sitter highlighting for those buffers.
vim.api.nvim_create_autocmd("FileType", {
  pattern = "andy_cpp",
  callback = function(args)
    pcall(vim.treesitter.start, args.buf, "andy_cpp")
  end,
})

-- Language server (Neovim 0.11+).
vim.lsp.config("ndc_lsp", {
  cmd = { "ndc", "lsp", "--stdio" },
  filetypes = { "andy_cpp" },
  root_markers = { ".git" },
})
vim.lsp.enable("ndc_lsp")

-- Optional: show inlay hints once the server attaches.
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.name == "ndc_lsp" then
      pcall(vim.lsp.inlay_hint.enable, true, { bufnr = args.buf })
    end
  end,
})
```

Run `./install.sh neovim` again after updating the grammar or its queries, then
restart Neovim. After rebuilding `ndc`, reload the language server with
`:LspRestart`.

If `.ndc` is already mapped to a different filetype, omit `vim.filetype.add` and
register the parser for that filetype instead:

```lua
vim.treesitter.language.register("andy_cpp", "your_filetype")
```

Use the same filetype in the autocmd pattern and language-server configuration.

## Helix

Helix also has built-in Tree-sitter and LSP support. Add the following to
`~/.config/helix/languages.toml` (or the equivalent path below
`XDG_CONFIG_HOME`):

```toml
[[language]]
name = "andy-cpp"
scope = "source.andy-cpp"
file-types = ["ndc"]
comment-tokens = ["//"]
indent = { tab-width = 4, unit = "    " }
language-servers = ["ndc-lsp"]

[language-server.ndc-lsp]
command = "ndc"
args = ["lsp", "--stdio"]

[[grammar]]
name = "andy-cpp"
source = { git = "https://github.com/timfennis/andy-cpp", rev = "master", subpath = "ext/tree-sitter-andy-cpp" }
```

From a checkout of the Andy C++ repository, install the parser and queries without
Node.js or npm:

```bash
cd ext/tree-sitter-andy-cpp
./install.sh helix
```

The files are installed under
`${XDG_CONFIG_HOME:-$HOME/.config}/helix/runtime`. Check the setup with
`hx --health andy-cpp` (or `helix --health andy-cpp` on systems where the binary
uses that name).

## Other editors

Any editor with an LSP client can use the Andy C++ language server. Configure it
to run `ndc lsp --stdio` for `.ndc` files with the language id `andy-cpp`.

## Notes

- The server uses full-document synchronisation and re-analyses on each edit.
- While the buffer is mid-edit and doesn't parse, the last successful analysis is
  retained so hints and dot-completion keep working.
- The Tree-sitter grammar currently has trouble with doubly nested generic type
  annotations such as `List<List<Int>>`. Single-level generics work as expected.
