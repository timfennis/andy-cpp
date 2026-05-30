# tree-sitter-andy-cpp

A [tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar for the
**Andy C++** (`.ndc`) language.

Tree-sitter powers incremental, error-tolerant syntax trees used by editors such
as **Neovim**, **Helix**, **Zed**, and **Emacs** for highlighting, structural
selection, folding, and code navigation. (VS Code does not use tree-sitter for
highlighting — it uses the TextMate grammar in `../andy-cpp/syntaxes/`.)

The grammar mirrors the precedence ladder and constructs implemented in
`ndc_lexer` / `ndc_parser`. It is validated against the interpreter's full
functional-test corpus: every valid `.ndc` program under
`tests/functional/programs/` parses without errors.

## Layout

```
grammar.js            # the grammar definition
tree-sitter.json      # package metadata (generated/maintained by the CLI)
queries/
  highlights.scm      # syntax highlighting
  locals.scm          # scopes & definitions (variables, params, functions)
  injections.scm      # `#!` shebang line highlighted as bash
test/corpus/          # tree-sitter test cases
src/                  # generated parser (run `tree-sitter generate`)
  scanner.c           # external scanner: named op-assign + raw strings
```

## Developing

Requires Node.js. The tree-sitter CLI is a dev dependency.

```bash
cd ext/tree-sitter-andy-cpp
npm install                      # installs tree-sitter-cli
npx tree-sitter generate         # regenerate src/parser.c from grammar.js
npx tree-sitter test             # run test/corpus
npx tree-sitter parse path.ndc   # dump the parse tree for a file
```

Re-run `generate` after every edit to `grammar.js`. Commit the regenerated
`src/` so consumers can build without the CLI.

### Re-validating against the interpreter corpus

```bash
cd ext/tree-sitter-andy-cpp
fail=0
for f in $(find ../../tests/functional/programs -name '*.ndc'); do
  npx tree-sitter parse -q "$f" >/dev/null 2>&1 || { echo "ERR $f"; fail=1; }
done
[ $fail -eq 0 ] && echo "all valid programs parse"
```

The only files that report errors are the interpreter's deliberate
`// expect-error:` cases (malformed input) — that is the expected outcome.

## Editor integration

The parser's language name is **`andy_cpp`** (the symbol exported by the
generated parser is `tree_sitter_andy_cpp`).

The instructions below drive each editor's **built-in** tree-sitter runtime, so
they don't depend on a plugin manager or a specific nvim-treesitter version.
Building the parser needs Node.js and a C compiler.

### Optional: a language server

The interpreter ships an LSP server, started with `ndc lsp` over stdio. It
provides hover (inferred types), completion, go-to-definition, document symbols
and inlay hints. Install the `ndc` binary so it's on your `PATH`:

```bash
cargo install --git https://github.com/timfennis/andy-cpp
```

The editor sections below wire this up alongside highlighting.

### Neovim

Neovim has a built-in tree-sitter runtime, so nvim-treesitter is not required to
load this grammar.

1. Build the parser and install it with the queries where Neovim's runtimepath
   can find them (the output file must be named `andy_cpp.so`):

   ```bash
   cd ext/tree-sitter-andy-cpp
   npm install
   mkdir -p ~/.config/nvim/parser ~/.config/nvim/queries/andy_cpp
   npx tree-sitter build -o ~/.config/nvim/parser/andy_cpp.so
   cp queries/*.scm ~/.config/nvim/queries/andy_cpp/
   ```

2. Add to your config (`init.lua`):

   ```lua
   -- Treat .ndc files as the `andy_cpp` filetype.
   vim.filetype.add({ extension = { ndc = "andy_cpp" } })

   -- Start tree-sitter highlighting for those buffers.
   vim.api.nvim_create_autocmd("FileType", {
     pattern = "andy_cpp",
     callback = function(args)
       pcall(vim.treesitter.start, args.buf, "andy_cpp")
     end,
   })

   -- Language server (Neovim 0.11+). Skip if you didn't install `ndc`.
   vim.lsp.config("ndc_lsp", {
     cmd = { "ndc", "lsp" },
     filetypes = { "andy_cpp" },
     root_markers = { ".git" }, -- falls back to the file's directory
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

Rebuild (step 1) after each `tree-sitter generate`, re-copy the queries after
editing them, then restart Neovim. After rebuilding the `ndc` binary, reload the
server with `:LspRestart`.

> **Already map `.ndc` to a different filetype?** (for example, via an existing
> `ftdetect` rule.) Keep that filetype, drop the `vim.filetype.add` call, and
> point the parser at it with
> `vim.treesitter.language.register("andy_cpp", "<your_filetype>")`. Use
> `<your_filetype>` as the autocmd `pattern` and in the LSP `filetypes` list.

### Helix

Helix has built-in tree-sitter and LSP support. Add to
`~/.config/helix/languages.toml`:

```toml
[[language]]
name = "andy-cpp"
scope = "source.andy-cpp"
file-types = ["ndc"]
comment-tokens = ["//"]
indent = { tab-width = 4, unit = "    " }
language-servers = ["ndc-lsp"] # omit if you didn't install `ndc`

[language-server.ndc-lsp]
command = "ndc"
args = ["lsp"]

[[grammar]]
name = "andy-cpp"
source = { git = "https://github.com/timfennis/andy-cpp", subpath = "ext/tree-sitter-andy-cpp" }
```

Fetch and build the grammar, then install the queries:

```bash
hx --grammar fetch
hx --grammar build
mkdir -p ~/.config/helix/runtime/queries/andy-cpp
cp ext/tree-sitter-andy-cpp/queries/*.scm ~/.config/helix/runtime/queries/andy-cpp/
```

## Known limitations

- **Doubly-nested generics** in type annotations (`List<List<Int>>`) can
  mis-tokenize the closing `>>`. Single-level generics (`Map<String, Int>`,
  `Option<Any>`) are fine.
