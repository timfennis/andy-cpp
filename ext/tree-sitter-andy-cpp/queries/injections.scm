; Highlight the leading `#!...` shebang line as bash.
; `#match?` (regex) is portable across Neovim and Helix; `#lua-match?` is
; Neovim-only and breaks Helix's combined query compilation.
((comment) @injection.content
  (#match? @injection.content "^#!")
  (#set! injection.language "bash"))
