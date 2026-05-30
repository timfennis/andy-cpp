; Highlight the leading `#!...` shebang line as bash.
((comment) @injection.content
  (#lua-match? @injection.content "^#!")
  (#set! injection.language "bash"))
