; Comments
(comment) @comment @spell

; Literals
(integer) @number
(float) @number.float
(complex) @number.float
(boolean) @boolean
(special_constant) @constant.builtin

(string) @string
(raw_string) @string
(escape_sequence) @string.escape

; Types
(type_identifier) @type
(generic_type name: (type_identifier) @type)

; Variables (generic fallback — the specific captures below override it,
; because Neovim resolves overlapping captures with last-match-wins)
(identifier) @variable

; Functions
(function_definition name: (identifier) @function)
(call function: (identifier) @function.call)
(member_expression method: (identifier) @function.method)

; Parameters
(parameter name: (identifier) @variable.parameter)

; Keywords
[
  "let"
  "fn"
  "pure"
] @keyword

[
  "if"
  "else"
  "while"
  "for"
  "return"
] @keyword.control

[
  (break_expression)
  (continue_expression)
] @keyword.control

"in" @keyword.operator

[
  "and"
  "or"
  "not"
] @keyword.operator

; Operators
[
  "="
  "+" "-" "*" "/" "\\" "%" "%%" "^"
  "&" "|" "~" "<<" ">>"
  "==" "!=" ">" ">=" "<" "<=" "<=>" ">=<"
  "++" "<>"
  ".." "..="
  "!"
  "->" "=>"
  "+=" "-=" "*=" "/=" "\\=" "%=" "%%=" "^="
  "&=" "|=" "~=" "++=" "<>=" "<<=" ">>="
] @operator

(named_op_assign) @operator

; Punctuation
[ "(" ")" "[" "]" "{" "}" "%{" ] @punctuation.bracket
[ "," ";" ] @punctuation.delimiter
[ ":" "." ] @punctuation.delimiter
