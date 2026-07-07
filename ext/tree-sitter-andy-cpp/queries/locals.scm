; Scopes
(block) @local.scope
(function_definition) @local.scope
(for_expression) @local.scope
(while_expression) @local.scope
(list_comprehension) @local.scope
(map_comprehension) @local.scope

; Definitions
; Plain and destructured let bindings: `let a = …`, `let [a, b] = …`,
; `let (a, b) = …`, `let a, b = …`, and nested patterns like
; `for (x, y), [a, b, c] in …`. The wildcard levels descend through the
; list / tuple / pattern_sequence containers.
(variable_declaration
  name: (identifier) @local.definition.var)
(variable_declaration
  name: (_ (identifier) @local.definition.var))
(variable_declaration
  name: (_ (_ (identifier) @local.definition.var)))

(function_definition
  name: (identifier) @local.definition.function)

; A struct declaration binds its constructor by the struct's name.
(struct_definition
  name: (type_identifier) @local.definition.type)

(parameter
  name: (identifier) @local.definition.parameter)
(parameter
  name: (_ (identifier) @local.definition.parameter))

(for_binding
  pattern: (identifier) @local.definition.var)
(for_binding
  pattern: (_ (identifier) @local.definition.var))
(for_binding
  pattern: (_ (_ (identifier) @local.definition.var)))

; References
(identifier) @local.reference
