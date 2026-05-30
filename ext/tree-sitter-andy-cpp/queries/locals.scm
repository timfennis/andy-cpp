; Scopes
(block) @local.scope
(function_definition) @local.scope
(for_expression) @local.scope
(while_expression) @local.scope
(list_comprehension) @local.scope
(map_comprehension) @local.scope

; Definitions
(variable_declaration
  name: (identifier) @local.definition.var)

(function_definition
  name: (identifier) @local.definition.function)

(parameter
  name: (identifier) @local.definition.parameter)

(for_binding
  pattern: (identifier) @local.definition.var)

; References
(identifier) @local.reference
