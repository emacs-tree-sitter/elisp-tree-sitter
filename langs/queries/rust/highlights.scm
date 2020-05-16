; Identifier conventions

((identifier) @keyword
 (#eq? @keyword "Self"))
((type_identifier) @keyword
 (#eq? @keyword "Self"))

; Assume all-caps names are constants
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z\\d_]+$'"))

; Assume that uppercase names in paths are types
((scoped_identifier
  path: (identifier) @type)
 (#match? @type "^[A-Z]"))
((scoped_identifier
  path: (scoped_identifier
         name: (identifier) @type))
 (#match? @type "^[A-Z]"))

; Assume other uppercase names are enum constructors
((identifier) @constructor
 (#match? @constructor "^[A-Z]"))

; Function calls

(call_expression
  function: (identifier) @function.call)
(call_expression
 function: (field_expression
            field: (field_identifier) @function.call))
(call_expression
 function: (scoped_identifier
            name: (identifier) @function.call))

(generic_function
  function: (identifier) @function)
(generic_function
 function: (scoped_identifier
            name: (identifier) @function))
(generic_function
 function: (field_expression
            field: (field_identifier) @function.call))

(macro_invocation
  macro: (identifier) @function.macro
  "!" @function.macro)

; Function definitions

(function_item (identifier) @function)
(function_signature_item (identifier) @function)

; Other identifiers

(type_arguments
 "<" @punctuation.bracket
 ">" @punctuation.bracket)
(type_parameters
 "<" @punctuation.bracket
 ">" @punctuation.bracket)
(where_predicate
 left: (type_identifier) @type.parameter)
(type_arguments
 (type_identifier) @type.argument)
(type_parameters
 (type_identifier) @type.parameter)

(type_identifier) @type
(primitive_type) @type.builtin
(field_declaration
 name: (field_identifier) @property)
(field_identifier) @property

((line_comment) @doc
 (#match? @doc "^///"))
(line_comment) @comment
(block_comment) @comment

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket


"::" @punctuation.delimiter
"." @punctuation.delimiter
";" @punctuation.delimiter

;;; Variable bindings
(let_declaration
 pattern: (identifier) @variable)
(let_declaration
 pattern: (_ (identifier) @variable))
(if_let_expression
 pattern: (identifier) @variable)
(if_let_expression
 pattern: (_ (identifier) @variable))
(for_expression
 pattern: (identifier) @variable)
(for_expression
 pattern: (_ (identifier) @variable))

(parameter (identifier) @variable.parameter)

((lifetime (identifier) @type.builtin)
 (#eq? @type.builtin "static"))
(lifetime (identifier) @label)

"break" @keyword
"const" @keyword
"continue" @keyword
"default" @keyword
"dyn" @keyword
"else" @keyword
"enum" @keyword
"extern" @keyword
"fn" @keyword
"for" @keyword
"if" @keyword
"impl" @keyword
"in" @keyword
"let" @keyword
"let" @keyword
"loop" @keyword
"macro_rules!" @keyword
"match" @keyword
"mod" @keyword
"move" @keyword
"pub" @keyword
"ref" @keyword
"return" @keyword
"static" @keyword
"struct" @keyword
"trait" @keyword
"type" @keyword
"union" @keyword
"unsafe" @keyword
"use" @keyword
"where" @keyword
"while" @keyword
"?" @keyword
(mutable_specifier) @keyword
(use_list (self) @keyword)
(scoped_use_list (self) @keyword)
(scoped_identifier (self) @keyword)
(super) @keyword
(visibility_modifier
 (_) @keyword)

(self) @keyword

(char_literal) @string
(string_literal) @string
(raw_string_literal) @string

(boolean_literal) @constant.builtin
(integer_literal) @constant.builtin
(float_literal) @constant.builtin

(escape_sequence) @escape

(attribute_item) @attribute
(inner_attribute_item) @attribute

"as" @operator
"*" @operator
"&" @operator
"'" @operator
"==" @operator

;;; Paths' prefixes. TODO: Use a different scope.
(scoped_identifier
 path: (identifier) @constant)
(scoped_identifier
 path: (_ (identifier) @constant))
(scoped_type_identifier
 path: (identifier) @constant)
(scoped_type_identifier
 path: (_ (identifier) @constant))
(scoped_use_list
 path: (identifier) @constant)
(scoped_use_list
 path: (_ (identifier) @constant))
