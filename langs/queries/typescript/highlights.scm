

(module
 "module" @keyword
 name: (_) @constant)
(internal_module
 "namespace"
 name: (_) @constant)

;; Function bindings.

(method_signature
 name: (property_identifier) @method)
(function_signature
 name: (identifier) @function)

;; Types.

(type_arguments
  "<" @punctuation.bracket
  ">" @punctuation.bracket)
(type_parameters
 "<" @punctuation.bracket
 ">" @punctuation.bracket)

(type_parameter (type_identifier) @type.parameter)

(type_identifier) @type
(predefined_type) @type.builtin
((identifier) @type
 (match? @type "^[A-Z]"))

(union_type "|" @keyword)
(optional_parameter "?" @keyword)

;; Variable bindings.

(required_parameter (identifier) @variable.parameter)
(optional_parameter (identifier) @variable.parameter)

(property_signature
 name: (property_identifier) @property.definition)
(enum_body
 [(property_identifier) @property.definition
  (enum_assignment
   (property_identifier) @property.definition)])

;; Keywords.

["abstract"
 "declare"
 "enum"
 "export"
 "implements"
 "interface"
 "keyof"
 "namespace"
 "private"
 "protected"
 "public"
 "type"
 "!"
 (readonly)] @keyword
