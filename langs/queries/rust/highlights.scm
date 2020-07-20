;;; Identifier conventions

((identifier) @keyword
 (.eq? @keyword "Self"))
((type_identifier) @keyword
 (.eq? @keyword "Self"))

;; Assume all-cap names are constants.
((identifier) @constant
 (.match? @constant "^[A-Z_][A-Z_\\d]*$"))

;; Assume that uppercase names in paths are types.
((scoped_identifier
  path: [(identifier) @type
         (scoped_identifier
          name: (identifier) @type)])
 (.match? @type "^[A-Z]"))

;; Assume other uppercase names are enum constructors
((identifier) @constructor
 (.match? @constructor "^[A-Z]"))

;;; Function calls.

(call_expression
 function: [(identifier) @function.call
            (field_expression
             field: (field_identifier) @method.call)
            (scoped_identifier
             name: (identifier) @function.call)])

(generic_function
 function: [(identifier) @function.call
            (field_expression
             field: (field_identifier) @method.call)
            (scoped_identifier
             name: (identifier) @function.call)])

(macro_invocation
 macro: [(identifier) @function.macro
         (scoped_identifier
          name: (identifier) @function.macro)]
 "!" @function.macro)

;;; Function definitions.

(function_item
 name: (identifier) @method
 parameters: (parameters [(self_parameter)
                          (parameter (self))]))
(function_item (identifier) @function)
(function_signature_item
 name: (identifier) @method
 parameters: (parameters [(self_parameter)
                          (parameter (self))]))
(function_signature_item (identifier) @function)
(macro_definition (identifier) @function)

;;; Types.

(type_arguments "<" @punctuation.bracket
                (type_identifier)? @type.argument
                ">" @punctuation.bracket)
(type_parameters "<" @punctuation.bracket
                 (type_identifier)? @type.parameter
                 ">" @punctuation.bracket)
(where_predicate
 left: (type_identifier) @type.parameter)

(type_identifier) @type
(primitive_type) @type.builtin

;;; Properties.

(field_declaration
 name: (field_identifier) @property.definition)
(field_identifier) @property

;;; Comments and docstrings.

((line_comment) @doc
 (.match? @doc "^///"))
[(line_comment)
 (block_comment)] @comment

;;; Punctuations.

["("
 ")"
 "["
 "]"] @punctuation.bracket

["::"
 "."
 ";"] @punctuation.delimiter

;;; Variable bindings

(let_declaration pattern: [(identifier) @variable
                           (_ (identifier) @variable)])
(if_let_expression pattern: [(identifier) @variable
                             (_ (identifier) @variable)])
(for_expression pattern: [(identifier) @variable
                          (_ (identifier) @variable)])

(parameter (identifier) @variable.parameter)

(token_binding_pattern
 name: (metavariable) @variable.parameter
 type: (fragment_specifier) @type)
(metavariable) @label

;;; Lifetime.

((lifetime (identifier) @type.builtin)
 (.eq? @type.builtin "static"))
(lifetime (identifier) @label)

;;; Keywords.

["async"
 "await"
 "break"
 "const"
 "continue"
 "default"
 "dyn"
 "else"
 "enum"
 "extern"
 "fn"
 "for"
 "if"
 "impl"
 "in"
 "let"
 "loop"
 "macro_rules!"
 "match"
 "mod"
 "move"
 "pub"
 "ref"
 "return"
 "static"
 "struct"
 "trait"
 "type"
 "union"
 "unsafe"
 "use"
 "where"
 "while"] @keyword

[(self)
 (super)
 (crate)
 (mutable_specifier)] @keyword

;;; Misc.

[(char_literal)
 (string_literal)
 (raw_string_literal)] @string

[(boolean_literal)
 (integer_literal)
 (float_literal)] @constant.builtin

(escape_sequence) @escape

[(attribute_item)
 (inner_attribute_item)] @attribute

["as"
 "&&" "||" "&"
 "^"
 "==" "!=" "<=" ">="
 "<<" ">>"
 "+" "-" "*" "/" "%"
 "'" "?" ] @operator

;; These are not always operators (e.g. can be type/closure param brackets).
(binary_expression
 operator: ["|" "<" ">"] @operator)

;;; Paths' prefixes. TODO: Use a different scope.

(scoped_identifier path: [(identifier) @constant
                          (scoped_identifier name: (identifier) @constant)])
(scoped_type_identifier path: (identifier) @constant)
(scoped_use_list path: [(identifier) @constant
                        (scoped_identifier name: (identifier) @constant)])
(use_wildcard [(identifier) @constant
               (scoped_identifier name: (identifier) @constant)])

;; (use_declaration
;;  [(identifier) @function
;;   (scoped_identifier
;;    name: (identifier) @function)])
;; (use_list (identifier) @function)
