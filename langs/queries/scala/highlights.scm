((identifier) @constant.builtin
 (.match? @constant.builtin "^(null|true|false)$"))

(number) @constant

;; Function bindings.

(function_definition name: (identifier) @function)
(function_declaration name: (identifier) @function)

(annotation
 "@" @function.special
 name: (_) @function.special)

;; Variable bindings.

(call_expression
 (arguments (assignment_expression
             left: (identifier) @label)))

(capture_pattern name: (_) @variable)

(val_definition pattern: (identifier) @variable)
(val_declaration name: (identifier) @variable)

((field_expression value: (identifier) @variable.parameter)
 (.eq? @variable.parameter "_"))

;; TODO
(var_definition pattern: (identifier) @variable.special)
(var_declaration name: (identifier) @variable.special)

(assignment_expression
 left: (identifier) @variable.special)

(parameter (identifier) @variable.parameter)
(class_parameter (identifier) @variable.parameter)

;; Keywords.

["abstract" "case" "catch" "class" "def" "else" "extends"
 "final" "finally" "if" "implicit" "import"
 "lazy" "match" "new" "object" "override" "package" "private"
 "protected" "sealed" "trait" "try" "type"
 "val" "var" "with"] @keyword

["=" (wildcard) (operator_identifier)] @operator

(string_transform_expression (identifier) @label)

;; Types.

(type_identifier) @type
(type_parameters (identifier) @type.parameter)

;; Assume all-cap names are constants.
((identifier) @constant
 (.match? @constant "^[A-Z_][A-Z_\\d]*$"))

;; Assume other uppercase names are enum constructors.
((identifier) @constructor
 (.match? @constructor "^[A-Z]"))

;; Function calls.

(call_expression
 function: [(identifier) @function.call
            (field_expression
             "." (identifier) @method.call)])

(infix_expression
 operator: [(identifier) @function.call
            (operator_identifier) @keyword])

;; Properties.
(field_expression
 field: (identifier) @property)

;; "Contexts".

((comment) @doc
 (.match? @doc "^/\\*\\*"))
(comment) @comment

(interpolation
 "$" @keyword
 (_) @embedded)
(string) @string
