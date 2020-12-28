;; Keywords

["catch"
 "class"
 "constexpr"
 "delete"
 "explicit"
 "final"
 "friend"
 "mutable"
 "namespace"
 "noexcept"
 "new"
 "override"
 "private"
 "protected"
 "public"
 "template"
 "throw"
 "try"
 "typename"
 "using"
 "virtual"] @keyword

;;; ----------------------------------------------------------------------------
;; Functions

(call_expression
 function: (scoped_identifier name: (_) @function.call))

(template_function
 name: [(identifier) @function.call
        (scoped_identifier name: (_) @function.call)])

(template_method
 name: [(field_identifier) @method.call
        (scoped_field_identifier name: (_) @method.call)])

(function_declarator
 declarator: [(field_identifier) @function
              (scoped_identifier name: (_) @function)])

;;; ----------------------------------------------------------------------------
;; Types

((namespace_identifier) @type
 (.match? @type "^[A-Za-z]"))

(namespace_definition (identifier) @type)

(auto) @type

;;; ----------------------------------------------------------------------------
;; Constants

(this) @variable.builtin
(nullptr) @constant
