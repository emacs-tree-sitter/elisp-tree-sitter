["break"
 "case"
 "const"
 "continue"
 "default"
 "do"
 "else"
 "enum"
 "extern"
 "for"
 "if"
 "inline"
 "return"
 "sizeof"
 "static"
 "struct"
 "switch"
 "typedef"
 "union"
 "volatile"
 "while"
 "..."] @keyword

[(storage_class_specifier)
 (type_qualifier)] @keyword

["#define"
 "#else"
 "#endif"
 "#if"
 "#ifdef"
 "#ifndef"
 "#include"
 (preproc_directive)] @function.macro

((["#ifdef" "#ifndef"] (identifier) @constant))

;; (preproc_directive("#define" (identifier) @variable.special))

["--"
 "-"
 "-="
 "->"
 "!="
 "*"
 "&"
 "&&"
 "+"
 "++"
 "+="
 "<"
 "=="
 ">"
 "||"
 "="] @operator

["."
 ";"] @delimiter

;;; ----------------------------------------------------------------------------
;;; Functions.

(function_declarator
 declarator: (identifier) @function)
(preproc_function_def
 name: (identifier) @function)

(function_declarator
 (parenthesized_declarator
  (pointer_declarator (field_identifier) @function)))

(call_expression
 function: [(identifier) @function.call
            (field_expression
             field: (field_identifier) @method.call)])

;;; ----------------------------------------------------------------------------
;;; Types.

[(primitive_type)
 (sized_type_specifier)] @type.builtin

(type_identifier) @type

;;; ----------------------------------------------------------------------------
;;; Variables.

(declaration declarator: [(identifier) @variable
                          (_ (identifier) @variable)])

(parameter_declaration declarator: [(identifier) @variable.parameter
                                    (_ (identifier) @variable.parameter)])

(init_declarator declarator: [(identifier) @variable
                              (_ (identifier) @variable)])

(assignment_expression
 left: [(identifier) @variable
        (field_expression field: (field_identifier) @variable)
        (pointer_expression (identifier) @variable)])

(preproc_def name: (identifier) @variable.special)

(preproc_params
 (identifier) @variable.parameter)

;;; ----------------------------------------------------------------------------
;;; Properties.

(field_declaration
 declarator: [(field_identifier) @property.definition
              (pointer_declarator (field_identifier) @property.definition)
              (pointer_declarator (pointer_declarator (field_identifier) @property.definition))])

(enumerator name: (identifier) @property.definition)

(field_identifier) @property

;;; ----------------------------------------------------------------------------

((identifier) @constant
 (.match? @constant "^[A-Z_][A-Z_\\d]*$"))

[(null) (true) (false)] @constant
[(number_literal)
 (char_literal)] @number

(statement_identifier) @label

;;; ----------------------------------------------------------------------------

;; ((comment) @doc (preproc_def))
;; ((comment) @doc (declaration))
(comment) @comment

[(string_literal)
 (system_lib_string)] @string
