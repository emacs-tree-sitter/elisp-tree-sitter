;; Built-ins.

((identifier) @constant.builtin
 (.match? @constant.builtin "^(__all__|__doc__|__name__|__package__|NotImplemented|Ellipsis)$"))

;; XXX: Not really a keyword, but it's sort of a tradition.
((identifier) @keyword
 (.eq? @keyword "self"))

;; Function definitions.

(class_definition
 body: (block (function_definition name: (identifier) @method)))
(function_definition
 name: (identifier) @function)

;; Types.
([(type [(identifier) @type.builtin
         (subscript (identifier) @type.builtin)
         (tuple [(identifier) @type.builtin
                 (subscript (identifier) @type.builtin)])])
  (class_definition superclasses: (_ (identifier) @type.builtin))]
 ;; TODO: Include built-in exception types.
 (.match? @type.builtin "^(bool|bytearray|bytes|dict|float|int|list|object|set|str|tuple|unicode)$"))
(type [(subscript
        value: (identifier) @type
        subscript: (identifier) @type.argument)
       (identifier) @type
       (string) @type
       (tuple [(identifier) @type
               (subscript
                value: (identifier) @type
                subscript: (identifier) @type.argument)])])
(class_definition
 name: (identifier) @type
 superclasses: (argument_list (identifier) @type.super))

;; Variables.

;; TODO: Add @variable.use

(parameters [(identifier) @variable.parameter
             (typed_parameter (identifier) @variable.parameter ":")
             (default_parameter name: (identifier) @variable.parameter)
             (typed_default_parameter name: (identifier) @variable.parameter)
             ;; TODO: Make splat variables more visually distinct.
             (list_splat (identifier) @variable.parameter)
             (dictionary_splat (identifier) @variable.parameter)])

(for_statement (variables (identifier) @variable))
(for_in_clause (variables (identifier) @variable))

(named_expression name: (identifier) @variable)

(keyword_argument name: (identifier) @label)

(assignment left: (_ [(identifier) @variable
                      (subscript subscript: (identifier) @variable)
                      (attribute attribute: (identifier) @variable)
                      (tuple (identifier) @variable)
                      (list [(identifier) @variable
                             (list_splat (identifier) @variable)])]))
(augmented_assignment left: (_ [(identifier) @variable
                                (subscript subscript: (identifier) @variable)
                                (attribute attribute: (identifier) @variable)]))

;; Literals.

[(none) (true) (false)] @constant.builtin
[(integer) (float)] @number

;; Identifier naming conventions.

((identifier) @constant
 (.match? @constant "^[A-Z_][A-Z_\\d]*$"))

((identifier) @constructor
 (.match? @constructor "^[A-Z]"))

;; Function calls.

((call
  function: (identifier) @function.builtin)
 (.match?
   @function.builtin
   "^(abs|all|any|ascii|bin|bool|breakpoint|bytearray|bytes|callable|chr|classmethod|compile|complex|delattr|dict|dir|divmod|enumerate|eval|exec|filter|float|format|frozenset|getattr|globals|hasattr|hash|help|hex|id|input|int|isinstance|issubclass|iter|len|list|locals|map|max|memoryview|min|next|object|oct|open|ord|pow|print|property|range|repr|reversed|round|set|setattr|slice|sorted|staticmethod|str|sum|super|tuple|type|vars|zip|__import__)$"))
(call function: [(attribute attribute: (identifier) @method.call)
                 (identifier) @function.call])

;; Properties.

(attribute attribute: (identifier) @property)

;; Operators.

["-"
 "-="
 "!="
 "*"
 "**"
 "**="
 "*="
 "/"
 "//"
 "//="
 "/="
 "&"
 "%"
 "%="
 "^"
 "+"
 "+="
 "<"
 "<<"
 "<="
 "<>"
 "="
 "=="
 ">"
 ">="
 ">>"
 "|"
 "~"
 "and"
 "in"
 "is"
 "not"
 "or"] @operator

;; Keywords.

["as"
 "assert"
 "async"
 "await"
 "break"
 "class"
 "continue"
 "def"
 "del"
 "elif"
 "else"
 "except"
 "exec"
 "finally"
 "for"
 "from"
 "global"
 "if"
 "import"
 "lambda"
 "nonlocal"
 "pass"
 "print"
 "raise"
 "return"
 "try"
 "while"
 "with"
 "yield"] @keyword

;; "Contexts" may have internal highlighting -> low priority.

(escape_sequence) @escape

(interpolation
 "{" @punctuation.special
 (_) @embedded
 "}" @punctuation.special)

(comment) @comment
((string) @doc
 (.match? @doc "^\"\"\""))
(string) @string

(decorator) @function.special
