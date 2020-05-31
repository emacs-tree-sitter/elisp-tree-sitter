; Identifier naming conventions

((identifier) @constructor
 (#match? @constructor "^[A-Z]"))

((identifier) @constant
 (#match? @constant "^[A-Z][A-Z_]*$"))

; Function calls

(decorator) @function.call

((call
  function: (identifier) @function.builtin)
 (#match?
   @function.builtin
   "^(abs|all|any|ascii|bin|bool|breakpoint|bytearray|bytes|callable|chr|classmethod|compile|complex|delattr|dict|dir|divmod|enumerate|eval|exec|filter|float|format|frozenset|getattr|globals|hasattr|hash|help|hex|id|input|int|isinstance|issubclass|iter|len|list|locals|map|max|memoryview|min|next|object|oct|open|ord|pow|print|property|range|repr|reversed|round|set|setattr|slice|sorted|staticmethod|str|sum|super|tuple|type|vars|zip|__import__)$"))
(call
  function: (attribute attribute: (identifier) @function.call))
(call
  function: (identifier) @function.call)

; Function definitions

(function_definition
  name: (identifier) @function)

((identifier) @constant.builtin
 (#eq? @constant.builtin "self"))

(assignment (expression_list (identifier) @variable) (*))

;; (identifier) @variable
(parameters (identifier) @variable)
(typed_parameter (identifier) @variable (*))
(default_parameter name: (identifier) @variable)
(typed_default_parameter name: (identifier) @variable)
(attribute attribute: (identifier) @property)
(type (identifier) @type)

(keyword_argument name: (identifier) @variable)

; Literals

(none) @constant.builtin
(true) @constant.builtin
(false) @constant.builtin

(integer) @number
(float) @number

(interpolation
 "{" @punctuation.special
 (*) @embedded
 "}" @punctuation.special)

(comment) @comment
((string) @constant
 (#match? @constant "^'"))
((string) @doc
 (#match? @doc "^\"\"\""))
(string) @string
(escape_sequence) @escape

; Tokens

"-" @operator
"-=" @operator
"!=" @operator
"*" @operator
"**" @operator
"**=" @operator
"*=" @operator
"/" @operator
"//" @operator
"//=" @operator
"/=" @operator
"&" @operator
"%" @operator
"%=" @operator
"^" @operator
"+" @operator
"+=" @operator
"<" @operator
"<<" @operator
"<=" @operator
"<>" @operator
"=" @operator
"==" @operator
">" @operator
">=" @operator
">>" @operator
"|" @operator
"~" @operator
"and" @operator
"in" @operator
"is" @operator
"not" @operator
"or" @operator

; Keywords

"as" @keyword
"assert" @keyword
"async" @keyword
"await" @keyword
"break" @keyword
"class" @keyword
"continue" @keyword
"def" @keyword
"del" @keyword
"elif" @keyword
"else" @keyword
"except" @keyword
"exec" @keyword
"finally" @keyword
"for" @keyword
"from" @keyword
"global" @keyword
"if" @keyword
"import" @keyword
"lambda" @keyword
"nonlocal" @keyword
"pass" @keyword
"print" @keyword
"raise" @keyword
"return" @keyword
"try" @keyword
"while" @keyword
"with" @keyword
"yield" @keyword
