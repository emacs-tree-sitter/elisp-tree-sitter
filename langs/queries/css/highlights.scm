(comment) @comment

;; (selectors) @function

(tag_name) @tag
(nesting_selector) @tag
(universal_selector) @tag

(id_selector) @function
(class_selector) @type

"~" @operator
">" @operator
"+" @operator
"-" @operator
"*" @operator
"/" @operator
"=" @operator
"^=" @operator
"|=" @operator
"~=" @operator
"$=" @operator
"*=" @operator

"and" @operator
"or" @operator
"not" @operator
"only" @operator

(attribute_selector (plain_value) @string)
(pseudo_element_selector (tag_name) @attribute)
(pseudo_class_selector (class_name) @attribute)

;; (class_name) @property
;; (id_name) @property
(namespace_name) @property
(property_name) @variable
(feature_name) @property

(attribute_name) @attribute

(function_name) @function

((property_name) @variable
 (.match? @variable "^--"))
((plain_value) @variable
 (.match? @variable "^--"))

"@media" @keyword
"@import" @keyword
"@charset" @keyword
"@namespace" @keyword
"@supports" @keyword
"@keyframes" @keyword
(at_keyword) @keyword
(to) @keyword
(from) @keyword
(important) @keyword

(string_value) @string
(color_value) @string.special

(unit) @type
(integer_value) @number
(float_value) @number
(plain_value) @keyword
;; (color_value) @attribute

"#" @punctuation.delimiter
"," @punctuation.delimiter
