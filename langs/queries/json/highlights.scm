(null) @keyword
(true) @keyword
(false) @keyword

(number) @number

(pair
 key: (_) @keyword)

(string) @string

(object
 "{" @escape
 (_)
 "}" @escape)
