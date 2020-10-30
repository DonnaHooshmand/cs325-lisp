(ql:quickload "drakma")
(ql:quickload "cl-json")
(ql:quickload "cl-html-parse")

(load "~/quicklisp/local-projects/cs325/fall2019/triples-v3.lisp")

; returns a string with the text in the file retrieved with the given URL 
(flexi-streams:octets-to-string (drakma:http-request "url-string" :force-binary t))

; takes a string with HTML and returns a nested Lisp syntax that's easy to process with normal Lisp functions
(net.html.parser:parse-html string)

; takes a string with JSON (or JSON-LD) and returns a nested Lisp syntax that's easy to process with normal Lisp functions.
(json:decode-json-from-string string with JSON) 

(defun fetch (url)
  (net.html.parser:parse-html
    (flexi-streams:octets-to-string
      (drakma:http-request url :force-binary t))))

(defparameter *data* (fetch "https://www.foodnetwork.com/recipes/food-network-kitchen/honey-mustard-dressing-recipe-2011614"))

*data*

