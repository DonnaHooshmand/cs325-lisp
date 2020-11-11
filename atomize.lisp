(defun parse-join (tok res)
  (cond ((stringp tok) 
         (cons (read-from-string (nsubstitute #\_ #\Space tok)) res))
        ((consp tok)
         (mapcan (lambda (x) (if (atom x) (list x) x))
                 (list '|)| (nreverse tok) '|(| res)))
        (t (cons tok res))))

(defun atomize-recursive (str idx len res)
  (cond ((= idx len) (reverse res))
        ((eql #\. (char str idx))
         (atomize-recursive str (1+ idx) len (cons '|.| res)))
        ((eql #\, (char str idx))
         (atomize-recursive str (1+ idx) len res))
        (t (multiple-value-bind 
               (tok idx) 
               (read-from-string str t nil :start idx)
             (atomize-recursive str idx len (parse-join tok res))))))

(defun atomize (str)
  (atomize-recursive str 0 (length str) nil))

(atomize "SELECT ?movie, ?year 
WHERE {
  ?movie movie ?year .
  ?movie actor \"Tom Cruise\"
  FILTER (
    ?year > 1990
  )
}")

(atomize "FILTER ( x > 10 )")

(atomize "SELECT ?movie, ?year")

(atomize " \"Tom Cruise\" age 56")
