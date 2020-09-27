;; Hello program
(defun hello () 
  (write-line "What's your name?")
  (let ((name (read-line)))
    (format t "Hello, ~A.~%" name)))


;; Write
(write "Single quote used, it inhibits evaluation")
(write '(* 2 3))
(write-line " ")
(write-line "Single quote not used, so expression evaluated")
(write (* 2 3))

(subseq "Hello, World!" 7 13)

(write-line (string-upcase "Hello world!"))

(length (string-upcase "Hello, world!"))

;; Predicates (boolean functions)
(numberp 10)
(oddp 9)
(evenp 10)
(stringp "hi")
(listp '(1 2 3))

(and (< 3 (* 2 5))
     (not (>= 2 6)))

;; Control structures
(+ 4 (if (= 2 2) (* 9 2) 7))

(if (> 3 2)
    (progn 
      (print "hello") 
      (print "yo")
      (print "whatssup?") 
      9)
    (+ 4 2 3))

(setf x (* 3 2))
(setf y (+ x 3))

