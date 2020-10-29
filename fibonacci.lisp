#!/usr/local/bin/sbcl --script
(defun generate (n &optional (a 0) (b 1) (acc (list)))
  (if (<= n 0)
      (reverse acc)
      (generate (1- n) b (+ a b) (cons a acc))))

(format t "~a~%" (generate 800))
