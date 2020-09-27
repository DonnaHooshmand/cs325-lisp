(in-package #:cl-user)

(defpackage #:fibonacci
  (:use #:cl)
  (:export #:generate))


(in-package #:fibonacci)

(defun generate (n &optional (a 0) (b 1) (acc (list)))
  (if (<= n 0)
      (reverse acc)
      (generate (1- n) b (+ a b) (push acc a))))
