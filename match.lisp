(load "~/quicklisp/local-projects/cs325/match.lisp")
(in-package #:exmatch)
(in-package #:match-tests)

(defun ?not (pat)
  pat)

(defun ?= (pat)
  pat)

(defun ?or (pats y lsts)
  (any (lambda (x) (print x)) pats))

(?or '(x y z) 'x '(nil))


(var-p '?not)
(fboundp '?not)

