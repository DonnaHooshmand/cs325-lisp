(load "~/quicklisp/local-projects/cs325/match.lisp")
(in-package #:exmatch)
(in-package #:match-tests)

(run-tests)


(defun ?not (pat)
  pat)

(defun ?= (pat)
  pat)

(defun ?or (path)
  pat)


(var-p '?not)
(fboundp '?not)

