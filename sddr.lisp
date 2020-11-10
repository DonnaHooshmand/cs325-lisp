(in-package :cs325-user)
(ql:quickload "sddr")
(in-package :sddr-tests)

(ask '(append nil nil nil) *append-kb*)
(ask '(append (cons 1 nil) nil (cons 1 nil)) *append-kb*)

(ask '(append (cons 1 nil) (cons 2 nil) (cons 1 (cons 2 nil))) *append-kb*)

(ask '(append (cons 1 nil) (cons 2 nil) ?x) *append-kb*)

(ask '(append (cons 1 (cons 3 nil)) ?x (cons 1 (cons 3 (cons 2)))) *append-kb*)

; ldiff: List difference from the front
(ldiff '(cons 1 (cons 3 nil)) '(cons 1 (cons 3 (cons 2))))
(ldiff '(cons 1 (cons 3 (cons 2 nil))) '(cons 1 (cons 3 nil)))



