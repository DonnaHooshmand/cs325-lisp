(in-package :cs325-user)

(defparameter *facts*
  '((bird isa animal)
    (pet isa animal)
    (canary isa bird)
    (penguin isa bird)
    (animal can-fly no)
    (bird can-fly yes)
    (penguin can-fly no)
    (tweety isa canary)
    (tweety isa pet)
    (chilly isa penguin)))

(defun foo (x)
  (cons x 
        (mapcan (lambda (fact)
                  (and (eql x (car fact))
                       (eql 'isa (cadr fact))
                       (foo (caddr fact))))
                *facts*)))

;;; for testing; ignore
(defun show-all ()
  (dolist (x '(animal bird canary tweety penguin chilly))
    (format t "~%~S => ~S" x (foo x))))