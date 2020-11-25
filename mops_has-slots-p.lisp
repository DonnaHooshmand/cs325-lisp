(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)

(run-tests has-slots-p)

(defun is-valid-slot (mop slot)
  (multiple-value-bind (filler has-filler) (get-filler mop (car slot))
    (and has-filler (isa-p filler (cadr slot)))))

(defun has-slots-p (mop slots)
  (every #'(lambda (slot) 
             (is-valid-slot mop slot))
         slots))

(get-filler 'event-1 'actor)
(get-filler 'tweety 'brain)
(get-filler 'buddy 'brain)

(is-valid-slot 'tweety '(brain nil))
(has-slots-p 'tweety '((brain nil)))
(is-valid-slot 'jumbo-1 '(name "Jumbo"))
(has-slots-p 'jumbo-1 '((color gray) (name "Jumbo")))
(has-slots-p 'event-1 '((actor elephant) (action ingest)))
