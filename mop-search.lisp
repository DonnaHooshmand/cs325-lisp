(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)

;;;;;;; defined in mops package
(defvar *mop-map* nil)

(defun kb-map () *mop-map*)

(defun load-kb (pathname)
  (load-data pathname 'set-kb)
  (build-mop-map))

(defun build-mop-map ()
  (setf *mop-map* (make-hash-table))
  (dolist (mop (kb-mops))
    (dolist (slot (cddr mop))
      (setf (gethash (cadr slot) (kb-map))
            (adjoin (car mop) (gethash (cadr slot) (kb-map)))))))

(defun valid-mop-p (mop mop-abst slots)
  (and (isa-p mop mop-abst)
       (has-slots-p mop slots)))

(defun mop-search (mop slots)
  (when (null (kb-map))
    (build-mop-map))
  (let ((candidates (mapcan #'(lambda (slot)
                                (gethash (cadr slot) (kb-map)))
                            slots)))
    (mapcan #'(lambda (candidate)
                (when (valid-mop-p candidate mop slots)
                  (list candidate)))
            candidates)))

;;;;;; mop-search
(MOP-SEARCH 'ANIMAL '((BRAIN NIL))) ; (CHIHUAHUA)

;;;;;; Self testing
(MOP-SEARCH 'EVENT '((ACTOR ELEPHANT))) ; NIL
(MOP-SEARCH 'BIRD '((COLOR YELLOW))) ; (canary)
(MOP-SEARCH 'BIRD '((COLOR YELLOW) (OWNER GRANNY))) ; (TWEETY)
(MOP-SEARCH 'ANIMAL '((AGE 7))) ; (buddy)
(MOP-SEARCH 'ANIMAL '((BRAIN NIL))) ; (CHIHUAHUA)

(assert-equal nil (mop-search 'event '((actor elephant))))
(assert-equal '(event-1) (mop-search 'event '((actor clyde-1))))

(run-tests mop-search)

(get-filler 'canary 'color)

(gethash 'bird (kb-map))
(gethash 'canary (kb-map))

