;;;;;;; defined in mops package
(defvar *mop-map* nil)

; exported
(defun kb-map () *mop-map*)

; exported
(defun build-mop-map ()
  (setf *mop-map* (make-hash-table))
  (dolist (mop (kb-mops))
    (dolist (parent (cadr mop))
      (let ((lst (gethash parent (kb-map))))
        (setf (gethash parent (kb-map))
              (remove-duplicates (cons (car mop) lst)))))))


;;;;;; mop-search
(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)
(build-mop-map)

(defun has-slots-flat (mop slots)
  (every #'(lambda (slot)
             (multiple-value-bind (filler success) (get-filler mop (car slot))
               (and success (eql filler (cadr slot)))
               )) slots))

(defun is-duplicate (mop mops)
  (some #'(lambda (x) (and (isa-p mop x) (not (eql x mop)))) mops))

(defun deduplicate-mops (mops)
  (remove nil (mapcar #'(lambda (x) 
                          (unless (is-duplicate x mops) x)) mops)))

(defun mop-search-worker (mop slots)
  (if (has-slots-flat mop slots)
      (list mop)
      (do ((child (gethash mop (kb-map)) (cdr child))
           (res nil (remove-duplicates (append res (mop-search (car child) slots)))))
          ((null child) res))))

(defun mop-search (mop slots)
  (deduplicate-mops (mop-search-worker mop slots)))

(defun has-slots-flat (mop slots)
  (every #'(lambda (slot)
             (multiple-value-bind (filler success) (get-filler mop (car slot))
               (and success (eql filler (cadr slot)))
               )) slots))

(defun is-duplicate (mop mops)
  (some #'(lambda (x) (and (isa-p mop x) (not (eql x mop)))) mops))

(defun deduplicate-mops (mops)
  (remove nil (mapcar #'(lambda (x) 
                          (unless (is-duplicate x mops) x)) mops)))

(defun mop-search-worker (mop slots)
  (if (has-slots-flat mop slots)
      (list mop)
      (do ((child (gethash mop (kb-map)) (cdr child))
           (res nil (remove-duplicates (append res (mop-search (car child) slots)))))
          ((null child) res))))

(defun mop-search (mop slots)
  (deduplicate-mops (mop-search-worker mop slots)))


;;;;;; Self testing
(MOP-SEARCH 'EVENT '((ACTOR ELEPHANT)))
(MOP-SEARCH 'BIRD '((COLOR YELLOW))) ; (canary)
(MOP-SEARCH 'ANIMAL '((AGE 7))) ; (buddy)
(MOP-SEARCH 'ANIMAL '((BRAIN NIL))) ; (CHIHUAHUA)

(run-tests mop-search)

(gethash 'bird (kb-map))
(gethash 'canary (kb-map))

