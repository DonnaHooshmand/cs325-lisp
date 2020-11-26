(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)

(run-tests pedalo)

(defun get-options (ll)
  (mapcar #'(lambda (l) (car l)) ll))

(defun get-lst-no-first (ll)
  (mapcar #'(lambda (l) (cdr l)) ll))

(defun ready-p (option ll)
  (not (some #'(lambda (l) (member option l)) ll)))

(defun find-first-ready (ll)
  (do ((i 0 (1+ i))
       (options (get-options ll) (cdr options))
       (lst (get-lst-no-first ll)))
      ((or (null options) 
           (ready-p (car options) lst))
       (car options))))

(defun remove-ready (ll)
  (let ((ready (find-first-ready ll)))
    (values ready (remove nil (mapcar #'(lambda (x)
                                (remove ready x)) ll)))))

(defun merge-parents-list (ll &optional (res nil))
  (if (null ll)
      (nreverse res)
      (multiple-value-bind (ready ll2) (remove-ready ll)
        (merge-parents-list ll2 (cons ready res)))))

(defun build-parents (mop)
  (let ((parents (cadr (get-mop mop))))
    (cons mop (merge-parents-list 
                (mapcar #'(lambda (x) (build-parents x)) parents)))))

(defun linearize (mop)
  (build-parents (car mop)))
