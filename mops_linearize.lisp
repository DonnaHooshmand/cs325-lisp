(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)

(assoc 'pedalo (kb-absts))

(run-tests pedalo)

(defun get-options (ll)
  (mapcar #'(lambda (l) (car l)) ll))

(defun ready-p (option ll)
  (not (some #'(lambda (l) (member option (cdr l))) ll)))

(defun find-first-ready (ll)
  (some #'(lambda (option)
            (when (ready-p option ll) option))
        (get-options ll)))

(defun remove-ready (ll ready)
  (mapcan #'(lambda (l)
              (let ((x (remove ready l)))
                (unless (null x)
                  (list x))))
          ll))

(defun merge-parents-list (parents-list)
  (do* ((ll parents-list (remove-ready ll ready))
        (ready (find-first-ready ll) (find-first-ready ll))
        (res (list ready) (cons ready res)))
    ((null ll) (nreverse (cdr res)))
    ))

(defun build-parents (mop)
  (let ((parents (cadr (get-mop mop))))
    (cons mop (merge-parents-list 
                (mapcar #'(lambda (x) (build-parents x)) parents)))))

(defun linearize (mop)
  (build-parents (car mop)))


(find-first-ready '((A C B E F) (B E F)))
(merge-parents-list '((A C B E F) (B E F)))
(mapcan #'(lambda (x) (unless (= x 2) (list x))) '(1 2 3))
(nconc '((1 2 3)1 2 3 4) '(1 2 3))
