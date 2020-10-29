(defun horner (x &rest args)
  (do ((ll (cdr args) (cdr ll))
       (res (car args)
            (+ (* x res) (car ll))))
      ((null ll) res)))


(horner 1 2 3 4)
(horner '3 1 -2 -5)
