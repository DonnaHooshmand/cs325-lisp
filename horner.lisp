(defun horner (x &rest args)
  (reduce #'(lambda (res curr)
              (+ (* x res) curr)) (cdr args) :initial-value (car args)))

(horner 1 2 3 4)
(horner '3 1 -2 -5)
