(defun greater (x y)
  (if (> x y)
      x
      y))

(defun has-list-p (lst)
  (if (null lst)
      nil
      (or (listp (car lst))
          (has-list-p (cdr lst)))))

(defun print-dots (x)
  (do ((i 1 (1+ i)))
      ((> i x) t)
    (format t ".")))

(defun print-dots (x)
  (or (<= x 0)
      (progn
        (format t ".")
        (print-dots (1- x)))))

(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))
      )
  )
