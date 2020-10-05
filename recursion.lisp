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
  (when (> x 0)
    (format t ".")
    (print-dots (1- x))))


(defun get-a-count (lst)
  (do ((ll lst (cdr ll))
       (cnt 0 (if (eql (car ll) 'a)
                  (1+ cnt) 
                  cnt)))
      ((null ll) cnt)))

(get-a-count '(a b a c a))

(defun get-a-count (lst &optional (cnt 0))
  (if lst
      (get-a-count 
        (cdr lst) 
        (+ cnt (if (eql (car lst) 'a)
                   1
                   0)))
      cnt))

;; REMOVE is a pure function that returns the modified list
;; as a value. Hence it must be used in the argument of APPLY
;; as the original list from the argument is not modified.
(defun summit (lst)
  (apply #'+ (remove nil lst)))

;; This recursive function does not handle the base case
;; for when the list is empty.
(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))
