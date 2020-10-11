(defun show-dots (lst)
  (cond ((atom lst)
         (format t "~a" lst))
        (t
           (format t "(")
           (show-dots (car lst))
           (format t " . ")
           (show-dots (cdr lst))
           (format t ")"))))

(defun show-list (lst)
  (when (atom lst) (format t "~a" lst))
  (when (consp lst)
    (format t "[")
    (do ((ll lst 
             (cond ((listp (cdr ll)) (cdr ll))
                   (t
                    (format t ". ")
                    (list (cdr ll))))))
        ((null ll) nil)
      (show-list (car ll))
      (unless (null (cdr ll))
        (format t " ")))
    (format t "]")))


(show-list '(a b c . d))
(show-list '(a b c))
(show-list '(((a b) c) d))
(show-list '(a (b c)))
(show-list '(a . b))
(show-list 'a)
(show-list nil)
(show-list '(nil))
