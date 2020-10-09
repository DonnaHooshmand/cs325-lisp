(defun show-dots-str (lst)
  (cond ((null lst) nil)
        ((atom lst)
         (format nil "~a" lst))
        ((atom (car lst))
         (format nil "(~a . ~b)" 
                 (car lst)
                 (show-dots-str (cdr lst))))
        (t
         (format nil "(~a . ~b)"
                 (show-dots-str (car lst))
                 (show-dots-str (cdr lst))))))

(defun show-dots (lst)
  (format t "~a" (show-dots-str lst)))

(defun safe-cdr (lst) 
  (cond ((consp (cdr lst))
         (cdr lst))
        ((null (cdr lst))
         nil)
        (t
         (cons (cdr lst) nil))))

(defun cdr-is-pair (lst)
  (and (not (null (cdr lst))) (atom (cdr lst))))

(defun show-list-str (lst)
  (cond ((null lst) 
         (format nil "~a" nil))
        ((atom lst)
         (format nil "~a" lst))
        (t
         (do ((ll lst (safe-cdr ll))
              (res "" (concatenate 'string 
                                   res 
                                   " "
                                   (show-list-str (car ll))
                                   (if (cdr-is-pair ll)
                                       " ."
                                     "")
                                   )))
             ((null ll) 
              (concatenate 'string
                           "["
                           (subseq res 1)
                           "]"))))))

(defun show-list (lst)
  (format t "~a" (show-list-str lst)))


(show-list '(a b c))
(show-list '(((a b) c) d))
(show-list '(a (b c)))
(show-list '(a . b))
(show-list nil)
(show-list '(nil))
