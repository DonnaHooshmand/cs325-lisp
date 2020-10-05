(defun reduce-tree (f tree &optional init)
  (cond ((null tree)
         init)
        ((atom tree)
         (funcall f init tree))
        (T
         (reduce-tree f 
                      (cdr tree) 
                      (reduce-tree f (car tree) init)))
  )
)

(reduce-tree '+ '((1 (2 3) (((4)))) 6) 0)
(reduce-tree (lambda (x y) (cons y x)) '(a (b (c d) e) f))
(reduce-tree '+ 2 1)
