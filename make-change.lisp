
(defun make-change (total &optional (lst '(25 10 5 1)))
    (do ((ll lst (cdr ll))
         (curr total (mod curr (car ll)))
         (res nil (cons 
                    (values (floor curr (car ll)))
                      res)))
        ((endp ll) (values-list (nreverse res)))))

(make-change 50)
(make-change 99)
(make-change 65)

