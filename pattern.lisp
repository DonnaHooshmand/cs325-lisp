
(char (symbol-name '?x) 0)

(defun match-p (x y &optional (blists '(nil)))
  (or (eql x y)
      (and (var-p x) (var-match ))
      (and (consp x)
           (consp y)
           (match-p (car x) (car y))
           (match-p (cdr x) (cdr y)))))

(defun var-p (x y)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun var-match (var obj blists)
  (cond ((null blists) nil)
        (t (let ((blist (car blists)))
             (let ((binding (assoc var blist)))
               (cond ((null binding)
                      (list (cons (list var obj) blist)))

                     ))))))
