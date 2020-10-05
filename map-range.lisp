(defun map-range (f x y)
  (if (> x y)
      (loop for i from x downto (1+ y)
            collect (funcall f i))
      (loop for i from x upto (1- y)
            collect (funcall f i))))

(map-range #'identity 1 10)
(map-range #'identity 10 1)

(defun find-range (f x y)
  (if (> x y)
      (loop for i from x downto (1+ y)
            when (funcall f i)
            do (return i)
            finally (return nil))
      (loop for i from x upto (1- y)
            when (funcall f i) 
            do (return i)
            finally (return nil))))

(find-range #'evenp 1 2)

(defun every-range (f x y)
  (cond ((> x y)
         (and (funcall f x)
              (every-range f (1- x) y)))
        ((< x y)
         (and (funcall f x)
              (every-range f (1+ x) y)))
        (t t)))

(defun reduce-range (f x y &optional init)
  (cond ((> x y)
         (reduce-range f (1- x) y (funcall f init x)))
        ((< x y)
         (reduce-range f (1+ x) y (funcall f init x)))
        (t init)))

(map-range 'identity 10 1)
(find-range (lambda (n) (= (mod 35 n) 0)) 2 10)
(every-range (lambda (n) (> (mod 37 n) 0)) 2 36)
(reduce-range (lambda (v x) (cons x v)) 1 5)
