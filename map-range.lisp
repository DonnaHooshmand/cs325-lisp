(trace range)
(map-range #'identity 1 10)
(map-range #'identity 10 1)

(defun map-range (f x y)
  (do ((increment (if (> x y) -1 1))
       (i x (+ i increment))
       (ll nil (cons 
                 (funcall f i)
                 ll)))
      ((eql i y) (nreverse ll))))

(defun find-range (f x y)
  (do ((increment (if (> x y) -1 1))
       (i x (+ i increment)))
      ((or (eql i y) (funcall f i))
       (if (eql i y)
           nil
           i))))

(defun every-range (f x y)
  (do ((increment (if (> x y) -1 1))
       (i x (+ i increment)))
      ((or (eql i y) (not (funcall f i)))
       (eql i y))))

(defun reduce-range (f x y &optional init)
  (do ((increment (if (> x y) -1 1))
       (i x (+ i increment))
       (acc init
            (funcall f acc i)))
      ((eql i y) acc)))

(map-range 'identity 10 1)
(find-range (lambda (n) (= (mod 35 n) 0)) 2 10)
(every-range (lambda (n) (> (mod 37 n) 0)) 2 36)
(reduce-range (lambda (v x) (cons x v)) 1 5)
(reduce-range '+ 1 5 0)
