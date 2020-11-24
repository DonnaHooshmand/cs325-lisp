
(defun perfect-init (f min max)
  (cond ((= 0 (funcall f min))
         min)
        ((= 0 (funcall f max))
         max)
        (t nil)))

(defun bisect (f min max epsilon)
  (let* ((c (/ (+ min max) 2))
         (fc (funcall f c)))    
    (cond ((< (abs fc) epsilon)
           c)
          ((> fc 0)
           (bisect f min c epsilon))
          (t
           (bisect f c max epsilon)))))

(defun solve (f min max epsilon)
  (or (perfect-init f min max)
      (if (> (funcall f max) 0)
          (bisect f min max epsilon)
          (bisect f max min epsilon))))
