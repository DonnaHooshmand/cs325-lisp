(in-package :cs325-user)

(defun max-min (vec &key (start 0) (end (length vec)))
  (if (>= start end)
      (values nil nil)
      (max-min-rec vec :start start :end end
                   :cmin (aref vec start) :cmax (aref vec start))))

(defun max-min-rec (vec &key start end cmin cmax)
  (if (>= start end)
      (values cmax cmin)
      (max-min-rec vec :start (1+ start) :end end
                   :cmin (min (aref vec start) cmin)
                   :cmax (max (aref vec start) cmax))))

(trace max-min)
(untrace)
(max-min #(1 2 3))
(max-min #(1 2 3 4 3  35  6 6 6))

  (assert-equal (values 4 1) (max-min #(1 2 3 4)))
  (assert-equal (values 10 1) (max-min #(3 1 8 2 10)))
  (assert-equal (values 10 10) (max-min #(10)))
  (assert-equal (values -5 -8) (max-min #(-5 -8)))
  (assert-equal (values 2 2) (max-min #(1 2 3 4) :start 1 :end 2))
  (assert-equal (values nil nil) (max-min #(1 2 3) :start 2 :end 2))
  (assert-equal (values 2 1) (max-min #(1 2 3) :end 2))
  (assert-equal (values nil nil) (max-min #()))
