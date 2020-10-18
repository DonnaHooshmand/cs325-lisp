(in-package :cs325-user)

(defun max-min (vec &key (start 0) (end 1 endp))
  (cond ((or (= 0 (length vec)) (>= start end)) (values nil nil))
        (endp (max-min-rec vec :start start :end end
                           :min (aref vec start) :max (aref vec start)))
        (t (max-min-rec vec :start start :end (length vec)
                        :min (aref vec start) :max (aref vec start)))))

(defun max-min-rec (vec &key start end min max)
  (cond ((>= start end) (values max min))
        (t (max-min-rec vec :start (1+ start) :end end
                        :min (if (< (aref vec start) min)
                                 (aref vec start)
                                 min)
                        :max (if (> (aref vec start) max)
                                 (aref vec start)
                                 max)))))

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
