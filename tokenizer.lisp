(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
      ((not (next-token-p tr)) (nreverse l)))))

(defclass tokenizer ()
  ((str :accessor tok-str
        :initarg :str)
   (delim :accessor tok-delim
          :initarg :delim
          :initform #\space)
   (len :accessor tok-len
        :initarg :len
        :initform 0)
   (idx :accessor tok-idx
        :initarg :idx
        :initform 0)))

(defmethod next-token-p ((tr tokenizer))
  (cond ((eql (tok-delim tr) #\space)
         ; Delimiter is a space
         (setf (tok-idx tr) (next-after-delim tr))
         (< (tok-idx tr) (tok-len tr)))
        ((and (= (tok-idx tr) (tok-len tr))
              (or (not (eql (tok-delim tr) #\space))
                  (eql (tok-delim tr) (char (tok-str tr) (1- (tok-idx tr))))))
         ; idx=len and delim!=space
         ; idx=len and str[idx] = delimiter
         t)
        (t (< (tok-idx tr) (tok-len tr)))))

(defmethod next-after-delim ((tr tokenizer))
  (do ((i (tok-idx tr) (1+ i)))
      ((or (>= i (tok-len tr))
           (not (eql (tok-delim tr) (char (tok-str tr) i))))
       i)))

(defmethod next-token ((tr tokenizer))
  (if (eql (tok-delim tr) #\space)
      (setf (tok-idx tr) (next-after-delim tr)))
  (cond ((>= (tok-idx tr) (tok-len tr))
         ; idx >= len
         (incf (tok-idx tr))
         "")
        ((eql (tok-delim tr) (char (tok-str tr) (tok-idx tr)))
         (incf (tok-idx tr))
         "")
        ((and (not (eql (tok-delim tr) #\space))
              (= (tok-idx tr) 0) (eql (tok-delim tr) (char (tok-str tr) 0)))
         (incf (tok-idx tr))
         "")
        (t
         (do ((i (tok-idx tr) (1+ i))
              (begin (tok-idx tr)))
             ((or (>= i (tok-len tr))
                  (eql (char (tok-str tr) i) (tok-delim tr)))
              (setf (tok-idx tr) (1+ i))
              (subseq (tok-str tr) begin i))))))

(defun make-tokenizer (str &optional (delim #\space))
  (make-instance 'tokenizer
                 :str str
                 :delim delim
                 :len (length str)))


(split-string "" #\space)

(split-string " now  is the+time  " #\space)

(defparameter tt (make-tokenizer ",," #\,))
(next-token tt)
(next-token-p tt)
(tok-idx tt)
(tok-len tt)
(next-token (make-tokenizer "now "))
(next-after-delim (make-tokenizer "  "))
(next-token (make-tokenizer "   hello world"))
(split-string "Now is the time ")


(run-tests tokenizer)
(define-test tokenizer 
  (assert-equal '("now" "is" "the" "time")
                 (split-string " now  is the time  "))
  (assert-equal '("12" "132" "abc")
                (split-string "12,132,abc" #\,))
  (assert-equal '("" "12" "132" "" "abc")
                (split-string ",12,132,,abc" #\,))
  (assert-equal '("" "")
                (split-string "," #\,))
  
  (assert-equal () (split-string "  "))
  (assert-equal '("") (split-string "" #\,))
  (assert-equal '("  ") (split-string "  " #\,))
  )

(next-token-p (make-tokenizer "" #\,))
