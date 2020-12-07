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
  (not (null (tok-idx tr))))

(defmethod get-next-token-start ((tr tokenizer))
  (if (eql (tok-delim tr) #\space)
      (do ((i (tok-idx tr) (1+ i)))
          ((or (>= i (tok-len tr))
               (not (eql (tok-delim tr) (char (tok-str tr) i))))
           (if (>= i (tok-len tr))
               nil
               i)))
      (if (>= (tok-idx tr) (tok-len tr))
          nil
          (tok-idx tr))))

(defmethod get-token-end ((tr tokenizer))
  (if (next-token-p tr)
      (do ((i (tok-idx tr) (1+ i)))
          ((or (>= i (tok-len tr))
               (eql (char (tok-str tr) i) (tok-delim tr)))
           i))
      nil))

(defmethod next-token ((tr tokenizer))
  (if (or (null (next-token-p tr)) (>= (tok-idx tr) (tok-len tr)))
      nil
      (let* ((end (get-token-end tr))
             (new-token (subseq (tok-str tr) (tok-idx tr) end)))
        (setf (tok-idx tr) (1+ end))
        (setf (tok-idx tr) (get-next-token-start tr))
        new-token)))

(defun make-tokenizer (str &optional (delim #\space))
  (let ((tr (make-instance 'tokenizer
                           :str str
                           :delim delim
                           :len (length str))))
    (setf (tok-idx tr) (get-next-token-start tr))
    tr))

(run-tests tokenizer)

(split-string "" #\space)
(split-string " now  is the+time  " #\space)
(defparameter tt (make-tokenizer ",," #\,))
(defparameter tt (make-tokenizer "now is the tim" #\space))

(next-token tt)
(next-token-p tt)
(tok-idx tt)
(tok-len tt)
(get-token-end tt)
(get-next-token-start tt)

(tok-idx tt)
(tok-len tt)
(next-token (make-tokenizer "now "))
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
