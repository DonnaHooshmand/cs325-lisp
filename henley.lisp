(defparameter *words* (make-hash-table :size 10000))
(defconstant maxword 100)

(defun read-stream (s func)
  (do* ((buffer (make-string maxword))
        (pos 0)
        (c (read-char s nil :eof) (read-char s nil :eof))
        (p (punc c) (punc c)))
    ((eql c :eof))
    (cond ((or (alpha-char-p c) (char= c #\'))
           (setf (aref buffer pos) c)
           (incf pos))
          (t
           (unless (zerop pos)
             (funcall func (intern (string-downcase
                                     (subseq buffer 0 pos))))
             (setf pos 0))
           (when p (funcall func p))))))

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((see (make-see)))
      (read-stream s see)))
  (hash-table-count *words*))

(defun punc (c) (case c (#\. '|.|) (#\, '|,|) (#\; '|;|) (#\! '|!|) (#\? '|?|)))

(defun make-see ()
  (let ((prev '|.|))
    (lambda (symb)
      (let ((pair (assoc symb (gethash prev *words*))))
        (if (null pair)
            (push (cons symb 1) (gethash prev *words*))
            (incf (cdr pair))))
      (setf prev symb))))

; iterative
(defun generate-text (n) 
  (do* ((i n (1- i))
        (prev '|.| next)
        (next (random-next prev) (random-next prev)))
    ((zerop i) (terpri))
    (format t "~A " next)))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))
