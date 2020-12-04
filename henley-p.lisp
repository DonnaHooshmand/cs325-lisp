(load "henley")

(read-text "/Users/tigernie/code/lisp325/pg26.txt")
(generate-text 20)

(defun make-valid ()
  (let ((prev '|.|))
    (lambda (symb)
      (let ((pair (assoc symb (gethash prev *words*))))
        (when (null pair)
            (throw 'abort nil)))
      (setf prev symb))))

(defun henley-p (str)
  (catch 'abort
         (with-input-from-string (s str)
           (read-stream s (make-valid)))
         t))

(run-tests henley-p)

(henley-p "therefore on what the north ; mystical dance not informidable ! thus high above all embroiled , the anarch old")
(henley-p "how now brown cow")

(henley-p str)
(defparameter str (with-output-to-string (*standard-output*) (generate-text 20)))
