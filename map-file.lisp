(defun map-stream (f str)
  (let ((eof (list 'eof)))
    (do ((line (read str nil eof)
               (read str nil eof)))
        ((eql line eof) nil)
        (funcall f line))))

(do ((line (read str nil eof)
           (read str nil eof)))
    ((eql line eof) nil)
    (funcall f line))


(defun map-file (f file)
  (with-open-file (str file :direction :input)
    (map-stream f str)))

(map-file #'print "~/.sbclrc")

(run-tests map-stream)
