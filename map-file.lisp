
(defun map-stream (f str)
  (let ((eof (list 'eof)))
    (do ((line (read str nil eof)
               (read str nil eof)))
        ((eql line eof) nil)
      (funcall f line))))

(defun map-file (f file)
  (with-open-file (str file :direction :input)
    (let ((eof (list 'eof)))
    (do ((line (read-line str nil eof)
               (read-line str nil eof))
         (res nil (cons (funcall f line) res)))
        ((eql line eof) nil)))))

(map-file #'print "~/.zshrc")
