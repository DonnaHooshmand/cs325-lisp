
(defun map-stream (f str)
    (do ((line (read str nil eof)
               (read str nil eof))
         (res nil (cons (funcall f line) res)))
        ((eql line eof) nil)))

(map-file #'list "~/.zshrc")

(defun map-file (f file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil eof)
               (read-line str nil eof))
         (res nil (cons (funcall f line) res)))
        ((eql line eof) nil))))

(defun ask-number ()
(format t "Please enter a number. ") (let ((val (read)))
(if (numberp val) val
(ask-number))))

(ask-number)
