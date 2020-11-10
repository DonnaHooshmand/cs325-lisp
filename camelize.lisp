(in-package :cs325-user)

(defun camelize (str &optional capitalize)
  (let  ((res (remove #\- (string-capitalize str))))
    (if capitalize
        res
        (format nil "~a~a" (char-downcase (char res 0)) (subseq res 1)))))

(camelize "hello-world" :capitalize)

(defun hyphenate-next-char (s prev curr)
  (cond ((and (lower-case-p prev) (upper-case-p curr))
         (format s "-~a" curr))
        (t
         (format s "~a" curr))))

(defun hyphenate-stream (str)
  (with-output-to-string (s)
    (do ((i 0 (1+ i))
         (prev-char '#\A (char str i))
         (l (length str)))
        ((>= i l) nil)
      (hyphenate-next-char s prev-char (char str i)))))

(defun hyphenate (str &optional case)
  (if (eql case "lower")
      (string-downcase (hyphenate-stream str))
      (string-upcase (hyphenate-stream str))))

(hyphenate "URL" )

(hyphenate "HelloWorld" :upper)
(hyphenate "urlID" :upper)
