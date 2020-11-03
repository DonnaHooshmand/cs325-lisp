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

(defun hyphenate (str &optional case)
  (do ((i 0 (1+ i))
       (prev-char '#\A (char str i))
       (l (length str))
       (res (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
      ((>= i l) (if (string-equal case "lower")
                    (string-downcase res)
                    (string-upcase res)))
    (with-output-to-string (s res)
      (hyphenate-next-char s prev-char (char str i)))))

(hyphenate "URL" )

(hyphenate "HelloWorld" :upper)
(hyphenate "urlID" :upper)
