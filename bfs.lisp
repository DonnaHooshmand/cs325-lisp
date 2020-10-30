(in-package :cs325-user)

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

;; No catch throw
(defun shortest-path (start end net)
  (if (some (lambda (node) (eql node end)) (car net))
      (list start end)
      (bfs end (list (list start)) net)))

(defun bfs (end queue net &optional (seen nil))
  (if (empty-queue-p queue)
      nil
      (let* ((path (car queue))
             (node (car path)))
        (if (member node seen)
            (bfs end (cdr queue) net seen)
            (let* ((news (new-paths path node net))
                   (found (find-if (lambda (p) (eql (car p) end)) news)))
              (if found (reverse found) (bfs end
                                             (append (cdr queue) news)
                                             net
                                             (cons node seen))))))))

;; Catch and throw
(defun shortest-path (start end net)
  (if (some (lambda (node) (eql node end)) (car net))
      (list start end)
      (catch 'abort
             (bfs end (list (list start)) net))))

(defun bfs (end queue net &optional (seen nil))
  (if (empty-queue-p queue)
      nil
      (let* ((path (car queue))
             (node (car path)))
        (if (member node seen)
            (bfs end (cdr queue) net seen)
            (let ((news (new-paths path node net)))
              (mapcan (lambda (p) (when (eql (car p) end)
                                    (throw 'abort (reverse p)))) news)
              (bfs end
                   (append (cdr queue) news)
                   net
                   (cons node seen)))))))

(shortest-path 'a 'f '((a b c) (b a c) (c d) (e f)))
(shortest-path 'a 'f '((a b c) (b c f) (c e) (e f)))
(shortest-path 'a 'c '((a b c) (b c) (c d)))
(shortest-path 'a 'd '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (if (some (lambda (node) (eql node end)) (car net))
      (list start end)
      (bfs end (list (list start)) net)))

(trace bfs)
(untrace)
(trace new-paths)

(format t "~a ~a ~a ~a" queue path node end)

(test-path '(a c d) 'a 'd '((a b c) (b c) (c d)))

(test-path '(a c) 'a 'c '((a b c) (b c) (c d)))
(test-path '(a c d) 'a 'd '((a b c) (b e) (e c) (c d)))
(test-path '(a c e f) 'a 'f '((a b c) (b c) (c e) (e f)))
(test-path '(a b f) 'a 'f '((a b c) (b c f) (c e) (e f)))
(test-path '()  'a 'f '((a b c) (b c) (c d) (e f)))
(test-path '(a b f) 'a 'f '((a b c d) (b e f)))

;; With cycles
(test-path '(a c d) 'a 'd '((a b c) (b a c) (c d)))
(test-path '(a c) 'a 'c '((a b c) (b c) (c a d)))
(test-path '() 'a 'c '((a b) (b a) (c)))
(test-path '() 'a 'f '((a b c) (b a c) (c d) (e f)))
(test-path '() 'a 'f '((a b c) (b c) (c b) (e f)))
(test-path '(a b c e f) 'a 'f '((a b) (b c d) (c e) (d a) (e f)))

