(in-package :cs325-user)

(defun shortest-path (start end net)
  (catch 'abort
         (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  ;(if (empty-queue-p queue)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (throw 'abort (reverse path))
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(trace bfs)
(untrace)
(trace new-paths)

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(shortest-path 'a 'd '((a b c) (b c) (c d)))

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

