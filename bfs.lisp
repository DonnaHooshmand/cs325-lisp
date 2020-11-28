(in-package :cs325-user)

; Original BFS
(defun bfs (end queue net) 
  (if (null queue) 
      nil
      (let ((path (car queue)))
        (let ((node (car path))) 
          (if (eql node end) 
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))


;; no catch throw
(defun new-paths (path node net)
  (mapcan (lambda (n)
            (unless (member n path) (list (cons n path))))
          (cdr (assoc node net))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net &optional (seen nil))
  (if (empty-queue-p queue)
      nil
      (let* ((path (car queue))
             (node (car path)))
        (cond ((member node seen)
               (bfs end (cdr queue) net seen))
              ((member end (assoc node net))
               (nreverse (cons end path)))
              (t
               (let* ((news (new-paths path node net))
                      (found (find-if (lambda (p) (eql (car p) end)) news)))
                 (or (nreverse found) 
                     (bfs end
                          (append (cdr queue) news)
                          net
                          (cons node seen)))))))))

;; catch and throw
(defun shortest-path (start end net)
  (catch 'abort 
         (bfs end (list (list start)) net)))

(defun new-paths (path node end net)
  (mapcan (lambda (n)
            (when (eql n end)
              (throw 'abort (nreverse (cons n path))))
            (unless (member n path) (list (cons n path))))
          (cdr (assoc node net))))

(defun bfs (end queue net) 
  (if (null queue) 
      nil
      (let ((path (car queue)))
        (let ((node (car path))) 
          (if (eql node end) 
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node end net))
                   net))))))

(run-tests shortest-path)

(shortest-path 'a 'd '((a b c) (b c) (c d))) ; '(a c d) 
(shortest-path 'a 'c '((a b c) (b c) (c d))) ; '(a c) 
(shortest-path 'a 'd '((a b c) (b e) (e c) (c d))) ; '(a c d) 
(shortest-path 'a 'f '((a b c) (b c) (c e) (e f))) ; '(a c e f) 
(shortest-path 'a 'f '((a b c) (b c f) (c e) (e f))) ; '(a b f) 
(shortest-path 'a 'f '((a b c) (b c) (c d) (e f))) ; '()  
(shortest-path 'a 'f '((a b c d) (b e f))) ; '(a b f) 

;; With cycles
(shortest-path 'a 'd '((a b c) (b a c) (c d))) ; '(a c d) 
(test-path '(a c) 'a 'c '((a b c) (b c) (c a d)))
(test-path '() 'a 'c '((a b) (b a) (c)))
(test-path '() 'a 'f '((a b c) (b a c) (c d) (e f)))
(test-path '() 'a 'f '((a b c) (b c) (c b) (e f)))
(test-path '(a b c e f) 'a 'f '((a b) (b c d) (c e) (d a) (e f)))

(trace bfs)
(untrace)
(trace new-paths)

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

(run-tests shortest-path)
