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


; paths - A list of paths, where each path is a list of states, from newest to oldest
; pred - A predicate that takes a state and returns true if the state is a goal state
; gen - A predicate that takes a path and returns the list of states that can be reached legally from the first state in the path, given the other states in the path.

(defun bfs (paths pred gen)
  (if (null paths)
      nil
      (let ((path (car paths)))
        (if (funcall pred (car path))
            path
            (bfs (append (cdr paths)
                         (mapcar (lambda (s)
                                   (cons s path))
                                 (funcall gen path)))
                 pred
                 gen)))))

(defun shortest-path (start end net)
  (nreverse (bfs (list (list start))
                 (lambda (state) 
                   (eql state end))
                 (lambda (path)
                   (mapcan (lambda (node)
                             (unless (member node path)
                               (list node)))
                           (cdr (assoc (car path) net)))))))


(shortest-path 'a 'f '((a b c) (b c) (c e) (e f)))
(test-path '(a c e f) 'a 'f '((a b c) (b c) (c e) (e f)))

(run-tests shortest-path)

(cdr (assoc 'b '((a b c) (b c) (c e) (e f))))

(defparameter net-test '((a b c) (b c) (c e) (e f)))
(defparameter net-test '((a b c) (b a c) (c d)))
(funcall
  (lambda (path)
    (mapcan (lambda (node)
              (unless (member node path)
                (list node))
              )
            (cdr (assoc (car path) net-test))))
  '(b a)
  )


(test-path '(a c e f) 'a 'f '((a b c) (b c) (c e) (e f)))
(run-tests shortest-path)
