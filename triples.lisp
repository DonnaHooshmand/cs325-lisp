(in-package :cs325-user)
(load "~/quicklisp/local-projects/cs325/triples/graph-search.lisp")

(ensure-data "~/quicklisp/local-projects/cs325/triples/movie-triples.txt")

(defun match-query (query triple &optional (blists (list nil)))
  (cond ((null blists) nil)
        ((null query) blists)
        (t (match-query (cdr query) (cdr triple)
                        (match-item (car query) (car triple) blists)))))

(defun prune-query (query triple &optional (blist (list nil)))
  (cond ((null blist) nil)
        ((null query) blists)
        (t (match-query ))
        ))

(defun query-search (query &optional (blists (list nil)))
  (cond ((null blists) nil)
        ((eql :not (car query))
         ())
        (t (mapcan (lambda (triple)
                     (match-query query triple blists))
                   *triples*))))

(graph-search '((?movie movie ?year)))
(graph-search '((?movie movie ?year) (:not (?movie actor tom_cruise))))
