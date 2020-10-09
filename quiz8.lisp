(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code from Quiz 3, Quiz 7, and mini-lecture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun foo (x)
  (cons x 
        (mapcan (lambda (fact) (baz x fact))
                *facts*)))

(defun baz (x fact)
  (and (eql x (car fact))
       (eql 'isa (cadr fact))
       (foo (caddr fact))))

(defun matches (subj pred fact)
  (and (eql (car fact) subj)
       (eql (cadr fact) pred)))

(defun answer (subj pred facts)
  (caddr
   (find-if (lambda (fact) (matches subj pred fact))
            facts)))

(defun infer (subj pred facts)
  (some (lambda (abst)
          (answer abst pred facts))
        (remove-duplicates (foo subj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the Pedalo example knowledge base for Quiz 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *facts*
  '((day-boat isa boat)
    (day-boat nav-zone 5)
    (wheel-boat isa boat)
    (wheel-boat nav-zone 100)
    (engineless-boat isa day-boat)
    (small-multi-hull-boat isa day-boat)
    (pedal-wheel-boat isa engineless-boat)
    (pedal-wheel-boat isa wheel-boat)
    (small-catamaran isa small-multi-hull-boat)
    (pedalo isa pedal-wheel-boat)
    (pedalo isa small-catamaran)
    ))


(defun show-all ()
  (let ((concepts '(boat day-boat wheel-boat engineless-boat small-multi-hull-boat
                    pedal-wheel-boat small-catamaran pedalo)))
    (dolist (x concepts)
      (format t "~%Nav-zone for ~S: ~S" x (infer x 'nav-zone *facts*)))))

(show-all)
