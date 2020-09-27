
(defun has-number-p (s-exp)
  (if (consp s-exp)
      (some #'has-number-p s-exp)
      (numberp s-exp)))

(defun make-balance (initial-balance)
  (let (setq balance initial-balance)
    )
  (lambda (%optional val) 
    (if val
        ())))

;; This doesnt work yet
(defun make-balance (initial-balance)
  (let (setq balance initial-balance)
    (lambda (%optional val)
      (if val
          (incf balance val)
          balance))))

(member 'a '(b a ))
