;;;; By Jess
;;;; http://grok-code.com

;;; Some clean-up by Rainer Joswig, joswig@lisp.de, http://lispm.dyndns.org/
;;; More clean-up by Tiger Nie

(defparameter *debug* nil)
(defparameter *test-know* nil)  

(defparameter *vocab* (make-hash-table :test 'equalp)) ; holds world knowledge
(defparameter *literal-list* (list nil)) ;; this is the key list for *vocab* -- the two data structures should be kept consistent

(defparameter *punchline* (make-hash-table :test 'equal))  ; holds punchlines of jokes that have already been told

(defstruct word-prop literal relation homophone POS anim art)

;; iterates through the vocabulary, tries to answer a joke for each pair of vocabulary words
(defun generate ()
  (cond (*test-know* (seed-knowledge-test))
        (t (seed-knowledge)))
  ; answer jokes for M_1 N_1, M_2 N_2 pairs, where N is a noun, M is a modifier N_1 != N_2 
  ; modifiers may be null
  (do* ((literals (cdr *literal-list*) (cdr literals))
        (word1 (car literals) (car literals)))
    ((null (cdr literals))) ; break condition
    (when (is-POS 'n (gethash word1 *vocab*))
      (dolist (word2 (cdr literals))
        (when (is-POS 'n (gethash word2 *vocab*))
          (do* ((literals-m (append *literal-list* (list nil)) (cdr literals-m))
                (mod1 (car literals-m) (car literals-m)))
            ((null (cdr literals-m))) ; break condition
            (when (and (or (null mod1) (is-POS 'm (gethash mod1 *vocab*)))
                       (anim-match word1 mod1))  ; animated qualities have to match -- "serious lemon" is not allowed
              (dolist (mod2 (append (cdr *literal-list*) (list nil)))
                (when (and (or (null mod2) (is-POS 'm (gethash mod2 *vocab*)))
                           (anim-match word2 mod2)) ; animated qualitites
                  (let ((answer (answer-joke word1 word2 mod1 mod2)))
                    (when answer (print-joke word1 word2 mod1 mod2 answer))))))))))))


;; takes strings or word-props returns true if they have they same animated quatlity   
(defun anim-match (str1 str2)
  (let ((w-prop1 (cond ((word-prop-p str1) str1)
                       (t (gethash str1 *vocab*))))
        (w-prop2 (cond ((word-prop-p str2) str2)
                       (t (gethash str2 *vocab*)))))
    (or (null w-prop1) (null w-prop2) 
        (eql 'b (word-prop-anim w-prop1)) (eq 'b (word-prop-anim w-prop2)) 
        (eql (word-prop-anim w-prop1) (word-prop-anim w-prop2)))))

; (critique-file "/Users/tigernie/code/lisp325/clean-jokes.lisp")

;; returns a string that will answer the joke, if possible
;; word1 and word2 are strings
;; mod1 and mod2 are strings or nil if no modifier
(defun answer-joke (word1 word2 mod1 mod2)

  (let ((derive-word1 (derive-words word1)) 
        (derive-word2 (derive-words word2))
        (derive-mod1 (derive-words mod1))
        (derive-mod2 (derive-words mod2))
        (answer nil)
        (answer-val 0)   ; heuristic for how good the joke is -- funniest is 10
        (threshold 5))   ; jokes with answer-val strictly less than threshold arent considered funny, and wont be returned

    (when *debug* (format t "answer-joke: ~O ~O and ~O ~O~%" mod1 word1 mod2 word2))
    ; no modifiers
    (cond ((and (null mod1) (null mod2)) 
           (dolist (d1 derive-word1)
             (dolist (d2 derive-word2)
               (let ((a (make-compound (word-prop-literal d1) (word-prop-literal d2))))
                 (when a 
                   (cond ((is-POS 'm a) 
                          (setq answer (format nil "I dont know, but it's ~O" (word-prop-literal a)))) 
                         ((is-POS 'n a) 
                          (setq answer (word-prop-literal a))))
                   (setq answer-val 10)))  ; found answer with N compound

               (when (and (> 8 answer-val) (>= 8 threshold) 
                          (member (word-prop-literal d1) (mapcar 'word-prop-literal (word-prop-homophone d2)) :test 'string-equal))
                 (let ((ans-prop (cond ((is-POS 'x d1) d1)
                                       ((is-POS 'x d2) d2)
                                       ((is-POS 'b d1) d1)
                                       ((is-POS 'b d2) d2)
                                       ((is-POS 'm d1) d1)
                                       ((is-POS 'm d2) d2)
                                       (t d1)))
                       (ans-phrase (if (or (is-POS 'm d1) (is-POS 'm d2)) 'm nil)))
                   (cond (ans-phrase (setq answer (format nil "I dont know, but it's ~O" (word-prop-literal ans-prop))))
                         (t (setq answer (word-prop-literal ans-prop)))))
                 (setq answer-val 8))))           ; found an answer where the 2 derived words are a homophone pair

           (when (and (> 3 answer-val) (>= 3 threshold)) 
             (setq answer (make-substring-word word1 word2 :POS 'n))
             (when answer (setq answer-val 3)))  ; found answer N with a substring match

           (when (and (> 3 answer-val) (>= 3 threshold))
             (dolist (d1 derive-word1)
               (dolist (d2 derive-word2)
                 (cond ((and (is-POS 'm d1) (is-POS 'n d2))
                        (setq answer (format nil "~O ~O" (word-prop-literal d1) (word-prop-literal d2)))
                        (setq answer-val 3))
                       ((and (is-POS 'm d2) (is-POS 'n d1))
                        (setq answer (format nil "~O ~O" (word-prop-literal d2) (word-prop-literal d1)))
                        (setq answer-val 3)))))))  ; found answer with an MN
          ; 1 modifier 
          ((or (null mod2) (null mod1))

           (when (null mod1)
             (rotatef mod1 mod2)
             (rotatef word1 word2)
             (rotatef derive-mod1 derive-mod2)
             (rotatef derive-word1 derive-word2))

           (when (and (> 7 answer-val) (>= 7 threshold))  
             (dolist (d derive-word2)
               (dolist (d-word (append (if (gethash word1 *vocab*) (list (gethash word1 *vocab*)) nil)
                                       derive-word1))
                 (when (make-compound (word-prop-literal d) (word-prop-literal d-word))
                   (dolist (d-mod (append (if (gethash mod1 *vocab*) (list (gethash mod1 *vocab*)) nil)
                                          derive-mod1))
                     (when (make-compound (word-prop-literal d) (word-prop-literal d-mod))
                       (let ((a1 (make-compound (word-prop-literal d) (word-prop-literal d-word)))
                             (a2 (make-compound (word-prop-literal d) (word-prop-literal d-mod))))
                         (cond ((and (is-POS 'm a1) (is-POS 'n a2))
                                (setq answer (format nil "~O ~O" (word-prop-literal a1) (word-prop-literal a2)))
                                (setq answer-val 8))
                               ((and (is-POS 'm a2) (is-POS 'n a1))
                                (setq answer (format nil "~O ~O" (word-prop-literal a2) (word-prop-literal a1)))
                                (setq answer-val 8))
                               ((and (is-POS 'm a1) (is-POS 'm a2))
                                (setq answer (format nil "I dont know, but it's ~O and ~O" 
                                                     (word-prop-literal a1) (word-prop-literal a2)))
                                (setq answer-val 8))
                               ((and (is-POS 'n a1) (is-POS 'n a2))
                                (setq answer (format nil "~O and ~O" (word-prop-literal a1) (word-prop-literal a2)))
                                (setq answer-val 8)))))))))))

          ; 2 modifiers
          (t
            (let ((a1 (make-substring-word word1 word2 :POS 'm))
                  (a2 (make-substring-word mod1 mod2 :POS 'n)))
              (when (and a1 a2 (anim-match (gethash a1 *vocab*) (gethash a2 *vocab*))
                         (<= (length word1) (length word2)) (<= (length mod1) (length mod2))) 
                (setq answer (format nil "~O ~O" a1 a2))
                (setq answer-val 8)))  ; made M N, both formed with substrings

            (when (and (> 8 answer-val) (>= 8 threshold))
              (let ((a1 (make-substring-word word1 word2 :POS 'n))
                    (a2 (make-substring-word mod1 mod2 :POS 'm)))
                (when (and a1 a2 (anim-match (gethash a1 *vocab*) (gethash a2 *vocab*))
                           (<= (length word1) (length word2)) (<= (length mod1) (length mod2)))
                  (setq answer (format nil "~O ~O" a2 a1))
                  (setq answer-val 8))))  ; made M N, both formed with substrings

            (when (and (> 8 answer-val) (>= 8 threshold))
              (let ((ans-list1 nil)
                    (ans-list2 nil))
                (dolist (d derive-word1)
                  (dolist (m derive-mod1)
                    (setq ans-list1 (append ans-list1 (let ((tmp (make-compound (word-prop-literal d) (word-prop-literal m))))
                                                        (if tmp (list tmp) nil))))))  
                (dolist (d derive-word2)
                  (dolist (m derive-mod2)
                    (setq ans-list2 (append ans-list2 (let ((tmp (make-compound (word-prop-literal d) (word-prop-literal m))))
                                                        (if tmp (list tmp) nil))))))
                (when (and ans-list1 ans-list2)

                  (dolist (a1 ans-list1)
                    (dolist (a2 ans-list2)
                      (cond ((and (is-POS 'm a1) (is-POS 'm a2))
                             (setq answer (format nil "I dont know, but its ~O and ~O" 
                                                  (word-prop-literal a1) (word-prop-literal a2)))
                             (setq answer-val 8))
                            ((and (is-POS 'n a1) (is-POS 'm a2) (anim-match a1 a2))
                             (setq answer (format nil "~O ~O" (word-prop-literal a2) (word-prop-literal a1)))
                             (setq answer-val 8))
                            ((and (is-POS 'm a1) (is-POS 'n a2) (anim-match a1 a2))
                             (setq answer (format nil "~O ~O" (word-prop-literal a1) (word-prop-literal a2)))
                             (setq answer-val 8))))))))))


    ; the joke loses "funny points" if the answer and question contain the same word
    (when (substring-p word1 answer)
      (decf answer-val 4))
    (when (substring-p word2 answer)
      (decf answer-val 4))
    (when (substring-p mod1 answer)
      (decf answer-val 4))
    (when (substring-p mod2 answer)
      (decf answer-val 4))

    ; it also loses points if the punchline has been used before
    (when (gethash answer *punchline*) 
      (setq answer-val (- answer-val (* 4 (gethash answer *punchline*)))))

    ; only return joke if it is funny enough
    (when (>= answer-val threshold)
      (add-punchline answer) ; record the punchline so it is less likely to be used again

      (when (not (null mod1)) ; record the elements of the question so we dont get more jokes with the question and the punchline switched
        (add-punchline (format nil "~O ~O" mod1 word1)))
      (when (not (null mod2))
        (add-punchline (format nil "~O ~O" mod2 word2)))

      answer)))  

;; adds the the punchline 
(defun add-punchline (str)
  (incf (gethash str *punchline* 0)))

;; returns t if str1 is contained in str2 
;; returns nil otherwise (returns nil if str1 is nil)
(defun substring-p (str1 str2)
  (and str1 (search str1 str2)))

;; returns t if str1 starts with str2     
(defun starts-with (str1 str2)
  (let ((l1 (length str1))
        (l2 (length str2)))
    (do ((i 0 (1+ i)))
        ((= l2 i) t) ; break
        (when (or (>= i l1) (not (eql (aref str2 i) (aref str1 i))))
          (return nil)))))

;; arg words should be strings  
;; returns a word-prop formed by combining the two arg words
;; the word-prop-literal may contain words that are homophones of known words.  
(defun make-compound (word1 word2 &key POS)
  (let ((ho-list1 (cons word1
                        (if (gethash word1 *vocab*) 
                            (mapcar 'word-prop-literal 
                                    (word-prop-homophone (gethash word1 *vocab*)))
                            nil)))
        (ho-list2 (cons word2
                        (if (gethash word2 *vocab*) 
                            (mapcar 'word-prop-literal 
                                    (word-prop-homophone (gethash word2 *vocab*)))
                            nil)))
        (answer nil))
    (when (and (not (null (gethash word2 *vocab*)))
               (is-POS POS (gethash word2 *vocab*))) 
      (dolist (h1 (cdr ho-list1))
        (when (and (starts-with word2 h1) (> (length word2) (length h1)))
          (cond ((char= (aref (subseq word2 (length h1)) 0) #\space)  ; there is a space at the break point
                 (setq answer (make-word-prop :literal (format nil "~O~O" word1 (subseq word2 (length h1)))
                                              :POS (word-prop-POS (gethash word2 *vocab*))
                                              :anim (word-prop-anim (gethash word2 *vocab*)))))
                (t 
                 (setq answer (make-word-prop :literal (format nil "~O-~O" word1 (subseq word2 (length h1)))
                                              :POS (word-prop-POS (gethash word2 *vocab*))
                                              :anim (word-prop-anim (gethash word2 *vocab*)))))))))

    (when answer (return-from make-compound answer))

    (when (and (not (null (gethash word1 *vocab*)))
               (is-POS POS (gethash word1 *vocab*))) 
      (dolist (h2 (cdr ho-list2))
        (when (and (starts-with word1 h2) (> (length word1) (length h2)))
          (cond ((char= (aref (subseq word1 (length h2)) 0) #\space) ; there is a space at the break point
                 (setq answer (make-word-prop :literal (format nil "~O~O" word2 (subseq word1 (length h2)))
                                              :POS (word-prop-POS (gethash word1 *vocab*))
                                              :anim (word-prop-anim (gethash word1 *vocab*)))))
                (t 
                 (setq answer (make-word-prop :literal (format nil "~O-~O" word2 (subseq word1 (length h2)))
                                              :POS (word-prop-POS (gethash word1 *vocab*))
                                              :anim (word-prop-anim (gethash word1 *vocab*)))))))))

    (dolist (h1 ho-list1)
      (dolist (h2 ho-list2)
        (if answer
            (return-from make-compound answer)
            (let ((hash nil))

              (setq hash (gethash (format nil "~O and ~O" h1 h2) *vocab*))
              (when (and hash (is-POS POS hash))
                (setq answer (make-word-prop :literal (format nil "~O and ~O" word1 word2)
                                             :POS (word-prop-POS hash)
                                             :anim (word-prop-anim hash))))

              (setq hash (gethash (format nil "~O and ~O" h2 h1) *vocab*))
              (when (and hash (is-POS POS hash))
                (setq answer (make-word-prop :literal (format nil "~O and ~O" word2 word1)
                                             :POS (word-prop-POS hash)
                                             :anim (word-prop-anim hash))))

              (setq hash (gethash (format nil "~O~O" h1 h2) *vocab*))
              (when (and hash (is-POS POS hash))
                (setq answer (make-word-prop :literal (format nil "~O~O" word1 word2)
                                             :POS (word-prop-POS hash)
                                             :anim (word-prop-anim hash))))

              (setq hash (gethash (format nil "~O~O" h2 h1) *vocab*))
              (when (and hash (is-POS POS hash))
                (setq answer (make-word-prop :literal (format nil "~O~O" word2 word1)
                                             :POS (word-prop-POS hash)
                                             :anim (word-prop-anim hash))))

              (setq hash (gethash (format nil "~O ~O" h1 h2) *vocab*))
              (when (and hash (is-POS POS hash))
                (setq answer (make-word-prop :literal (format nil "~O ~O" word1 word2)
                                             :POS (word-prop-POS hash)
                                             :anim (word-prop-anim hash))))

              (setq hash (gethash (format nil "~O ~O" h2 h1) *vocab*))
              (when (and hash (is-POS POS hash))
                (setq answer (make-word-prop :literal (format nil "~O ~O" word2 word1)
                                             :POS (word-prop-POS hash)
                                             :anim (word-prop-anim hash))))))))

    answer))


;; see if we can combine words by using substrings: cat + parrot = carrot
;; args and return value are strings
;; the suffix is always taken from the shorter word
(defun make-substring-word (word1 word2 &key POS)
  (let* ((small-str (cond ((<= (length word1) (length word2)) word1)
                          (t word2)))
         (big-str (cond ((<= (length word1) (length word2)) word2)
                        (t word1)))
         (suffix (let ((s (mapcan (lambda (x) (when (starts-with small-str x) (list x))) ; chair + parrot != carrot 
                                  '("thr" "th" "ch" "str" "st" "spr" "sp" "tr" "sc" "gr" "fl" "fr"))))
                   (cond (s (car s))
                         (t (aref small-str 0)))))
         (new-str (format nil "~O~O" suffix (string-left-trim (make-array 1 :initial-element (aref big-str 0)) big-str))))
    (if (and (gethash new-str *vocab*) (is-POS POS (gethash new-str *vocab*)) 
             (not (equal new-str small-str)) (not (equal new-str big-str)))  
        new-str
        nil)))


;; returns true if word-prop can be that kind of speech
(defun is-POS (POS word-prop)
  (or (null POS)
      (and (eq 'b POS) (not (eql 'x (word-prop-POS word-prop))))
      (and word-prop (eql POS (word-prop-POS word-prop)))))


; returns relations of the literal, and relations of the literal with common sufixes, 
; the return list is made of word-props
; the input is a string literal
(defun derive-words (literal)
  (cond ((null literal) nil)
        ((null (gethash literal *vocab*)) (add-suffix (make-word-prop :literal literal)))
        (t (append (word-prop-relation (gethash literal *vocab*))
                   (mapcan 'add-suffix (word-prop-relation (gethash literal *vocab*)))))))



;; use some common grammer rules to add suffixes to the literal, return a list of possible words 
(defun add-suffix (word-prop)
  (if (is-POS 'n word-prop)
      (list (make-word-prop :literal (format nil "~Os" (word-prop-literal word-prop))
                            :POS (word-prop-POS word-prop)
                            :anim (word-prop-anim word-prop)))
      nil))


(defun print-joke (word1 word2 mod1 mod2 answer)
  (format t "WHAT DO YOU GET WHEN YOU CROSS ~A WITH ~A?" 
          (cond ((and (gethash word1 *vocab*) (eql t (word-prop-art (gethash word1 *vocab*))))  ; article
                 (cond (mod1 (string-upcase (add-article (format nil "~O ~O" mod1 word1))))         ; mod
                       (t (string-upcase (add-article word1)))))                                    ; no mod
                (t                                                                              ; no article
                 (cond (mod1 (string-upcase (format nil "~O ~O" mod1 word1)))                       ; mod
                       (t (string-upcase word1)))))                                               ; no mod
          (cond ((and (gethash word2 *vocab*) (eql t (word-prop-art (gethash word2 *vocab*))))  ; article
                 (cond (mod2 (string-upcase (add-article (format nil "~O ~O" mod2 word2))))         ; mod
                       (t (string-upcase (add-article word2)))))                                    ; no mod
                (t                                                                              ; no article
                 (cond (mod2 (string-upcase (format nil "~O ~O" mod2 word2)))                       ; mod
                       (t (string-upcase word2))))))                                               ; no mod

  (format t "~%~A~%~%" answer))

(defun add-article (str)
  (cond ((is-vowel (char str 0)) (format nil "an ~O" str))
        (t (format nil "a ~O" str))))

(defun is-vowel (char)
  (member char '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U )))

;; Add some knowledge, so we can turn it into jokes
(defun seed-knowledge-test ()
  (add-relation "parrot" "polly" :anim1 t :anim2 t)
  (add-relation "cat" "puss" :anim1 t :anim2 t)
  (add-word "carrot")
  (add-word "super" :POS 'm :anim 'b)
  (add-word "dupe" :POS 'm :anim 'b)
  (add-word "duper" :POS 'm :anim 'b)

  )

;; Add some knowledge, so we can turn it into jokes
(defun seed-knowledge ()
  (add-relation "cheetah" "fast" :POS2 'm :anim1 t :anim2 t)
  (add-relation "cheetah" "spots" :anim1 t)
  (add-relation "elephant" "trunk" :anim1 t)
  (add-relation "hamburger" "food")
  (add-relation "hamburger" "meat" :art2 'f)
  (add-relation "dance" "ball" :anim2 'b)
  (add-relation "galaxy" "star" :anim2 'b)
  (add-relation "murderer" "killer" :anim1 t :anim2 t)
  (add-relation "toad" "warts" :anim1 t :art2 'f)
  (add-relation "strawberry" "jam" :anim2 'b :art2 'f)
  (add-relation "road" "traffic")
  (add-relation "bell" "ding")
  (add-relation "cow" "milk" :anim1 t :art2 'f)
  (add-relation "duck" "quack" :anim1 t :anim2 'b)
  (add-relation "bank" "dollars" :art2 'f)
  (add-relation "skunk" "scent" :anim1 t)
  (add-relation "ninja" "chops" :anim1 t :anim2 t)
  (add-relation "assistant" "aide" :anim1 t :anim2 'b)
  (add-relation "pig" "pork" :anim1 t)
  (add-relation "cat" "puss" :anim1 t :anim2 t)
  (add-relation "lemon" "sour" :POS2 'm :anim2 'b)
  (add-relation "rabbit" "hare" :anim1 t :anim2 t)
  (add-relation "lawn sprinkler" "spray")
  (add-relation "cemetery" "grave yard")
  (add-relation "mad" "crazy" :POS1 'm :POS2 'm :anim1 t :anim2 t)
  (add-relation "mad" "angry" :POS1 'm :POS2 'm :anim1 t :anim2 t)
  (add-relation "cap" "hat")
  (add-relation "ant" "bug" :anim1 t :anim2 t)
  (add-relation "aunt" "relative" :anim1 t :anim2 t)
  (add-relation "parent" "relative" :anim1 t :anim2 t)
  (add-relation "scared" "afraid" :POS1 'm :POS2 'm :anim1 t :anim2 t)
  (add-relation "rabbit" "hopping" :POS2 'm :anim1 t :anim2 t)
  (add-relation "rabid" "frothing" :POS1 'm :POS2 'm :anim1 t :anim2 t)
  (add-relation "cereal" "Frosted Flakes" :art2 'f)
  (add-relation "boy" "young man" :anim1 t :anim2 t)
  (add-relation "parrot" "polly" :anim1 t :anim2 t)
  (add-relation "flower" "poppy")
  (add-relation "jelly" "jam" :art1 'f :art2 'f)
  (add-relation "fish" "trout" :anim1 t :anim2 t)
  (add-relation "grave" "serious" :POS1 'b :POS2 'm :anim1 t :anim2 t)
  (add-relation "thief" "robber" :anim1 t :anim2 t)
  (add-relation "grave" "serious" :POS1 'b :POS2 'm :anim1 'b :anim2 t)
  (add-relation "music" "band" :anim2 'b :art1 'f)
  (add-relation "pea" "vegetable" :anim2 'b)
  (add-relation "centipede" "legs" :anim1 t :anim2 t :art2 'f)
  (add-relation "einstein" "relative" :anim1 t :anim2 t :art1 'f)
  (add-relation "jacket" "coat")
  (add-relation "fire" "hot" :POS2 'm) 
  (add-relation "electricity" "power" :art1 'f :art2 'f)
  (add-relation "pond" "lake")
  (add-relation "rain" "wet" :POS2 'm :art1'f)
  (add-relation "alcohol" "drunk" :POS2 'b :anim2 t)
  (add-relation "rabbit" "bunny" :anim1 t :anim2 t)
  (add-relation "car" "automobile")
  (add-relation "country" "nation")
  (add-relation "beach" "sand" :art2 'f)
  (add-relation "dog" "ruff" :anim1 t :POS2 'x)
  (add-relation "cat" "mew" :anim1 t :POS2 'x)
  (add-relation "cat" "purrr" :anim1 t :POS2 'x)
  (add-relation "sandpaper" "rough" :POS2 'm :art1 'f)
  (add-relation "radio" "music" :art2 'f)
  (add-relation "tune" "music" :art2 'f)
  (add-relation "chicken" "egg" :anim1 t)
  (add-relation "extraterrestrial" "alien" :anim1 'b :anim2 t) 
  (add-relation "finals" "exams" :art1 'f :art2 'f)
  (add-relation "port" "serial" :POS2 'm)

  (add-homophone "cereal" "serial" :POS2 'm)
  (add-homophone "hare" "hair" :anim1 t :art2 'f)
  (add-homophone "wars" "warts" :art1 'f :art2 'f)
  (add-homophone "cents" "scents" :art1 'f :art2 'f)
  (add-homophone "afraid" "frayed" :POS2 'm :anim1 t :anim2 'b)
  (add-homophone "parent" "apparent" :POS2 'm :anim1 t)
  (add-homophone "band" "banned" :POS2 'm :anim1 'b :anim2 t)
  (add-homophone "ant" "aunt" :anim1 t :anim2 t)
  (add-homophone "rabbit" "rabid" :POS2 'm :anim1 t :anim2 t)
  (add-homophone "puppy" "poppy" :anim1 t)
  (add-homophone "cracker" "quacker" :POS2 'm :anim2 t)
  (add-homophone "peas" "peace")
  (add-homophone "son" "sun")
  (add-homophone "tune" "toon" :POS2 'x)
  (add-homophone "witch" "which" :anim1 t :POS2 'x)
  (add-homophone "rough" "ruff" :POS1 'm :POS2 'x)
  (add-homophone "mew" "mu" :POS1 'x :POS2 'x)
  (add-homophone "purrr" "per" :POS1 'x :POS2 'x)
  (add-homophone "purrr" "pur" :POS1 'x :POS2 'x)
  (add-homophone "eggs" "ex" :POS2 'x :art1 'f)
  (add-homophone "ade" "aide" :POS1 'x :anim2 t)
  (add-homophone "aid" "aide" :POS1 'x :anim2 t)


  (add-word "canned" :POS 'm :anim 'b)
  (add-word "cow bell")
  (add-word "pig headed" :POS 'm :anim t)
  (add-word "star wars" :art 'f)
  (add-word "sour puss" :anim t)
  (add-word "traffic jam")
  (add-word "dingbat" :anim t)
  (add-word "milk and crackers" :art 'f)
  (add-word "carrot")
  (add-word "pork chops" :art 'f)
  (add-word "fast food" :art 'f)
  (add-word "dollars and cents" :art 'f)
  (add-word "lemonade" :art 'f)
  (add-word "hair spray" :art 'f)
  (add-word "mad hatter" :anim t)
  (add-word "hopping mad" :POS 'm :anim t)
  (add-word "serial killer" :anim t)
  (add-word "boycrazy" :POS 'm :anim t)
  (add-word "flower power")
  (add-word "jellyfish" :anim t)
  (add-word "grave robber" :anim t)
  (add-word "sour balls" :art 'f)
  (add-word "fastball")
  (add-word "sour milk" :art 'f)
  (add-word "bandaid")
  (add-word "peas and carrots" :art 'f)
  (add-word "peace and quiet" :art 'f)
  (add-word "war and peace" :art 'f)  
  (add-word "raincoat")
  (add-word "fireman")
  (add-word "pancake")
  (add-word "cupcake")
  (add-word "butterfly")
  (add-word "milkman" :anim t)
  (add-word "doorbell")
  (add-word "sunshine") 
  (add-word "bad" :POS 'm :anim t)
  (add-word "belly")
  (add-word "boat")
  (add-word "cake")
  (add-word "drunk" :POS 'b :anim t)
  (add-word "fake" :POS 'm)
  (add-word "jolly" :POS 'm :anim t)
  (add-word "mare" :anim t)
  (add-word "bugs bunny" :anim t :art 'f)
  (add-word "cartoon")
  (add-word "carnation")
  (add-word "sandwich")
  (add-word "snowball")
  (add-word "snowman")
  (add-word "excited" :POS 'm :anim t)
  (add-word "purple" :POS 'm :anim 'b)
  (add-word "person" :anim t)

  (sort *literal-list* #'(lambda(x y) (< (length x) (length y)))))



;; Everything below this line is the underlying data structure
;; -------------------------------------------------------------


(defun add-relation (literal1 literal2 &key POS1 POS2 anim1 anim2 art1 art2)
  (add-word literal1 :POS POS1 :anim anim1 :art art1)
  (add-word literal2 :POS POS2 :anim anim2 :art art2)
  (let ((word-prop1 (gethash literal1 *vocab*))
        (word-prop2 (gethash literal2 *vocab*)))
    (setf (word-prop-relation word-prop2) (remove-duplicates (nconc (list word-prop1) (word-prop-relation word-prop2)))) ;; ??
    (setf (word-prop-relation word-prop1) (remove-duplicates (nconc (list word-prop2) (word-prop-relation word-prop1))))))

(defun add-homophone (literal1 literal2 &key POS1 POS2 anim1 anim2 art1 art2) 
  (add-word literal1 :POS POS1 :anim anim1 :art art1)
  (add-word literal2 :POS POS2 :anim anim2 :art art2)
  (let ((word-prop1 (gethash literal1 *vocab*))
        (word-prop2 (gethash literal2 *vocab*)))
    (setf (word-prop-homophone word-prop2) (remove-duplicates (nconc (list word-prop1) (word-prop-homophone word-prop2))))
    (setf (word-prop-homophone word-prop1) (remove-duplicates (nconc (list word-prop2) (word-prop-homophone word-prop1))))))

;; Add a word to the vocab-list if it isnt already there
;; If the POS in unspecified, it defaults to 'n (noun)
;; Other acceptable parts of speech are 'm (modifier), 'b (both), 'x (neither)
;; Anim (animated) can be t (true) 'f (false) 'b (both), the default is 'f
;; art (article) can be t (true) or 'f (false), the default is t
(defun add-word (literal &key POS anim art)
  (cond ((null (gethash literal *vocab*))
         (setf (gethash literal *vocab*) (make-word-prop :literal literal
                                                         :POS (or POS 'n)
                                                         :anim (or anim 'f)
                                                         :art (or art t)))
         (nconc *literal-list* (list literal)))
        (t nil)))

;; Debug funtions 
(defun print-vocab ()
  (maphash (lambda (key val) 
             (word-prop-literal val)
             (format t "~A: " key)
             (mapcar #'(lambda(y)
                         (format t "~A " (word-prop-literal y)))
                     (word-prop-relation val))
             (format t ": ")
             (mapcar #'(lambda(y)
                         (format t "~A " (word-prop-literal y)))
                     (word-prop-homophone val))
             (terpri))
           *vocab*))

(defun test ()
  (add-relation 'traffic 'jam)
  (print-vocab))




