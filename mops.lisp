(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)

(run-tests has-slots-p)

(defun has-slots-p (mops slots)
  )

