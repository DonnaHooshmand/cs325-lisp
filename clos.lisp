; CLOS (Common Lisp Object System) is a set of operators for
; doing OOP. 
; Technically they are in no way distinguished from the rest of CL:
; defmethod is just as much and just as little an integral part of 
; the language as defun

(defclass rectangle ()
  (height width))

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)
   (center :accessor circle-center
           :initarg :center
           :initform (cons 0 0))))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(defparameter r (make-instance 'rectangle))

(defun make-rectangle (height width)
  (let ((r (make-instance 'rectangle)))
        (setf (slot-value r 'height) height)
        (setf (slot-value r 'width) width)
        r))

(area (make-rectangle 10 12))
