

(in-package #:nefarious)


(defparameter *handles* (make-hash-table)
  "Maps NFS handles to pathnames.")

(defconstant +nefarious-handle-size+ 32)

(defun make-handle (pathname)
  (let ((h (nibbles:make-octet-vector +nefarious-handle-size+)))
    (setf (nibbles:ub64ref/be h 0) 
	  (sxhash pathname))
    h))

