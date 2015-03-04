

(in-package #:nefarious)

(define-condition nfs-error (error)
  ((stat :initarg :stat :reader nfs-error-stat)
   (format-control :initform "NFS-ERROR: ~A" :initarg :format-control :reader nfs-error-format-control)
   (format-args :initform nil :initarg :format-args :reader nfs-error-format-args))
  (:report (lambda (condition stream)
	     (apply #'format stream 
		    (nfs-error-format-control condition)
		    (nfs-error-stat condition)
		    (nfs-error-format-args condition)))))
